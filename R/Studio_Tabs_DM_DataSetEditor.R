# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Graphical user interface for the editor/explorer of files.
#' @description Functions generates the tab within a page for creating an editor/explorer.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_data_management_data_set_editor
#' @keywords internal
#'
DataManagement_DataSetEditorUI <- function(id) {
  bslib::page_sidebar(
    # Sidebar------------------------------------------------------------------
    sidebar = bslib::sidebar(
      position = "left",
      shinyFiles::shinyDirButton(
        id = shiny::NS(id, "button_select_folder"),
        label = "Choose Folder",
        title = "Please choose a folder",
        icon = shiny::icon("folder-open")
      ),
      shiny::uiOutput(
        outputId = shiny::NS(id, "raw_text_control")
      )
    ),
    bslib::card(
      bslib::card_header(
        shiny::uiOutput(
          outputId = shiny::NS(id, "card_header_path")
        )
      ),
      bslib::card_body(
        shiny::uiOutput(
          outputId = shiny::NS(id, "card_body_data_set_editor")
        )
      )
    )
  )
}


#' @title Server function for: graphical user interface for making predictions with a classifier.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param log_dir `string` Path to the directory where the log files should be stored.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_data_management_data_set_editor
#' @keywords internal
#'
DataManagement_DataSetEditorServer <- function(id, log_dir, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    # File system management----------------------------------------------------
    # Embeddings
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_folder",
      roots = volumes,
      allowDirCreate = FALSE
    )

    folder_path <- shiny::eventReactive(input$button_select_folder,
      {
        path <- shinyFiles::parseDirPath(volumes, input$button_select_folder)
        if (!identical(path, character(0))) {
          return(path)
        } else {
          return(NULL)
        }
      },
      ignoreNULL = FALSE
    )
    # Main ui components--------------------------------------------------------
    # Card header
    output$card_header_path <- shiny::renderUI({
      return(folder_path())
    })
    output$card_body_data_set_editor <- shiny::renderUI({
      if (!is.null(loaded_object())) {
        if ("LargeDataSetForText" %in% class(loaded_object())) {
          ui <- shiny::tagList(
            shiny::uiOutput(
              outputId = ns("editor_content_meta_data")
            ),
            shiny::textOutput(
              outputId = ns("editor_content")
            )
          )
          return(ui)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })
    output$editor_content_meta_data <- shiny::renderUI({
      if (is.null_or_na(current_document()$url_license)) {
        license_ui <- shiny::tags$p("License: ", current_document()$license)
      } else {
        license_ui <- shiny::tags$p(
          "License: ",
          shiny::tags$a(
            current_document()$license,
            href = current_document()$url_license,
            target = "_blank"
          )
        )
      }

      ui <- shiny::tagList(
        bslib::layout_column_wrap(
          shiny::tags$p(
            "ID: ", current_document()$id
          ),
          shiny::tags$p(
            "Bibliographic Entry: ", current_document()$bib_entry
          ),
          license_ui,
          shiny::tags$p(
            "Source URL: ", current_document()$url_license
          )
        )
      )

      return(ui)
    })

    output$editor_content <- shiny::renderText({
      page <- max(1, min(input$page_selected, max_pages()))
      text_to_display <- substring(
        text = current_document()$text,
        first = (page - 1) * 1800,
        last = page * 1800
      )
      return(text_to_display)
    })

    # Reactive Values-----------------------------------------------------------
    # Load Data Sets
    loaded_object <- shiny::reactive({
      if (!is.null(folder_path())) {
        object <- load_from_disk(dir_path = folder_path())
        if ("LargeDataSetForText" %in% class(object)) {
          return(object)
        } else {
          display_errors(
            error_messages = "Folder does not contain a LargeDataSetForText"
          )
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })
    n_documents <- shiny::reactive({
      if (!is.null(loaded_object())) {
        n_documents <- loaded_object()$n_rows()
        shiny::updateNumericInput(
          inputId = "document_selected",
          label = paste("Max", n_documents, "Documents"),
          max = n_documents
        )
        return(n_documents)
      } else {
        return(NULL)
      }
    })
    current_document <- shiny::reactive({
      if (!is.null(loaded_object())) {
        document <- loaded_object()$get_dataset()
        # Python is zero based
        document <- document[[(input$document_selected - 1)]]
        return(document)
      } else {
        return(NULL)
      }
    })
    max_pages <- shiny::reactive({
      if (!is.null(current_document())) {
        max_pages <- ceiling(nchar(current_document()$text) / 1800)
        shiny::updateNumericInput(
          inputId = "page_selected",
          label = paste("Max", max_pages, "Pages"),
          max = n_documents
        )
        return(max_pages)
      } else {
        return(NULL)
      }
    })

    # Ui Elements editor--------------------------------------------------------
    output$raw_text_control <- shiny::renderUI({
      if (!is.null(loaded_object())) {
        if ("LargeDataSetForText" %in% class(loaded_object())) {
          ui <- shiny::tagList(
            bslib::card(
              bslib::card_header("Documents"),
              bslib::card_body(
                shiny::actionButton(
                  inputId = ns("document_plus"),
                  label = "+"
                ),
                shiny::numericInput(
                  inputId = ns("document_selected"),
                  value = 1,
                  min = 1,
                  step = 1,
                  label = "Selected Document"
                ),
                shiny::actionButton(
                  inputId = ns("document_minus"),
                  label = "-"
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Pages"
              ),
              bslib::card_body(
                shiny::actionButton(
                  inputId = ns("page_plus"),
                  label = "+"
                ),
                shiny::numericInput(
                  inputId = ns("page_selected"),
                  value = 1,
                  min = 1,
                  step = 1,
                  label = "Selected Page"
                ),
                shiny::actionButton(
                  inputId = ns("page_minus"),
                  label = "-"
                )
              )
            )
          )
          return(ui)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    ## Ui Elements editor Selection of documents---------------------------------
    shiny::observeEvent(input$document_plus, {
      shiny::updateNumericInput(
        inputId = "document_selected",
        value = min(input$document_selected + 1, n_documents())
      )
    })
    shiny::observeEvent(input$document_minus, {
      shiny::updateNumericInput(
        inputId = "document_selected",
        value = max(1, input$document_selected - 1)
      )
    })
    # Selection of pages--------------------------------------------------------
    shiny::observeEvent(input$page_plus, {
      shiny::updateNumericInput(
        inputId = "page_selected",
        value = min(input$page_selected + 1, max_pages())
      )
    })
    shiny::observeEvent(input$page_minus, {
      shiny::updateNumericInput(
        inputId = "page_selected",
        value = max(1, input$page_selected - 1)
      )
    })




    #--------------------------------------------------------------------------
  })
}
