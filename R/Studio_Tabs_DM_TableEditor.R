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
#' @family studio_gui_page_data_management_table_editor
#' @keywords internal
#' @noRd
#'
DataManagement_TableEditorUI <- function(id) {
  # Sidebar------------------------------------------------------------------
  bslib::page(
    bslib::layout_column_wrap(
      bslib::card(
        bslib::card_body(
          shiny::actionButton(
            inputId = shiny::NS(id, "button_new_table"),
            label = "New Table",
            icon = shiny::icon("plus")
          ),
          shinyFiles::shinyFilesButton(
            id = shiny::NS(id, "button_select_file"),
            label = "Choose File",
            title = "Please choose a file",
            icon = shiny::icon("folder-open"),
            multiple = FALSE
          ),
          shinyFiles::shinyDirButton(
            id = shiny::NS(id, "start_SaveModal"),
            label = "Save As",
            title = "Choose Destination",
            icon = shiny::icon("floppy-disk")
          )
        )
      ),
      bslib::card(
        bslib::card_body(
          shiny::actionButton(
            inputId = shiny::NS(id, "add_row"),
            label = "Add Row",
            icon = shiny::icon("plus")
          ),
          shiny::numericInput(
            inputId = shiny::NS(id, "remove_row_number"),
            label = "Remove Row Number",
            value = 1,
            min = 1
          ),
          shiny::actionButton(
            inputId = shiny::NS(id, "remove_row"),
            label = "Remove Row",
            icon = shiny::icon("minus")
          ),
        )
      ),
      bslib::card(
        bslib::card_body(
          shiny::actionButton(
            inputId = shiny::NS(id, "add_col"),
            label = "Add column",
            icon = shiny::icon("plus")
          ),
          shiny::textInput(
            inputId = shiny::NS(id, "remove_col_name"),
            label = "Remove Column Name",
          ),
          shiny::actionButton(
            inputId = shiny::NS(id, "remove_col"),
            label = "Remove Column",
            icon = shiny::icon("minus")
          )
        )
      ),
      bslib::card(
        bslib::card_body(
          bslib::layout_column_wrap(
            shiny::textInput(
              inputId = shiny::NS(id, "rename_col_old"),
              label = "Column Name Old",
            ),
            shiny::textInput(
              inputId = shiny::NS(id, "rename_col_new"),
              label = "Column Name New",
            ),
            shiny::actionButton(
              inputId = shiny::NS(id, "rename_col"),
              label = "Rename Column",
              icon = shiny::icon("arrows-left-right")
            )
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header(
        shiny::uiOutput(
          outputId = shiny::NS(id, "card_header_path")
        )
      ),
      bslib::card_body(
        DT::dataTableOutput(
          outputId = shiny::NS(id, "card_body_table_editor")
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
#' @family studio_gui_page_data_management_table_editor
#' @keywords internal
#' @noRd
#'
DataManagement_TableEditorServer <- function(id, log_dir, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    # File system management----------------------------------------------------
    # Embeddings
    shinyFiles::shinyFileChoose(
      input = input,
      id = "button_select_file",
      roots = volumes,
      filetypes = c("csv", "rda", "rdata")
    )

    shiny::observeEvent(input$button_select_file,
      {
        tmp_file_path <- shinyFiles::parseFilePaths(volumes, input$button_select_file)
        if (!is.null(tmp_file_path)) {
          if (nrow(tmp_file_path) > 0) {
            tmp_path <- tmp_file_path[[1, "datapath"]]
          } else {
            tmp_path <- NULL
          }
          file_path(tmp_path)
        } else {
          tmp_path <- NULL
          file_path(tmp_path)
        }

        if (!is.null(tmp_path)) {
          file_extension <- get_file_extension(file_path())
          if (file_extension == "csv") {
            table <- read.csv(file = file_path())
          } else if (file_extension == "rda" | file_extension == "rdata") {
            table <- load(file = file_path())
            table <- get(table)
          }
          table <- as.data.frame(table)
          if (!is.null(rownames(table))) {
            table$id <- rownames(table)
          }
          reload_table(TRUE)
          current_table(table)
        }
      },
      ignoreNULL = FALSE
    )
    # Reactive Value------------------------------------------------------------
    current_table <- shiny::reactiveVal()
    file_path <- shiny::reactiveVal()
    reload_table <- shiny::reactiveVal(value = TRUE)

    # Routines------------------------------------------------------------------
    shiny::observeEvent(input$button_new_table, {
      table <- as.data.frame(matrix(
        data = NA,
        nrow = 5,
        ncol = 3,
        dimnames = list(
          NULL,
          c("id", "column_1", "column_2")
        )
      ))

      current_table(table)
      file_path(NULL)
    })

    # Card header
    output$card_header_path <- shiny::renderUI({
      return(file_path())
    })

    shiny::observe({
      if (reload_table() == TRUE) {
        output$card_body_table_editor <- DT::renderDataTable({
          return(
            DT::datatable(
              data = current_table(),
              editable = "cell",
              selection = "none",
              rownames = FALSE
            )
          )
        })
      }
    })

    # Edit Cell-----------------------------------------------------------------
    shiny::observeEvent(input$card_body_table_editor_cell_edit, {
      tmp_data <- current_table()
      tmp_row <- input$card_body_table_editor_cell_edit[1, 1]
      tmp_col <- input$card_body_table_editor_cell_edit[1, 2] + 1
      tmp_value <- input$card_body_table_editor_cell_edit[1, 3]
      tmp_data[tmp_row, tmp_col] <- tmp_value
      reload_table(FALSE)
      current_table(tmp_data)
    })

    # Add and remove rows-------------------------------------------------------
    shiny::observeEvent(input$add_row, {
      tmp_data <- current_table()
      shiny::req(tmp_data)
      new_row <- rep(x = NA, times = ncol(tmp_data))
      names(new_row) <- colnames(tmp_data)
      tmp_data <- rbind(new_row, tmp_data)
      reload_table(TRUE)
      current_table(tmp_data)
    })
    shiny::observeEvent(input$remove_row, {
      tmp_data <- current_table()
      shiny::req(tmp_data)
      if (input$remove_row_number <= nrow(tmp_data) & input$remove_row_number > 0) {
        tmp_data <- tmp_data[-input$remove_row_number, ]
        reload_table(TRUE)
        current_table(tmp_data)
      }
    })

    # Add and remove col-------------------------------------------------------
    shiny::observeEvent(input$add_col, {
      tmp_data <- current_table()
      shiny::req(tmp_data)
      tmp_data[[paste0("new_col_", ncol(tmp_data))]] <- NA
      reload_table(TRUE)
      current_table(tmp_data)
    })
    shiny::observeEvent(input$remove_col, {
      tmp_data <- current_table()
      shiny::req(tmp_data)
      index <- which(colnames(tmp_data) == input$remove_col_name)
      if (input$remove_col_name %in% colnames(tmp_data)) {
        tmp_data <- tmp_data[, -index]
        reload_table(TRUE)
        current_table(tmp_data)
      }
    })

    # Rename columns------------------------------------------------------------
    shiny::observeEvent(input$rename_col, {
      tmp_data <- current_table()
      shiny::req(tmp_data)
      if (input$rename_col_old %in% colnames(tmp_data) & !input$rename_col_new %in% colnames(tmp_data)) {
        colnames(tmp_data)[which(input$rename_col_old == colnames(tmp_data))] <- input$rename_col_new
        current_table(tmp_data)
        reload_table(TRUE)
      }
    })


    # Saving Routine-------------------------------------------------------------
    # Start screen for choosing the location for storing the data set
    # Create Save Modal
    save_modal <- create_save_modal(
      id = id,
      title = "Choose Destination",
      easy_close = FALSE,
      size = "l"
    )

    # Implement file connection
    shinyFiles::shinyDirChoose(
      input = input,
      id = "start_SaveModal",
      roots = volumes,
      allowDirCreate = TRUE
    )

    # show save_modal
    shiny::observeEvent(input$start_SaveModal, {
      path <- shinyFiles::parseDirPath(volumes, input$start_SaveModal)
      if (!is.null(path) & !identical(path, character(0))) {
        if (path != "") {
          shiny::showModal(save_modal)
          shiny::updateTextInput(
            inputId = "save_modal_directory_path",
            value = path
          )
        }
      }
    })

    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_model_destination_dir",
      roots = volumes,
      # session = session,
      allowDirCreate = TRUE
    )

    shiny::observeEvent(input$button_model_destination_dir, {
      shiny::updateTextInput(
        inputId = ns("destination_dir"),
        value = shinyFiles::parseDirPath(volumes, input$button_model_destination_dir)
      )
    })

    shiny::observeEvent(input$save_modal_button_continue, {
      #Remove Save Modal
      shiny::removeModal()

      write.csv(
        x = current_table(),
        file = paste0(input$save_modal_directory_path, "/", input$save_modal_folder_name, ".csv"),
        row.names = FALSE
      )
      shiny::removeModal()
    })



    #--------------------------------------------------------------------------
  })
}
