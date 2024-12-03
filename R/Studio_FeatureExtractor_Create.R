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

#' @title Graphical user interface for feature extractors - create
#' @description Functions generates the page for a creating a new [TEFeatureExtractor].
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_feature_extractor_create
#' @keywords internal
#' @noRd
#'
FeatureExtractors_Create_UI <- function(id) {
  shiny::tagList(
    bslib::page_sidebar(
      # Sidebar------------------------------------------------------------------
      sidebar = bslib::sidebar(
        position = "left",
        shiny::tags$h3("Control Panel"),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "button_select_dataset_for_embeddings"),
          label = "Choose Embeddings",
          title = "Please choose a folder",
          icon = shiny::icon("folder-open")
        ),
        shiny::tags$hr(),
        shiny::textInput(
          inputId = shiny::NS(id, "name"),
          label = "Model Name",
          width = "100%"
        ),
        shiny::textInput(
          inputId = shiny::NS(id, "label"),
          label = "Model Label",
          width = "100%"
        ),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "start_SaveModal"),
          label = "Train Model",
          title = "Choose Destination",
          icon = shiny::icon("floppy-disk")
        )
      ),
      # Main Page---------------------------------------------------------------
      # Content depends in the TextEmbeddingModel of the embeddings
      # Embeddings
      bslib::layout_column_wrap(
        bslib::card(
          bslib::card_header("Input Data"),
          bslib::card_body(
            shiny::textInput(
              inputId = shiny::NS(id, "embeddings_dir"),
              label = shiny::tags$p(shiny::icon("folder"), "Path")
            ),
            shiny::uiOutput(outputId = shiny::NS(id, "summary_data_embeddings"))
          )
        ),
        bslib::card(
          bslib::card_header(
            "Architecture"
          ),
          bslib::card_body(
            shiny::sliderInput(
              inputId = shiny::NS(id, "features"),
              label = "Target Features",
              min = 1,
              value = 128,
              max = 1024,
              step = 1,
              round = TRUE
            ),
            shiny::selectInput(
              inputId = shiny::NS(id, "method"),
              label = "Method",
              choices = c("conv", "lstm", "dense")
            ),
            shiny::sliderInput(
              inputId = shiny::NS(id, "noise_factor"),
              label = "Noise Factor",
              min = 0,
              value = 0,
              max = 1,
              step = .01,
              round = TRUE
            ),
            shiny::selectInput(
              inputId = shiny::NS(id, "optimizer"),
              label = "Optimizer",
              choices = c("adam", "rmsprop")
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            "Training Settings"
          ),
          bslib::card_body(
            shiny::selectInput(
              inputId = shiny::NS(id, "sustainability_country"),
              label = "Country for Sustainability Tracking",
              choices = country_alpha_3_list,
              # choices=NULL,
              selected = "DEU"
            ),
            shiny::sliderInput(
              inputId = shiny::NS(id, "data_val_size"),
              label = "Proportion for Validation Sample",
              min = 0.02,
              value = 0.25,
              max = 0.5,
              step = 0.01
            ),
            shiny::numericInput(
              inputId = shiny::NS(id, "epochs"),
              label = "Epochs",
              min = 1,
              value = 40,
              step = 1
            ),
            shiny::sliderInput(
              inputId = shiny::NS(id, "batch_size"),
              label = "Batch Size",
              min = 2,
              max = 256,
              value = 32,
              step = 1
            )
          )
        )
      )
    )
  )
}


#' @title Server function for: graphical user interface for feature extractors - create
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param log_dir `string` Path to the directory where the log files should be stored.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_feature_extractor_create
#' @keywords internal
#' @noRd
#'
FeatureExtractor_Create_Server <- function(id, log_dir, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    # ns <- session$ns
    log_path <- paste0(log_dir, "/aifeducation_state.log")

    # File system management----------------------------------------------------
    # Embeddings
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_dataset_for_embeddings",
      roots = volumes,
      # session = session,
      allowDirCreate = FALSE
    )
    shiny::observeEvent(input$button_select_dataset_for_embeddings, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_dataset_for_embeddings)
      shiny::updateTextInput(
        inputId = "embeddings_dir",
        value = path
      )
    })

    path_to_embeddings <- shiny::eventReactive(input$embeddings_dir, {
      if (input$embeddings_dir != "") {
        return(input$embeddings_dir)
      } else {
        return(NULL)
      }
    })

    data_embeddings <- shiny::reactive({
      if (!is.null(path_to_embeddings())) {
        return(load_and_check_embeddings(path_to_embeddings()))
      } else {
        return(NULL)
      }
    })

    # Start screen for choosing the location for storing the data set-----------
    # Create Save Modal
    save_modal <- create_save_modal(
      id = id,
      # ns=session$ns,
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

    # Start training------------------------------------------------------------

    shiny::observeEvent(input$save_modal_button_continue, {
      # Check for errors
      errors <- check_errors_create_feature_extractor(
        destination_path = input$save_modal_directory_path,
        folder_name = input$save_modal_folder_name,
        path_to_embeddings = path_to_embeddings(),
        features = input$features,
        model_name = input$name,
        model_label = input$label
      )

      # If there are errors display them. If not start running task.
      if (!is.null(errors)) {
        display_errors(
          title = "Error",
          size = "l",
          easy_close = TRUE,
          error_messages = errors
        )
      } else {
        # Start task and monitor
        start_and_monitor_long_task(
          id = id,
          ExtendedTask_type = "feature_extractor",
          ExtendedTask_arguments = list(
            name = input$name,
            label = input$label,
            destination_path = input$save_modal_directory_path,
            folder_name = input$save_modal_folder_name,
            path_to_embeddings = path_to_embeddings(),
            features = input$features,
            method = input$method,
            noise_factor = input$noise_factor,
            optimizer = input$optimizer,
            data_val_size = input$data_val_size,
            epochs = input$epochs,
            batch_size = input$batch_size,
            sustain_iso_code = input$sustainability_country,
            log_dir = log_dir,
            log_write_interval = 3
          ),
          log_path = log_path,
          pgr_use_middle = TRUE,
          pgr_use_bottom = TRUE,
          pgr_use_graphic = TRUE,
          update_intervall = 300,
          success_type = "classifier"
        )
      }
    })

    # Display Data Summary------------------------------------------------------
    # Embeddings
    output$summary_data_embeddings <- shiny::renderUI({
      embeddings <- data_embeddings()
      # shiny::req(embeddings)
      if (!is.null(embeddings)) {
        ui <- create_data_embeddings_description(embeddings)

        return(ui)
      } else {
        return(NULL)
      }
    })

    # Error handling-----------------------------------------------------------


    #--------------------------------------------------------------------------
  })
}
