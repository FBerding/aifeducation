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

#' @title Graphical user interface for making predictions with a classifier.
#' @description Functions generates the tab within a page for making predictions with an object of class
#'   [TEClassifierRegular] and [TEClassifierProtoNet].
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_classifier_predict
#' @keywords internal
#' @noRd
#'
Classifier_Prediction_UI <- function(id) {
  bslib::page_sidebar(
    # Sidebar------------------------------------------------------------------
    sidebar = bslib::sidebar(
      position = "right",
      shinyFiles::shinyDirButton(
        id = shiny::NS(id, "button_select_dataset_for_embeddings"),
        label = "Choose Embeddings",
        title = "Please choose a folder",
        icon = shiny::icon("folder-open")
      ),
      shiny::tags$hr(),
      shiny::sliderInput(
        inputId = shiny::NS(id, "batch_size"),
        label = "Batch Size",
        min = 1,
        max = 256,
        value = 32,
        step = 1
      ),
      shiny::actionButton(
        inputId = shiny::NS(id, "start_predictions"),
        label = "Predict",
        icon = shiny::icon("paper-plane")
      ),
      shiny::tags$hr(),
      shinyFiles::shinyDirButton(
        id = shiny::NS(id, "start_SaveModal"),
        label = "Save Predictions",
        title = "Choose Destination",
        icon = shiny::icon("floppy-disk")
      )
    ),
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
      bslib::card_header("Predictions"),
      bslib::card_body(
        DT::DTOutput(outputId = shiny::NS(id, "table_predictions"))
      )
    ),
    shiny::uiOutput(
      outputId = shiny::NS(id, "proto_net_embeddings")
    )
  )
  # )
}

#' @title Server function for: graphical user interface for making predictions with a classifier.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model Model used for inference.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_classifier_predict
#' @keywords internal
#' @noRd
#'
Classifier_Prediction_Server <- function(id, model, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    # TODO (Yuliia): Remove? Variable is not used
    ns <- session$ns

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

    # Display Main pages---------------------------------------------------------
    # Description of embeddings
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

    # Predictions
    prediction_results <- shiny::eventReactive(input$start_predictions, {
      error_list <- check_errors_predict_classifier(
        model = model(),
        embeddings = data_embeddings()
      )
      if (length(error_list) > 0) {
        display_errors(
          title = "Error",
          size = "l",
          easy_close = TRUE,
          error_messages = error_list
        )
      } else {
        predictions <- model()$predict(
          newdata = data_embeddings(),
          batch_size = input$batch_size
        )

        # Generate embedding plot for TEProtoNetClassifier
        if ("TEClassifierProtoNet" %in% class(model())) {
          if (model()$model_config$embedding_dim == 2) {
            plot <- model()$plot_embeddings(
              embeddings_q = data_embeddings(),
              classes_q = NULL,
              batch_size = input$batch_size,
              alpha = 0.5,
              size_points = 3,
              size_points_prototypes = 8,
              inc_unlabeled = TRUE
            )
          } else {
            plot <- NULL
          }
        } else {
          plot <- NULL
        }

        return(
          list(
            predictions = predictions,
            plot = plot
          )
        )
      }
    })

    # Show prediction results
    output$table_predictions <- DT::renderDT({
      predictions <- prediction_results()$predictions
      if (is.null(predictions)) {
        return(NULL)
      } else {
        for (i in 1:(ncol(predictions) - 1)) {
          predictions[, i] <- round(predictions[, i], digits = 3)
        }
        return(predictions)
      }
    })

    # Save Predictions---------------------------------------------------------
    # Create Save Modal
    save_modal <- create_save_modal(
      # id = id,
      ns = session$ns,
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
          if (!is.null(prediction_results())) {
            shiny::showModal(save_modal)
            shiny::updateTextInput(
              inputId = "save_modal_directory_path",
              value = path
            )
          } else {
            display_errors(
              title = "Error",
              size = "l",
              easy_close = TRUE,
              error_messages = "There are no predictions."
            )
          }
        }
      }
    })

    # Save
    shiny::observeEvent(input$save_modal_button_continue, {
      #Remove Save Modal
      shiny::removeModal()

      write.csv(
        prediction_results()$predictions,
        file = paste0(
          input$save_modal_directory_path,
          "/",
          input$save_modal_folder_name,
          ".csv"
        )
      )
    })

    # Display Protonet embeddings
    output$proto_net_embeddings <- shiny::renderUI({
      model <- model()
      if ("TEClassifierProtoNet" %in% class(model)) {
        if (model$model_config$embedding_dim == 2) {
          ui <- shiny::tagList(
            bslib::card(
              bslib::card_header(
                "Embeddings"
              ),
              bslib::card_body(
                shiny::plotOutput(
                  outputId = ns("proto_net_embeddings_plot")
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

    output$proto_net_embeddings_plot <- shiny::renderPlot({
      return(prediction_results()$plot)
    })



    #--------------------------------------------------------------------------
  })
}
