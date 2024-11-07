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

#' @title Graphical user interface for displaying the training history of an object.
#' @description Functions generates the tab within a page for displaying the training history of an object.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_training
#' @keywords internal
#'
Training_UI <- function(id) {
  bslib::page(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        position = "right",
        shiny::sliderInput(
          inputId = shiny::NS(id, "text_size"),
          label = "Text Size",
          min = 1,
          max = 20,
          step = 0.5,
          value = 12
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "y_min"),
          label = "Y Min",
          value = NA
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "y_max"),
          label = "Y Max",
          value = NA
        ),
        shiny::uiOutput(
          outputId = shiny::NS(id, "classifier_specific")
        )
      ),
      shiny::plotOutput(
        outputId = shiny::NS(id, "training_plot")
      )
    )
  )
}

#' @title Server function for: graphical user interface for displaying the training history of an object.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model Model used for inference.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @importFrom rlang .data
#'
#' @family studio_gui_training
#' @keywords internal
#'
Training_Server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    # Control widgets for classifiers--------------------------------------------
    output$classifier_specific <- shiny::renderUI({
      if ("TEClassifierRegular" %in% class(model()) |
        "TEClassifierProtoNet" %in% class(model())) {
        ui <- shiny::tagList(
          shinyWidgets::radioGroupButtons(
            inputId = ns("training_phase"),
            label = "Training Phase",
            choices = list(
              "Folds" = FALSE,
              "Final" = TRUE
            )
          ),
          shiny::selectInput(
            inputId = ns("measure"),
            label = "Measures",
            choices = list(
              "Loss" = "loss",
              "Average Iota"="avg_iota",
              "Accuracy" = "accuracy",
              "Balanced Accuracy" = "balanced_accuracy"
            )
          ),
          shinyWidgets::materialSwitch(
            inputId = ns("training_min_max"),
            label = "Add Min/Max",
            value = TRUE,
            status = "primary"
          ),
          shiny::uiOutput(
            outputId = ns("widget_classifier_pl_step")
          )
        )
        return(ui)
      } else if ("TEFeatureExtractor" %in% class(model())) {
        ui <- shiny::tagList(
          shinyWidgets::radioGroupButtons(
            inputId = ns("measure"),
            label = "Measures",
            choices = list(
              "Loss" = "loss"
            )
          ),
          shinyWidgets::materialSwitch(
            inputId = ns("training_min_max"),
            label = "Add Min/Max",
            value = TRUE,
            status = "primary"
          )
        )
      } else {
        return(NULL)
      }
    })


    output$widget_classifier_pl_step <- shiny::renderUI({
      if ("TEClassifierRegular" %in% class(model()) |
        "TEClassifierProtoNet" %in% class(model())) {
        if (model()$last_training$config$use_pl == TRUE) {
          n_steps <- model()$last_training$config$pl_max_steps
          return(
            shinyWidgets::radioGroupButtons(
              inputId = ns("classifier_pl_step"),
              label = "Steps during Pseudo Labeling",
              choices = seq.int(from = 1, to = n_steps)
            )
          )
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    # plot------------------------------------------------
    output$training_plot <- shiny::renderPlot(
      {
        shiny::req(model)
        if ("TextEmbeddingModel" %in% class(model())) {
          # Plot for TextEmbeddingModels-----------------------------------------
          plot_data <- model()$last_training$history


          y_min <- input$y_min
          y_max <- input$y_max

          val_loss_min <- min(plot_data$val_loss)
          best_model_epoch <- which(x = (plot_data$val_loss) == val_loss_min)

          plot <- ggplot2::ggplot(data = plot_data) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$loss, color = "train")) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$val_loss, color = "validation")) +
            ggplot2::geom_vline(
              xintercept = best_model_epoch,
              linetype = "dashed"
            )

          plot <- plot + ggplot2::theme_classic() +
            ggplot2::ylab("value") +
            ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
            ggplot2::xlab("epoch") +
            ggplot2::scale_color_manual(values = c(
              "train" = "red",
              "validation" = "blue",
              "test" = "darkgreen"
            )) +
            ggplot2::theme(
              text = ggplot2::element_text(size = input$text_size),
              legend.position = "bottom"
            )

          # Plot for classifiers-----------------------------------------------
        } else if ("TEClassifierRegular" %in% class(model()) ||
          "TEClassifierProtoNet" %in% class(model()) ||
          "TEFeatureExtractor" %in% class(model())) {
          # Necessary input
          shiny::req(input$measure)

          # Get data for plotting
          if ("TEClassifierRegular" %in% class(model()) ||
            "TEClassifierProtoNet" %in% class(model())) {
            plot_data <- prepare_training_history(
              model = model(),
              final = input$training_phase,
              use_pl = model()$last_training$config$use_pl,
              pl_step = input$classifier_pl_step
            )
          } else if ("TEFeatureExtractor" %in% class(model())) {
            plot_data <- prepare_training_history(
              model = model(),
              final = FALSE,
              use_pl = FALSE,
              pl_step = NULL
            )
          }

          # Select the performance measure to display
          plot_data <- plot_data[[input$measure]]

          # Create Plot
          y_min <- input$y_min
          y_max <- input$y_max
          if (input$measure == "loss") {
            y_label <- "loss"
          } else if (input$measure == "accuracy") {
            y_label <- "Accuracy"
          } else if (input$measure == "balanced_accuracy") {
            y_label <- "Balanced Accuracy"
          } else if (input$measure == "avg_iota") {
            y_label <- "Average Iota"
          }


          # TODO (Yuliia): .data has no visible binding
          plot <- ggplot2::ggplot(data = plot_data) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train_mean, color = "train")) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation_mean, color = "validation"))

          if (input$training_min_max == TRUE) {
            plot <- plot +
              ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train_min, color = "train")) +
              ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train_max, color = "train")) +
              ggplot2::geom_ribbon(
                ggplot2::aes(
                  x = .data$epoch,
                  ymin = .data$train_min,
                  ymax = .data$train_max
                ),
                alpha = 0.25,
                fill = "red"
              ) +
              ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation_min, color = "validation")) +
              ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation_max, color = "validation")) +
              ggplot2::geom_ribbon(
                ggplot2::aes(
                  x = .data$epoch,
                  ymin = .data$validation_min,
                  ymax = .data$validation_max
                ),
                alpha = 0.25,
                fill = "blue"
              )
          }
          # TODO (Yuliia): .data has no visible binding
          if ("test_mean" %in% colnames(plot_data)) {
            plot <- plot +
              ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test_mean, color = "test"))
            if (input$training_min_max == TRUE) {
              plot <- plot +
                ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test_min, color = "test")) +

                ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test_max, color = "test")) +
                ggplot2::geom_ribbon(
                  ggplot2::aes(
                    x = .data$epoch,
                    ymin = .data$test_min,
                    ymax = .data$test_max
                  ),
                  alpha = 0.25,
                  fill = "darkgreen"
                )
            }
          }

          plot <- plot + ggplot2::theme_classic() +
            ggplot2::ylab(y_label) +
            ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
            ggplot2::xlab("epoch") +
            ggplot2::scale_color_manual(values = c(
              "train" = "red",
              "validation" = "blue",
              "test" = "darkgreen"
            )) +
            ggplot2::theme(
              text = ggplot2::element_text(size = input$text_size),
              legend.position = "bottom"
            )
        }
        return(plot)
      },
      res = 72 * 2
    )
  })
}
