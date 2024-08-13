create_process_modal <- function(ns = session$ns,
                                 title = "In progress. Please wait.",
                                 inc_middle = TRUE,
                                 inc_bottom = TRUE,
                                 inc_graphic = FALSE,
                                 easy_close = FALSE,
                                 size = "l") {
  prograssbars_list <- shiny::tagList(
    shinyWidgets::progressBar(
      id = ns("pgr_top"),
      value = 0,
      display_pct = TRUE,
      title = "Overall"
    )
  )
  if (inc_middle == TRUE) {
    prograssbars_list[length(prograssbars_list) + 1] <- list(
      shinyWidgets::progressBar(
        id = ns("pgr_middle"),
        value = 0,
        display_pct = TRUE,
        title = "Batches"
      )
    )
  }
  if (inc_bottom == TRUE) {
    prograssbars_list[length(prograssbars_list) + 1] <- list(
      shinyWidgets::progressBar(
        id = ns("pgr_bottom"),
        value = 0,
        display_pct = TRUE,
        title = "Steps"
      )
    )
  }

  if (inc_graphic == TRUE) {
    prograssbars_list[length(prograssbars_list) + 1] <- list(
      shiny::tags$hr()
    )
    prograssbars_list[length(prograssbars_list) + 1] <- list(
      shiny::tags$p("Loss Development")
    )
    prograssbars_list[length(prograssbars_list) + 1] <- list(
      shiny::plotOutput(
        outputId = ns("pgr_plot")
      )
    )
  }

  prograssbars_list[length(prograssbars_list) + 1] <- list(
    shiny::tags$hr()
  )
  prograssbars_list[length(prograssbars_list) + 1] <- list(
    tags$p("Error messages:")
  )
  prograssbars_list[length(prograssbars_list) + 1] <- list(
    textOutput(outputId = ns("error_messages"))
  )

  modal <- shiny::modalDialog(
    title = title,
    easyClose = easy_close,
    size = size,
    prograssbars_list,
    footer = shiny::actionButton(
      inputId = ns("pgr_cancel"),
      label = "Cancel/Close App",
      con = shiny::icon("ban")
    )
  )
  return(modal)
}




start_and_monitor_long_task <- function(id,
                                        ExtendedTask_type,
                                        ExtendedTask_arguments,
                                        log_path = NULL,
                                        pgr_use_middle = FALSE,
                                        pgr_use_bottom = FALSE,
                                        pgr_use_graphic = FALSE,
                                        update_intervall = 2,
                                        success_type = "data_sets") {
  moduleServer(id, function(input, output, session) {
    #--------------------------------------------------------------------------

    print("log_path")
    print(log_path)
    print("Argument")
    print(ExtendedTask_type)
    print(ExtendedTask_arguments)

    # Reset log
    reset_log(log_path = log_path)
    if (ExtendedTask_type %in% c("classifier")) {
      reset_loss_log(
        log_path = paste0(dirname(log_path), "/aifeducation_loss.log"),
        epochs = ExtendedTask_arguments$epochs
      )
    }

    # Create progress modal
    progress_modal <- create_process_modal(
      ns = session$ns,
      inc_middle = pgr_use_middle,
      inc_bottom = pgr_use_bottom,
      inc_graphic = pgr_use_graphic,
      easy_close = FALSE,
      size = "l"
    )

    # Show modal
    shiny::showModal(progress_modal)
    args <- ExtendedTask_arguments
    save(args,
      file = paste0(getwd(), "/arguments.rda")
    )

    # Start ExtendedTask
    if (ExtendedTask_type == "raw_texts") {
      CurrentTask <- shiny::ExtendedTask$new(long_add_texts_to_dataset)
      do.call(what = CurrentTask$invoke, args = ExtendedTask_arguments)
    } else if (ExtendedTask_type == "embed_raw_text") {
      CurrentTask <- shiny::ExtendedTask$new(long_transform_text_to_embeddings)
      do.call(what = CurrentTask$invoke, args = ExtendedTask_arguments)
    } else if (ExtendedTask_type == "classifier") {
      CurrentTask <- shiny::ExtendedTask$new(long_classifier)
      do.call(what = CurrentTask$invoke, args = ExtendedTask_arguments)
    } else if (ExtendedTask_type=="feature_extractor"){
      CurrentTask <- shiny::ExtendedTask$new(long_feature_extractor)
      do.call(what = CurrentTask$invoke, args = ExtendedTask_arguments)
    }

    # Check progress of the task
    progress_bar_status <- reactive({
      # Do periodical checks only if the task is actual running
      if (CurrentTask$status() == "running") {
        shiny::invalidateLater(millis = update_intervall*1000)

        if (!is.null(log_path)) {
          log <- read_log(log_path)
        } else {
          log <- NULL
        }

        if (is.null(log)) {
          top <- NULL
          middle <- NULL
          bottom <- NULL
        } else {
          if (is.na(log[1, 3]) | log[1, 3] == "NA") {
            top <- NULL
          } else {
            top <- log[1, ]
          }

          if (is.na(log[2, 3]) | log[2, 3] == "NA") {
            middle <- NULL
          } else {
            middle <- log[2, ]
          }

          if (is.na(log[3, 3]) | log[3, 3] == "NA") {
            bottom <- NULL
          } else {
            bottom <- log[3, ]
          }
        }

        if (pgr_use_graphic == TRUE) {
          path_loss <- paste0(dirname(log_path), "/aifeducation_loss.log")
          loss_data <- read_loss_log(path_loss)
        } else {
          loss_data <- NULL
        }

        log_list <- list(
          top = top,
          middle = middle,
          bottom = bottom,
          loss_data = loss_data
        )
        return(log_list)
      }
    })

    output$pgr_plot <- shiny::renderPlot(
      {
        plot_data <- progress_bar_status()$loss_data
        if (!is.null(plot_data)) {
          if (ncol(plot_data) == 4) {
            data_columns <- c("train", "validation", "test")
          } else {
            data_columns <- c("epoch", "validation")
          }
          y_max <- max(plot_data[data_columns])
          plot <- ggplot2::ggplot(data = plot_data) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train, color = "train")) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation, color = "validation"))
          if (ncol(plot_data) == 4) {
            plot <- plot + ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test, color = "test"))
          }
          plot <- plot +
            ggplot2::theme_classic() +
            ggplot2::ylab("loss") +
            ggplot2::coord_cartesian(ylim = c(0, y_max)) +
            ggplot2::xlab("epoch") +
            ggplot2::scale_color_manual(values = c(
              "train" = "red",
              "validation" = "blue",
              "test" = "darkgreen"
            )) +
            ggplot2::theme(
              text = ggplot2::element_text(size = 12),
              legend.position = "bottom"
            )

          return(plot)
        }
      },
      res = 2 * 72
    )


    # Display progress on the progress modal
    observe({
      if (!is.null(progress_bar_status()$top)) {
        shinyWidgets::updateProgressBar(
          id = "pgr_top",
          value = as.numeric(progress_bar_status()$top$value),
          total = as.numeric(progress_bar_status()$top$total),
          title = progress_bar_status()$top$message
        )
      }

      if (!is.null(progress_bar_status()$middle)) {
        shinyWidgets::updateProgressBar(
          id = "pgr_middle",
          value = as.numeric(progress_bar_status()$middle$value),
          total = as.numeric(progress_bar_status()$middle$total),
          title = progress_bar_status()$middle$message
        )
      }

      if (!is.null(progress_bar_status()$bottom)) {
        shinyWidgets::updateProgressBar(
          id = "pgr_bottom",
          value = as.numeric(progress_bar_status()$bottom$value),
          total = as.numeric(progress_bar_status()$bottom$total),
          title = progress_bar_status()$bottom$message
        )
      }
    })

    # Show message if the progress finishes
    observeEvent(CurrentTask$status(), {
      if (CurrentTask$status() == "success") {
        # Remove process modal
        shiny::removeModal()

        if (success_type == "data_sets") {
          success_message <- paste(
            "Created data set with",
            CurrentTask$result(),
            "documents."
          )
        } else {
          success_message <- ""
        }

        # Show success
        shinyWidgets::show_alert(
          title = "Success",
          type = "success",
          text = success_message
        )
      }
    })

    # Error display--------------------------------------------------------------
    output$error_messages <- renderText({
      if (CurrentTask$status() == "error") {
        return(CurrentTask$result())
      } else {
        return(NULL)
      }
    })

    # Cancel process------------------------------------------------------------
    shiny::observeEvent(input$pgr_cancel, {
      shiny::stopApp()
    })
    #--------------------------------------------------------------------------
  })
}
