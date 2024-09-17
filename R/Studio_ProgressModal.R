#' @title Create process modal for long running tasks
#' @description Function creates a shiny modal which is used to report the current status of all long running tasks.
#'
#' @param ns `function` for setting the namespace of the input and output elements.
#' @param title `string` Title of the modal.
#' @param inc_middle `bool` If `TRUE` includes the middle progress bar.
#' @param inc_bottom `bool` If `TRUE` includes the bottom progress bar.
#' @param inc_graphic `bool` If `TRUE` includes a graphic display the development of the loss.
#' @param easy_close `bool` If `TRUE`, the modal dialog can be dismissed by clicking outside the dialog box, or be
#'   pressing the Escape key. If `FALSE` the modal must be dismissed by clicking on a modalButton or from a call
#'   removeModal on the server.
#' @param size `string` Size of the modal. Possible are `"m"`, `"s"`, `"l"`, and `"xl"`.
#'
#' @return Returns a shiny modal.
#'
#' @family studio_long_tasks
#' @keywords internal
#'
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



#' @title Starts and monitors a long running task
#' @description This function is supposed to be used as a server function. It contains the set up of a long running
#'   tasks, the creation of a modal, and the reporting of the current status of the long running task.
#'
#' @param id `string` Namespace id for the input and output elements of the modal.
#' @param ExtendedTask_type `string` Type of the long running task.
#' @param ExtendedTask_arguments `string` Arguments to set up the long running task.
#' @param log_path `string` Path to the log file.
#' @param pgr_use_middle `bool` If `TRUE` includes the middle progress bar.
#' @param pgr_use_bottom `bool` If `TRUE` includes the bottom progress bar.
#' @param pgr_use_graphic `bool` If `TRUE` includes a graphic display the development of the loss.
#' @param update_intervall `double` Value greater 0 indicating in which interval the report should be updated. Values in
#'   seconds.
#' @param success_type `string` indicating which type of message should be displayed if the tasks successfully finishes.
#'
#' @return Return value depends in the `ExtendedTask_type` and `success_type`.
#'
#' @family studio_long_tasks
#' @keywords internal
#'
start_and_monitor_long_task <- function(id,
                                        ExtendedTask_type,
                                        ExtendedTask_arguments,
                                        log_path = NULL,
                                        pgr_use_middle = FALSE,
                                        pgr_use_bottom = FALSE,
                                        pgr_use_graphic = FALSE,
                                        update_intervall = 2,
                                        success_type = "data_sets") {
  shiny::moduleServer(id, function(input, output, session) {
    #--------------------------------------------------------------------------

    print("log_path")
    print(log_path)
    print("Argument")
    print(ExtendedTask_type)
    print(ExtendedTask_arguments)

    # Reset log
    reset_log(log_path = log_path)
    if (ExtendedTask_type %in% c("classifier", "feature_extractor")) {
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
    CurrentTask <- NULL
    if (ExtendedTask_type == "raw_texts") {
      CurrentTask <- shiny::ExtendedTask$new(long_add_texts_to_dataset)
    } else if (ExtendedTask_type == "embed_raw_text") {
      CurrentTask <- shiny::ExtendedTask$new(long_transform_text_to_embeddings)
    } else if (ExtendedTask_type == "classifier") {
      CurrentTask <- shiny::ExtendedTask$new(long_classifier)
    } else if (ExtendedTask_type == "feature_extractor") {
      CurrentTask <- shiny::ExtendedTask$new(long_feature_extractor)
    }
    if (!is.null(CurrentTask)) do.call(what = CurrentTask$invoke, args = ExtendedTask_arguments)

    # Check progress of the task
    progress_bar_status <- shiny::reactive({
      # Do periodical checks only if the task is actual running
      if (CurrentTask$status() == "running") {
        shiny::invalidateLater(millis = update_intervall * 1000)
        print(date())

        log <- NULL
        if (!is.null(log_path)) log <- read_log(log_path)

        top <- NULL
        middle <- NULL
        bottom <- NULL

        if (!is.null(log)) {
          if (!is.na(log[1, 3]) & log[1, 3] != "NA") top <- log[1, ]
          if (!is.na(log[2, 3]) & log[2, 3] != "NA") middle <- log[2, ]
          if (!is.na(log[3, 3]) & log[3, 3] != "NA") bottom <- log[3, ]
        }

        loss_data <- NULL
        if (pgr_use_graphic == TRUE) {
          path_loss <- paste0(dirname(log_path), "/aifeducation_loss.log")
          loss_data <- read_loss_log(path_loss)
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
            data_columns <- c("train", "validation")
          }

          y_max <- max(plot_data[data_columns])
          y_min <- 0
          plot <- ggplot2::ggplot(data = plot_data) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train, color = "train")) +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation, color = "validation"))
          if (ncol(plot_data) == 4) {
            plot <- plot + ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test, color = "test"))
          }
          plot <- plot +
            ggplot2::theme_classic() +
            ggplot2::ylab("loss") +
            ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
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
    shiny::observe({
      ids <- c("pgr_top", "pgr_middle", "pgr_bottom")
      prgbars_status<-progress_bar_status()
      prgbars <- list(prgbars_status$top, prgbars_status$middle, prgbars_status$bottom)

      for (i in 1:length(ids)) {
        if (!is.null(prgbars[[i]])) {
          shinyWidgets::updateProgressBar(
            id = ids[i],
            value = as.numeric(prgbars[[i]]$value),
            total = as.numeric(prgbars[[i]]$total),
            title = prgbars[[i]]$message
          )
        }
      }
    })

    # Show message if the progress finishes
    shiny::observeEvent(CurrentTask$status(), {
      if (CurrentTask$status() == "success") {
        # Remove process modal
        shiny::removeModal()
        Sys.sleep(1)

        success_message <- ""
        if (success_type == "data_sets") {
          success_message <- paste(
            "Created data set with",
            CurrentTask$result(),
            "documents."
          )
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
    output$error_messages <- shiny::renderText({
      res <- NULL
      if (CurrentTask$status() == "error") res <- CurrentTask$result()
      return(res)
    })

    # Cancel process------------------------------------------------------------
    shiny::observeEvent(input$pgr_cancel, {
      shiny::stopApp()
    })
    #--------------------------------------------------------------------------
  })
}
