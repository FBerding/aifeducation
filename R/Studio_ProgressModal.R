create_process_modal <- function(ns = session$ns,
                                 title = "In progress. Please wait.",
                                 inc_middle = TRUE,
                                 inc_bottom = TRUE,
                                 easy_close = FALSE,
                                 size = "l") {
  prograssbars_list <- shiny::tagList(
    shinyWidgets::progressBar(
      # id = "pgr_top",
      id = ns("pgr_top"),
      value = 0,
      display_pct = TRUE,
      title = "Overall"
    )
  )
  if (inc_middle == TRUE) {
    prograssbars_list[length(prograssbars_list) + 1] <- list(
      shinyWidgets::progressBar(
        # id = "pgr_middle",
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
        # id = "pgr_bottom",
        id = ns("pgr_bottom"),
        value = 0,
        display_pct = TRUE,
        title = "Steps"
      )
    )
  }

  modal <- shiny::modalDialog(
    title = title,
    easyClose = easy_close,
    size = size,
    prograssbars_list,
    tags$p("Error messages:"),
    textOutput(outputId = ns("error_messages")),
    footer = "To stop the progress please close the browser."
  )
  return(modal)
}

read_log <- function(file_path) {
  if (!is.null_or_na(file_path)) {
    log_file <- try(read.csv(file_path))
    if (class(log_file) != "try-error") {
      return(log_file)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

reset_log <- function(log_path_top = NULL,
                      log_path_middle = NULL,
                      log_path_bottom = NULL) {
  data_point <- c(0, 100)
  names(data_point) <- c("value", "total")

  if (!is.null(log_path_top)) {
    try(write.csv(x = t(data_point), file = log_path_top, row.names = FALSE))
  }

  if (!is.null(log_path_middle)) {
    try(write.csv(x = t(data_point), file = log_path_middle, row.names = FALSE))
  }

  if (!is.null(log_path_bottom)) {
    try(write.csv(x = t(data_point), file = log_path_bottom, row.names = FALSE))
  }
}


start_and_monitor_long_task <- function(id,
                              ExtendedTask_type,
                              ExtendedTask_arguments,
                              log_path_top = NULL,
                              log_path_middle = NULL,
                              log_path_bottom = NULL,
                              update_intervall = 2000,
                              success_type = "raw_texts") {
  moduleServer(id, function(input, output, session) {
    #--------------------------------------------------------------------------

    # Reset log
    reset_log(
      log_path_top = log_path_top,
      log_path_middle = log_path_middle,
      log_path_bottom = log_path_bottom
    )

    # Create progress modal
    progress_modal <- create_process_modal(
      ns = session$ns,
      inc_middle = !is.null(log_path_middle),
      inc_bottom = !is.null(log_path_bottom),
      easy_close = FALSE,
      size = "l"
    )

    # Show modal
    shiny::showModal(progress_modal)

    #Start ExtendedTask
    if(ExtendedTask_type=="raw_texts"){
      print(ExtendedTask_arguments)
      CurrentTask <- shiny::ExtendedTask$new(long_add_texts_to_dataset)
      do.call(what=CurrentTask$invoke,args=ExtendedTask_arguments)
    }

    # Check progress of the task
    progress_bar_status <- reactive({
      # Do periodical checks only if the task is actual running
      if (CurrentTask$status() == "running") {
        shiny::invalidateLater(millis = update_intervall)

        if (!is.null(log_path_top)) {
          top <- read_log(log_path_top)
        } else {
          top <- NULL
        }

        if (!is.null(log_path_middle)) {
          middle <- read_log(log_path_middle)
        } else {
          middle <- NULL
        }

        if (!is.null(log_path_bottom)) {
          bottom <- read_log(log_path_bottom)
        } else {
          bottom <- NULL
        }

        log <- list(
          top = top,
          middle = middle,
          bottom = bottom
        )

        return(log)
      }
    })

    # Display progress on the progress modal
    observe({
      print(progress_bar_status())
      if (!is.null(progress_bar_status()$top)) {
        print("update_top")
        shinyWidgets::updateProgressBar(
          id = "pgr_top",
          value = progress_bar_status()$top$value,
          total = progress_bar_status()$top$total,
          title = "Overall"
        )
      }

      if (!is.null(progress_bar_status()$middle)) {
        print("update_middle")
        print(progress_bar_status()$middle$value)
        shinyWidgets::updateProgressBar(
          id = "pgr_middle",
          value = progress_bar_status()$middle$value,
          total = progress_bar_status()$middle$total,
          title = "Batches"
        )
      }

      if (!is.null(progress_bar_status()$bottom)) {
        print("update_top")
        shinyWidgets::updateProgressBar(
          id = "pgr_bottom",
          value = progress_bar_status()$bottom$value,
          total = progress_bar_status()$bottom$total,
          title = "Steps"
        )
      }
    })

    # Show message if the progress finishes
    observeEvent(CurrentTask$status(), {
      if (CurrentTask$status() == "success") {
        # Remove process modal
        shiny::removeModal()

        if (success_type == "raw_texts") {
          success_message <- paste(
            "Created text corpus with",
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

    output$error_messages <- renderText({
      if (CurrentTask$status() == "error") {
        return(CurrentTask$result())
      } else {
        return(NULL)
      }
    })
    #--------------------------------------------------------------------------
  })
}
