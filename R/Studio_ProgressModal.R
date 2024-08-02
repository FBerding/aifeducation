create_process_modal <- function(ns = session$ns,
                                 title = "In progress. Please wait.",
                                 inc_middle = TRUE,
                                 inc_bottom = TRUE,
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

  modal <- shiny::modalDialog(
    title = title,
    easyClose = easy_close,
    size = size,
    prograssbars_list,
    tags$p("Error messages:"),
    textOutput(outputId = ns("error_messages")),
    #footer = "To stop the progress please close the browser."
    footer=shiny::actionButton(inputId = ns("pgr_cancel"),
                        label = "CancelClose App",
                        con = shiny::icon("ban"))

  )
  return(modal)
}




start_and_monitor_long_task <- function(id,
                              ExtendedTask_type,
                              ExtendedTask_arguments,
                              log_path = NULL,
                              pgr_use_middle = FALSE,
                              pgr_use_bottom = FALSE,
                              update_intervall = 2000,
                              success_type = "data_sets") {
  moduleServer(id, function(input, output, session) {
    #--------------------------------------------------------------------------

    # Reset log
    print(log_path)
    reset_log(log_path = log_path)

    # Create progress modal
    progress_modal <- create_process_modal(
      ns = session$ns,
      inc_middle = isTRUE(pgr_use_middle),
      inc_bottom = isTRUE(pgr_use_bottom),
      easy_close = FALSE,
      size = "l"
    )

    # Show modal
    shiny::showModal(progress_modal)
    print(ExtendedTask_arguments)

    #Start ExtendedTask
    if(ExtendedTask_type=="raw_texts"){
      CurrentTask <- shiny::ExtendedTask$new(long_add_texts_to_dataset)
      do.call(what=CurrentTask$invoke,args=ExtendedTask_arguments)
    } else if(ExtendedTask_type=="embed_raw_text") {
      CurrentTask <- shiny::ExtendedTask$new(long_transform_text_to_embeddings)
      do.call(what=CurrentTask$invoke,args=ExtendedTask_arguments)
    }

    # Check progress of the task
    progress_bar_status <- reactive({
      # Do periodical checks only if the task is actual running
      if (CurrentTask$status() == "running") {
        shiny::invalidateLater(millis = update_intervall)

        if (!is.null(log_path)) {
          log <- read_log(log_path)
        } else {
          log <- NULL
        }

        if(is.null(log)){
          top=NULL
          middle=NULL
          bottom=NULL
        } else {
          if(is.na(log[1,3])|log[1,3]=="NA"){
            top=NULL
          } else {
            top=log[1,]
          }

          if(is.na(log[2,3])|log[2,3]=="NA"){
            middle=NULL
          } else {
            middle=log[2,]
          }

          if(is.na(log[3,3])|log[3,3]=="NA"){
            bottom=NULL
          } else {
            bottom=log[3,]
          }
        }

        log_list <- list(
          top = top,
          middle = middle,
          bottom = bottom
        )

        return(log_list)
      }
    })

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

    #Error display--------------------------------------------------------------
    output$error_messages <- renderText({
      if (CurrentTask$status() == "error") {
        return(CurrentTask$result())
      } else {
        return(NULL)
      }
    })

    #Cancel process------------------------------------------------------------
      shiny::observeEvent(input$pgr_cancel,{
        shiny::stopApp()
      })
    #--------------------------------------------------------------------------
  })
}
