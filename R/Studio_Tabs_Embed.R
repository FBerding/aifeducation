Embed_UI <- function(id) {
  bslib::page(
    bslib::card(
      bslib::card_header(
        "Raw Texts"
      ),
      bslib::card_body(
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "choose_file_raw_texts"),
          label = "Choose File",
          title = "Please choose a file",
          icon = shiny::icon("file"),
          multiple = FALSE
        ),
        shiny::textInput(
          inputId = shiny::NS(id, "file_path_raw_texts"),
          label = shiny::tags$p(shiny::icon("file"), "File Path")
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "batch_size"),
          label = "Batch Size",
          min = 1,
          max = 512,
          value = 8
        ),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "start_SaveModal"),
          label = "Save Embeddings",
          title = "Choose Destination",
          icon = shiny::icon("floppy-disk")
        )
      )
    )
  )
}



Embed_Server <- function(id, model, model_path, log_dir, volumes) {
  moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns
    log_path <- paste0(log_dir, "/aifeducation_state.log")

    # file system---------------------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "choose_file_raw_texts",
      roots = volumes,
      allowDirCreate = FALSE
    )

    shiny::observeEvent(input$choose_file_raw_texts, {

      tmp_file_path <- shinyFiles::parseDirPath(volumes, input$choose_file_raw_texts)
      if (length(tmp_file_path) > 0) {
        shiny::updateTextInput(
          inputId = "file_path_raw_texts",
          value = tmp_file_path
        )
      }
    })


    # Start screen for choosing the location for storing the data set-----------
    # Create Save Modal
    save_modal <- create_save_modal(
      id = id,
      ns=session$ns,
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
    observeEvent(input$start_SaveModal, {
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

    # Calculate Embedding------------------------------------------------------
    shiny::observeEvent(input$save_modal_button_continue, {
      # Check input
      error_list <- check_errors_text_embedding_model_embed(
        destination_path = input$save_modal_directory_path,
        folder_name = input$save_modal_folder_name,
        path_to_raw_texts = input$file_path_raw_texts,
        batch_size = input$batch_size
      )

      if (length(error_list) == 0) {
        start_and_monitor_long_task(
          id = id,
          ExtendedTask_type = "embed_raw_text",
          ExtendedTask_arguments = list(
            source_path = input$file_path_raw_texts,
            destination_path = input$save_modal_directory_path,
            destination_folder = input$save_modal_folder_name,
            log_path = log_path,
            batch_size = input$batch_size,
            model_path = model_path,
            log_write_interval = 2
          ),
          log_path = log_path,
          pgr_use_middle = FALSE,
          pgr_use_bottom = FALSE,
          update_intervall = 2000,
          success_type = "data_sets"
        )
      } else {
        display_errors(
          title = "Error",
          size = "l",
          easy_close = TRUE,
          error_messages = error_list
        )
      }
    })
    #--------------------------------------------------------------------------
  })
}

check_errors_text_embedding_model_embed <- function(destination_path,
                                                    folder_name,
                                                    path_to_raw_texts,
                                                    batch_size) {
  # List for gathering errors
  error_list <- NULL

  # Check if all inputs are correctly set
  if (!dir.exists(destination_path)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p("The destination directory does not
                                                   exist. Please check your directory path
                                                   and/or create that directory."))
  }
  if (is.null(folder_name) | folder_name == "") {
    error_list[length(error_list) + 1] <- "Folder name for the dataset storing the embeddings is missing."
  }

  if (!file.exists(path_to_raw_texts)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p("There is no file at the current path."))
  } else {
    raw_texts <- try(load_from_disk(dir_path = path_to_raw_texts), silent = TRUE)
    if ("try-error" %in% class(raw_texts)) {
      error_list[length(error_list) + 1] <- raw_texts
    } else {
      if (!"LargeDataSetForText" %in% class(raw_texts)) {
        error_list[length(error_list) + 1] <- paste("The object is not of class LargeDataSetForText")
      }
    }
  }


  # summary
  if (length(error_list) > 0) {
    tmp_ui_error <- NULL
    for (i in 1:length(error_list)) {
      tmp_ui_error[length(tmp_ui_error) + 1] <- list(
        shiny::tags$p(error_list[i])
      )
    }
    return(tmp_ui_error)
  } else {
    return(NULL)
  }
}
