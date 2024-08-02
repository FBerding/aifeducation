DataManagement_RawTextsUI <- function(id) {
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        tags$h3("Control Panel"),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "start_SaveModal"),
          label = "Create Data Set",
          title = "Choose Destination",
          icon = shiny::icon("floppy-disk")
        )
      ),
      bslib::page(
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header("Text Sources"),
            bslib::card_body(
              width = "33%",
              shinyFiles::shinyDirButton(
                id = shiny::NS(id, "source_dir_select"),
                label = "Choose Folder",
                title = "Please choose a folder",
                icon = shiny::icon("folder-open")
              ),
              shiny::textInput(
                inputId = shiny::NS(id, "text_source_dir"),
                label = shiny::tags$p(shiny::icon("folder"), "Path to Folder")
              )
            )
          ),
          bslib::card(
            bslib::card_header("File Types"),
            bslib::card_body(
              bslib::card(
                bslib::card_body(
                  width = "33%", shinyWidgets::materialSwitch(
                    inputId = shiny::NS(id, "include_txt"),
                    label = shiny::tags$p("Include .txt ", shiny::icon(name = "file")),
                    right = TRUE,
                    inline = FALSE,
                    value = TRUE,
                    status = "primary"
                  )
                )
              ),
              bslib::card(
                bslib::card_body(
                  shinyWidgets::materialSwitch(
                    inputId = shiny::NS(id, "include_pdf"),
                    label = shiny::tags$p("Include .pdf", shiny::icon(name = "file-pdf")),
                    right = TRUE,
                    inline = FALSE,
                    value = FALSE,
                    status = "primary"
                  )
                )
              ),
              bslib::card(
                bslib::card_body(
                  shinyWidgets::materialSwitch(
                    inputId = shiny::NS(id, "include_xlsx"),
                    label = shiny::tags$p("Include .xlsx", shiny::icon(name = "file-excel")),
                    right = TRUE,
                    inline = FALSE,
                    value = FALSE,
                    status = "primary"
                  ),
                  shiny::textInput(
                    inputId = shiny::NS(id, "excel_id_column"),
                    label = "Name of ID column for xlsx files"
                  ),
                  shiny::textInput(
                    inputId = shiny::NS(id, "excel_text_column"),
                    label = "Name of text column for xlsx files"
                  ),
                  shiny::textInput(
                    inputId = shiny::NS(id, "excel_license_column"),
                    label = "Name of license column for xlsx files"
                  ),
                  shiny::textInput(
                    inputId = shiny::NS(id, "excel_bib_entry_column"),
                    label = "Name of bib entry column for xlsx files"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

DataManagement_RawTextsServer <- function(id, log_dir, volumes) {
  moduleServer(id, function(input, output, session) {
    # local variables------------------------------------------------------------
    log_path <- paste0(log_dir, "/aifeducation_state.log")

    # File system management----------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "source_dir_select",
      roots = volumes,
      allowDirCreate = FALSE
    )
    shiny::observeEvent(input$source_dir_select, {
      shiny::updateTextInput(
        inputId = "text_source_dir",
        value = shinyFiles::parseDirPath(volumes, input$source_dir_select)
      )
    })

    # Start screen for choosing the location for storing the data set-----------
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

    # Implement Algorithm--------------------------------------------------------
    # button_continue comes from the SaveModal
    shiny::observeEvent(input$save_modal_button_continue, {
      # Check for errors
      errors <- check_errors_create_dataset_raw_texts(
        source_path = input$text_source_dir,
        destination_path = input$save_modal_directory_path,
        folder_name = input$save_modal_folder_name,
        include_txt = input$include_txt,
        include_pdf = input$include_pdf,
        include_xlsx = input$include_xlsx,
        excel_id_column = input$excel_id_column,
        excel_text_column = input$excel_text_column,
        excel_license_column = input$excel_license_column,
        excel_bib_entry_column = input$excel_bib_entry_column
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
          ExtendedTask_type = "raw_texts",
          ExtendedTask_arguments = list(
            source_path = input$text_source_dir,
            destination_path =  input$save_modal_directory_path,
            destination_folder = input$save_modal_folder_name,
            log_path = log_path,
            include_txt = input$include_txt,
            include_pdf = input$include_pdf,
            include_xlsx = input$include_xlsx,
            excel_id_column = input$excel_id_column,
            excel_text_column = input$excel_text_column,
            excel_license_column = input$excel_license_column,
            excel_bib_entry_column = input$excel_bib_entry_column,
            log_write_interval=2
          ),
          log_path = log_path,
          pgr_use_middle = TRUE,
          pgr_use_bottom = FALSE,
          update_intervall = 2000,
          success_type = "data_sets"
        )
      }
    })
    #--------------------------------------------------------------------------
  })
}

check_errors_create_dataset_raw_texts <- function(source_path,
                                                  destination_path,
                                                  folder_name,
                                                  include_txt,
                                                  include_pdf,
                                                  include_xlsx,
                                                  excel_id_column,
                                                  excel_text_column,
                                                  excel_license_column,
                                                  excel_bib_entry_column) {
  # List for gathering errors
  error_list <- NULL

  # Check if all inputs are correctly set
  if (dir.exists(source_path) == FALSE) {
    error_list[length(error_list) + 1] <- "Source directory does not exist. Please check
      your directory path."
  }
  if (is.null(destination_path) | destination_path == "") {
    error_list[length(error_list) + 1] <- "Path to the output directory is missing."
  }
  if (is.null(folder_name) | folder_name == "") {
    error_list[length(error_list) + 1] <- "File name for the text dataset is missing."
  }
  if (dir.exists(dirname(destination_path)) == FALSE) {
    error_list[length(error_list) + 1] <- "Target directory does not exist. Please check
      if a directory exists for saving your data."
  }
  if (include_txt == FALSE &
    include_pdf == FALSE &
    include_xlsx == FALSE) {
    error_list[length(error_list) + 1] <- "No file types selected. Please select
      at least one file type."
  }

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
