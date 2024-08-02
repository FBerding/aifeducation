Classifiers_Create_UI <- function(id) {
  tagList(
    bslib::page_sidebar(
      # Sidebar------------------------------------------------------------------
      sidebar = bslib::sidebar(
        position = "left",
        tags$h3("Control Panel"),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "button_select_dataset_for_embeddings"),
          label = "Choose Embeddings",
          title = "Please choose a folder",
          icon = shiny::icon("folder-open")
        ),
        shinyFiles::shinyFilesButton(
          id = shiny::NS(id, "button_select_target_data"),
          multiple = FALSE,
          label = "Choose Target Data",
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
        shiny::selectInput(
          inputId = shiny::NS(id, "classifier_type"),
          choices = c("regular", "protonet"),
          label = "Classifier Type"
        ),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "start_SaveModal"),
          label = "Train Model",
          title = "Choose Destination",
          icon = shiny::icon("floppy-disk")
        ),
        shiny::tags$hr(),
        shiny::actionButton(
          inputId = shiny::NS(id, "test_data_matching"),
          label = "Test Data Matching",
          icon = shiny::icon("circle-question")
        ),
      ),
      # Main Page---------------------------------------------------------------
      # Content depends in the selected base model
      bslib::layout_column_wrap(
        bslib::card(
          bslib::card_header("Input Data"),
          bslib::card_body(
            shiny::uiOutput(outputId = shiny::NS(id, "summary_data_embeddings"))
          )
        ),
        bslib::card(
          bslib::card_header("Target Data"),
          bslib::card_body(
            shiny::uiOutput(outputId = shiny::NS(id, "summary_data_targets"))
          )
        )
      ),
      bslib::card(
        bslib::card_header(
          "Architecture"
        ),
        bslib::card_body(
          bslib::layout_column_wrap(
            bslib::card(
              bslib::card_header(
                "Feature Extractor"
              ),
              bslib::card_body()
            ),
            bslib::card(
              bslib::card_header(
                "Positional Embedding"
              ),
              bslib::card_body(
                shinyWidgets::materialSwitch(
                  inputId = shiny::NS(id, "add_pos_embedding"),
                  label = "Add Positional Embedding",
                  value = TRUE,
                  status = "primary"
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Optimizer"
              ),
              bslib::card_body(
                shiny::selectInput(
                  inputId = shiny::NS(id, "optimizer"),
                  label = "Optimizer",
                  choices = c("adam", "rmsprop")
                )
              )
            )
          ),
          bslib::layout_column_wrap(
            bslib::card(
              bslib::card_header(
                "Encoder Layers"
              ),
              bslib::card_body(
                shiny::selectInput(
                  inputId = shiny::NS(id, "attention_type"),
                  choices = c("fourier", "multihead"),
                  label = "Attention Type"
                ),
                shiny::uiOutput(outputId = shiny::NS(id, "attention_layers_for_training")),
                shiny::sliderInput(
                  inputId = "intermediate_size",
                  label = "Intermediate Size",
                  min = 0,
                  value = 512,
                  max = 8096,
                  step = 1,
                  round = TRUE
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "repeat_encoder"),
                  label = "Number Encoding Layers",
                  value = 1,
                  min = 0,
                  max = 48,
                  step = 1,
                  round = TRUE
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "encoder_dropout"),
                  label = "Encoder Layers Dropout",
                  value = 0.1,
                  min = 0,
                  max = 0.99,
                  step = 0.01
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Recurrent Layers"
              ),
              bslib::card_body(
                shiny::textInput(
                  inputId = shiny::NS(id, "rec"),
                  label = "Reccurrent Layers"
                ),
                shiny::textOutput(outputId = shiny::NS(id, "rec_layer_check")),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "rec_dropout"),
                  label = "Reccurent Layers Dropout",
                  value = 0.1,
                  min = 0,
                  max = 0.99,
                  step = 0.01
                ),
                shiny::selectInput(
                  inputId = shiny::NS(id, "rec_type"),
                  label = "Type",
                  choices = c("gru", "lstm")
                ),
                shinyWidgets::materialSwitch(
                  inputId = shiny::NS(id, "rec_bidirectional"),
                  value = FALSE,
                  label = "Bidirectional",
                  status = "primary"
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Dense Layers"
              ),
              bslib::card_body(
                shiny::textInput(
                  inputId = shiny::NS(id, "hidden"),
                  label = "Dense Layers",
                  width = "100%"
                ),
                shiny::textOutput(outputId = shiny::NS(id, "dense_layer_check")),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "dense_dropout"),
                  label = "Dense Dropout",
                  value = 0.4,
                  min = 0,
                  max = 0.99,
                  step = 0.01
                )
              )
            )
          )
        )
      ),
      bslib::card(
        bslib::card_header(
          "Training Settings"
        ),
        bslib::card_body(
          bslib::layout_column_wrap(
            bslib::card(
              bslib::card_header(
                "General Settings"
              ),
              bslib::card_body(
                shinyWidgets::materialSwitch(
                  inputId = shiny::NS(id, "balance_class_weights"),
                  label = "Balance Class Weights",
                  value = TRUE,
                  status = "primary"
                ),
                shinyWidgets::materialSwitch(
                  inputId = shiny::NS(id, "balance_sequence_length"),
                  label = "Balance Sequnce Length",
                  value = TRUE,
                  status = "primary"
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "data_folds"),
                  label = "Number of Folds",
                  value = 5,
                  min = 1,
                  max = 25,
                  round = TRUE,
                  step = 1
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "val_size"),
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
                  min = 1,
                  max = 256,
                  value = 32,
                  step = 1
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Synthetic Cases"
              ),
              bslib::card_body(
                shinyWidgets::materialSwitch(
                  inputId = shiny::NS(id, "use_sc"),
                  value = FALSE,
                  label = "Add Synthetic Cases",
                  status = "primary"
                ),
                shiny::selectInput(
                  inputId = shiny::NS(id, "sc_methods"),
                  label = "Method",
                  choices = c("dbsmote", "adas", "smote")
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "sc_min_max_k"),
                  label = "Min k",
                  value = c(1, 10),
                  min = 1,
                  max = 20,
                  step = 1,
                  round = TRUE
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Pseudo Labeling"
              ),
              bslib::card_body(
                shinyWidgets::materialSwitch(
                  inputId = shiny::NS(id, "use_pl"),
                  value = FALSE,
                  label = "Add Pseudo Labeling",
                  status = "primary"
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "pl_max_steps"),
                  label = "Max Steps",
                  value = 5,
                  min = 1,
                  max = 20,
                  step = 1,
                  round = TRUE
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "pl_anchor"),
                  label = "Certainty Anchor",
                  value = 1,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                shiny::uiOutput(outputId = shiny::NS(id, "dynamic_sample_weights"))
              )
            )
          )
        )
      )
    )
  )
}

Classifiers_Create_Server <- function(id, log_dir, volumes) {
  moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
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
    path_to_embeddings <- shiny::eventReactive(input$button_select_dataset_for_embeddings, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_dataset_for_embeddings)
      return(path)
    })

    data_embeddings <- shiny::reactive({
      if (length(path_to_embeddings()) > 0) {
        return(load_and_check_embeddings(path_to_embeddings()))
      } else {
        return(NULL)
      }
    })

    # Target Data
    shinyFiles::shinyFileChoose(
      input = input,
      id = "button_select_target_data",
      roots = volumes,
      filetypes = c("csv", "rda", "rdata", "xlsx")
    )

    path_to_target_data <- shiny::eventReactive(input$button_select_target_data,
      {
        tmp_file_path <- shinyFiles::parseFilePaths(volumes, input$button_select_target_data)
        if (nrow(tmp_file_path) > 0) {
          return(tmp_file_path[[1, "datapath"]])
        } else {
          return(NULL)
        }
      },
      ignoreNULL = FALSE
    )

    data_targets <- shiny::reactive({
      return(load_and_check_target_data(path_to_target_data()))
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

    # Display Data Summary------------------------------------------------------
    # Embeddings
    output$summary_data_embeddings <- shiny::renderUI({
      embeddings <- data_embeddings()
      shiny::req(embeddings)
      if (!is.null(embeddings)) {
        model_info <- embeddings$get_model_info()
        info_table <- matrix(
          nrow = 3,
          ncol = 4,
          data = ""
        )
        info_table[1, 1] <- "Model Method:"
        info_table[2, 1] <- "Hidden State Aggregation:"
        info_table[3, 1] <- "Model Language:"

        info_table[1, 2] <- model_info$model_method
        info_table[2, 2] <- model_info$param_aggregation
        info_table[3, 2] <- model_info$model_language

        info_table[1, 3] <- "Tokens per Chunk:"
        info_table[2, 3] <- "Max Chunks:"
        info_table[3, 3] <- "Token Overlap:"

        info_table[1, 4] <- model_info$param_seq_length
        info_table[2, 4] <- model_info$param_chunks
        info_table[3, 4] <- model_info$param_overlap

        ui <- list(
          bslib::value_box(
            value = embeddings$n_rows(),
            title = "Number of Cases",
            showcase = shiny::icon("list")
          ),
          shiny::tags$h3("Model:", model_info$model_label),
          shiny::tags$p("Name:", model_info$model_name),
          shiny::tags$p("Created", model_info$model_date),
          shiny::renderTable(
            expr = info_table,
            colnames = FALSE
          )
        )
        return(ui)
      } else {
        return(NULL)
      }
    })

    # Target data
    output$summary_data_targets <- shiny::renderUI({
      target_data <- data_targets()
      shiny::req(target_data)
      if (!is.null(target_data)) {
        column_names <- colnames(target_data)
        column_names <- setdiff(x = column_names, y = c("id", "text"))
        ui <- list(
          bslib::value_box(
            value = nrow(target_data),
            title = "Number of Cases",
            showcase = shiny::icon("list")
          ),
          shiny::selectInput(
            inputId = ns("data_target_column"),
            label = "Select a Column",
            choices = column_names
          ),
          shiny::tableOutput(outputId = ns("data_target_abs_freq"))
        )
      } else {
        return(NULL)
      }
    })

    output$data_target_abs_freq <- shiny::renderTable({
      shiny::req(data_targets())
      relevant_data <- data_targets()
      relevant_data <- relevant_data[input$data_target_column]
      if (nrow(relevant_data) > 0) {
        return(table(relevant_data, useNA = "always"))
      } else {
        return(NULL)
      }
    })

    # Test Data matching--------------------------------------------------------
    shiny::observeEvent(input$test_data_matching,
      {
        cond_1 <- (!is.null(data_embeddings()))
        cond_2 <- (!is.null(data_targets()))

        if (cond_1 & cond_2) {
          embeddings <- data_embeddings()
          targets <- data_targets()[input$target_data_column]
          ids <- embeddings$get_ids()
          matched_cases <- intersect(
            x = ids,
            y = rownames(targets)
          )
          n_matched_cases <- length(matched_cases)
          shinyWidgets::show_alert(
            title = "Matching Results",
            text = paste(
              n_matched_cases,
              "out of",
              embeddings$n_rows(),
              "could be matched"
            ),
            type = "info"
          )
        } else {
          display_errors(
            title = "Error",
            size = "l",
            easy_close = TRUE,
            error_messages = "Embeddings and target data must be selected before matching is possible."
          )
        }
      },
      ignoreInit = TRUE
    )



    # Error handling-----------------------------------------------------------


    #--------------------------------------------------------------------------
  })
}

check_errors_text_embedding_model_create <- function(destination_path,
                                                     folder_name,
                                                     path_to_base_model,
                                                     interface_architecture) {
  # List for gathering errors
  error_list <- NULL

  # Check if all inputs are correctly set
  if (!dir.exists(destination_path)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p("The destination directory does not
                                                   exist. Please check your directory path
                                                   and/or create that directory."))
  }
  if (is.null(folder_name) | folder_name == "") {
    error_list[length(error_list) + 1] <- "File name for the text dataset is missing."
  }

  if (!identical(path_to_base_model, character(0))) {
    if (is.null(interface_architecture[[1]]) &
      is.null(interface_architecture[[2]])) {
      error_list[length(error_list) + 1] <- "There is no model to load in the directory."
    }
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
