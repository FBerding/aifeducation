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

#' @title Graphical user interface for classifiers - create
#' @description Functions generates the page for a creating new classifiers.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_classifier_create
#' @keywords internal
#'
Classifiers_Create_UI <- function(id) {
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
        shiny::actionButton(
          inputId = shiny::NS(id, "test_featureextractor_matching"),
          label = "Test TEFeatureExtractor",
          icon = shiny::icon("circle-question")
        )
      ),
      # Main Page---------------------------------------------------------------
      # Content depends in the selected base model
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
          bslib::card_header("Target Data"),
          bslib::card_body(
            shiny::textInput(
              inputId = shiny::NS(id, "target_dir"),
              label = shiny::tags$p(shiny::icon("folder"), "Path")
            ),
            bslib::layout_column_wrap(
              shiny::uiOutput(outputId = shiny::NS(id, "summary_data_targets")),
              shiny::uiOutput(outputId = shiny::NS(id, "output_target_levels"))
            )
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
              bslib::card_body(
                shinyFiles::shinyDirButton(
                  id = shiny::NS(id, "button_select_feature_extractor"),
                  label = "Choose TEFeatureExtractor",
                  title = "Please choose a folder",
                  icon = shiny::icon("folder-open")
                ),
                shiny::textInput(
                  inputId = shiny::NS(id, "feature_extractor_dir"),
                  label = shiny::tags$p(shiny::icon("folder"), "Path")
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Positional Embedding"
              ),
              bslib::card_body(
                shinyWidgets::materialSwitch(
                  inputId = shiny::NS(id, "add_pos_embedding"),
                  label = "Add Positional Embedding",
                  value = FALSE,
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
                  value = 0,
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
                shiny::sliderInput(
                  value=1,
                  min=0,
                  max=20,
                  step=1,
                  inputId = shiny::NS(id, "rec_layers"),
                  label = "Reccurrent Layers"
                ),
                shiny::sliderInput(
                  value=1,
                  min=1,
                  max=20,
                  step=1,
                  inputId = shiny::NS(id, "rec_size"),
                  label = "Reccurrent Layers Size"
                ),
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
                shiny::sliderInput(
                  value=0,
                  min=0,
                  max=20,
                  step=1,
                  inputId = shiny::NS(id, "dense_layers"),
                  label = "Dense Layers",
                  width = "100%"
                ),
                shiny::sliderInput(
                  value=1,
                  min=1,
                  max=20,
                  step=1,
                  inputId = shiny::NS(id, "dense_size"),
                  label = "Dense Layers Size",
                  width = "100%"
                ),
                shiny::sliderInput(
                  inputId = shiny::NS(id, "dense_dropout"),
                  label = "Dense Dropout",
                  value = 0.4,
                  min = 0,
                  max = 0.99,
                  step = 0.01
                )
              )
            ),
            shiny::uiOutput(outputId = shiny::NS(id, "protonet_embedding_layer"))
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
                shiny::selectInput(
                  inputId = shiny::NS(id, "sustainability_country"),
                  label = "Country for Sustainability Tracking",
                  choices = country_alpha_3_list,
                  # choices=NULL,
                  selected = "DEU"
                ),
                shiny::uiOutput(outputId = shiny::NS(id, "regular_train")),
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
                  inputId = shiny::NS(id, "sc_method"),
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
            ),
            shiny::uiOutput(outputId = shiny::NS(id, "protonet_train")),
          )
        )
      )
    )
  )
}

#' @title Server function for: graphical user interface for classifiers - create
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param log_dir `string` Path to the directory where the log files should be stored.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_classifier_create
#' @keywords internal
#'
Classifiers_Create_Server <- function(id, log_dir, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns
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

    # Target Data
    shinyFiles::shinyFileChoose(
      input = input,
      id = "button_select_target_data",
      roots = volumes,
      filetypes = c("csv", "rda", "rdata", "xlsx")
    )

    shiny::observeEvent(input$button_select_target_data,
      {
        tmp_file_path <- shinyFiles::parseFilePaths(volumes, input$button_select_target_data)
        if (nrow(tmp_file_path) > 0) {
          shiny::updateTextInput(
            inputId = "target_dir",
            value = tmp_file_path[[1, "datapath"]]
          )
        } else {
          shiny::updateTextInput(
            inputId = "target_dir",
            value = ""
          )
        }
      },
      ignoreNULL = FALSE
    )

    path_to_target_data <- shiny::eventReactive(input$target_dir, {
      if (input$target_dir != "") {
        return(input$target_dir)
      } else {
        return(NULL)
      }
    })

    data_targets <- shiny::reactive({
      if (!is.null(path_to_target_data())) {
        return(load_and_check_target_data(path_to_target_data()))
      } else {
        return(NULL)
      }
    })

    # FeatureExtractor
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_feature_extractor",
      roots = volumes,
      allowDirCreate = FALSE
    )
    shiny::observeEvent(input$button_select_feature_extractor, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_feature_extractor)
      shiny::updateTextInput(
        inputId = "feature_extractor_dir",
        value = path
      )
    })
    path_to_feature_extractor <- shiny::eventReactive(input$feature_extractor_dir, {
      if (input$feature_extractor_dir != "") {
        return(input$feature_extractor_dir)
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
      # Check vor valid arguments
      if (identical(as.double(input$alpha), numeric(0))) {
        loss_alpha <- NULL
      } else {
        loss_alpha <- as.double(input$alpha)
      }
      if (identical(as.double(input$margin), numeric(0))) {
        loss_margin <- NULL
      } else {
        loss_margin <- as.double(input$margin)
      }

      # Check for errors
      errors <- check_errors_create_classifier(
        classifier_type=input$classifier_type,
        destination_path = input$save_modal_directory_path,
        folder_name = input$save_modal_folder_name,
        path_to_embeddings = path_to_embeddings(),
        path_to_target_data = path_to_target_data(),
        path_to_feature_extractor = path_to_feature_extractor(),
        model_name = input$name,
        model_label = input$label,
        Ns = input$n_sample,
        Nq = input$n_query,
        loss_alpha = loss_alpha,
        loss_margin = loss_margin,
        embedding_dim = input$protonet_embedding_dim
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
          ExtendedTask_type = "classifier",
          ExtendedTask_arguments = list(
            classifier_type = input$classifier_type,
            destination_path = input$save_modal_directory_path,
            folder_name = input$save_modal_folder_name,
            path_to_embeddings = path_to_embeddings(),
            path_to_target_data = path_to_target_data(),
            target_levels = input$target_levels,
            target_data_column = input$data_target_column,
            path_to_feature_extractor = path_to_feature_extractor(),
            name = input$name,
            label = input$label,
            # text_embeddings=NULL,
            # feature_extractor=NULL,
            # targets=NULL,
            dense_layers = input$dense_layers,
            dense_size=input$dense_size,
            rec_layers = input$rec_layers,
            rec_size=input$rec_size,
            rec_type = input$rec_type,
            rec_bidirectional = input$rec_bidirectional,
            self_attention_heads = input$self_attention_heads,
            intermediate_size = input$intermediate_size,
            attention_type = input$attention_type,
            add_pos_embedding = input$add_pos_embedding,
            rec_dropout = input$rec_dropout,
            repeat_encoder = input$repeat_encoder,
            dense_dropout = input$dense_dropout,
            recurrent_dropout = 0,
            encoder_dropout = input$encoder_dropout,
            optimizer = input$optimizer,

            # data_embeddings,
            # data_targets,

            data_folds = input$data_folds,
            data_val_size = as.double(input$val_size),
            balance_class_weights = input$balance_class_weights,
            balance_sequence_length = input$balance_sequence_length,
            use_sc = input$use_sc,
            sc_method = input$sc_method,
            sc_min_k = input$sc_min_max_k[1],
            sc_max_k = input$sc_min_max_k[2],
            use_pl = input$use_pl,
            pl_max_steps = input$pl_max_steps,
            pl_max = as.double(input$pl_max),
            pl_anchor = as.double(input$pl_anchor),
            pl_min = as.double(input$pl_min),
            # sustain_track = TRUE,
            sustain_iso_code = input$sustainability_country,
            # sustain_region = NULL,
            # sustain_interval = 15,
            epochs = input$epochs,
            batch_size = input$batch_size,
            # dir_checkpoint,
            # trace=TRUE,
            # keras_trace=0,
            # pytorch_trace=0,
            log_dir = log_dir,
            log_write_interval = 3,
            Ns = input$n_sample,
            Nq = input$n_query,
            loss_alpha = loss_alpha,
            loss_margin = loss_margin,
            sampling_separate=input$sampling_separate,
            sampling_shuffle=input$sampling_shuffle,
            embedding_dim = input$protonet_embedding_dim
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

    # Target data
    output$summary_data_targets <- shiny::renderUI({
      target_data <- data_targets()
      # shiny::req(target_data)
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
      # shiny::req(data_targets())
      relevant_data <- data_targets()
      relevant_data <- relevant_data[input$data_target_column]
      if (nrow(relevant_data) > 0) {
        return(table(relevant_data, useNA = "always"))
      } else {
        return(NULL)
      }
    })

    target_levels_unsorted <- shiny::reactive({
      if (!is.null(data_targets())) {
        relevant_data <- data_targets()
        relevant_data <- relevant_data[input$data_target_column]
        if (nrow(relevant_data) > 0) {
          target_levels <- names(table(relevant_data, useNA = "no"))
          return(target_levels)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    output$output_target_levels <- shiny::renderUI({
      if (!is.null(target_levels_unsorted())) {
        return(
          sortable::rank_list(
            text = "Please select the order of categories/classes.",
            labels = target_levels_unsorted(),
            input_id = session$ns("target_levels"),
            class = c("default-sortable", "aifeducation-sortable")
          )
        )
      } else {
        return(NULL)
      }
    })

    # Pseudo labeling specific--------------------------------------------------
    output$dynamic_sample_weights <- shiny::renderUI({
      ui <- list(
        shiny::sliderInput(
          inputId = session$ns("pl_max"),
          label = "Max Certainty Value",
          value = 1,
          max = 1,
          min = input$pl_anchor,
          step = 0.01
        ),
        shiny::sliderInput(
          inputId = session$ns("pl_min"),
          label = "Min Certainty Value",
          value = 0,
          max = input$pl_anchor,
          min = 0,
          step = 0.01
        )
      )
      return(ui)
    })

    # Attention specific---------------------------------------------------------
    output$attention_layers_for_training <- shiny::renderUI({
      if (input$attention_type == "multihead") {
        ui <- list(
          shiny::sliderInput(
            inputId = session$ns("self_attention_heads"),
            label = "Number of Self Attention Heads",
            min = 1,
            value = 4,
            max = 48,
            step = 1,
            round = TRUE
          )
        )
        return(ui)
      } else {
        return(NULL)
      }
    })


    # Regular specific elements-------------------------------------------------
    output$regular_train <- shiny::renderUI({
      if (input$classifier_type == "regular") {
        ui <- shiny::tagList(
          shinyWidgets::materialSwitch(
            inputId = session$ns("balance_class_weights"),
            label = "Balance Class Weights",
            value = TRUE,
            status = "primary"
          ),
          shinyWidgets::materialSwitch(
            inputId = session$ns("balance_sequence_length"),
            label = "Balance Sequnce Length",
            value = TRUE,
            status = "primary"
          )
        )
      } else {
        ui <- NULL
      }
      return(ui)
    })

    # ProtoNet specific elements------------------------------------------------
    output$protonet_embedding_layer <- shiny::renderUI({
      if (input$classifier_type == "protonet") {
        ui <- bslib::card(
          bslib::card_header(
            "ProtoNet Embedding Layer"
          ),
          bslib::card_body(
            shiny::sliderInput(
              inputId = session$ns("protonet_embedding_dim"),
              label = "Dimension",
              value = 2,
              min = 1,
              max = 64,
              step = 1
            )
          )
        )
      } else {
        ui <- NULL
      }

      return(ui)
    })

    output$protonet_train <- shiny::renderUI({
      if (input$classifier_type == "protonet") {
        ui <- shiny::tagList(
          bslib::card(
            bslib::card_header(
              "ProtoNet Specific"
            ),
            bslib::card_body(
              shiny::sliderInput(
                inputId = session$ns("n_sample"),
                label = "N Sample",
                min = 1,
                max = 256,
                value = 5,
                step = 1
              ),
              shiny::sliderInput(
                inputId = session$ns("n_query"),
                label = "N Query",
                min = 1,
                max = 256,
                value = 2,
                step = 1
              ),
              shiny::sliderInput(
                inputId = session$ns("alpha"),
                label = "Alpha",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1
              ),
              shiny::sliderInput(
                inputId = session$ns("margin"),
                label = "Margin",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1
              ),
              shinyWidgets::materialSwitch(
                inputId = shiny::NS(id, "sampling_separate"),
                label = "Separate Sample and Query",
                value = FALSE,
                status = "primary"
              ),
              shinyWidgets::materialSwitch(
                inputId = shiny::NS(id, "sampling_shuffle"),
                label = "Shuffle",
                value = TRUE,
                status = "primary"
              )
            )
          )
        )
      } else {
        ui <- NULL
      }
      return(ui)
    })


    # Test Data matching--------------------------------------------------------
    # Data Sets
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
