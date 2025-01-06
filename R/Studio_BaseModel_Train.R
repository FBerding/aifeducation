#' @title Graphical user interface for base models - train
#' @description Functions generates the page for using the [.AIFE*Transformer]s.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_base_model_train
#' @keywords internal
#' @noRd
#'
BaseModel_Train_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::page_sidebar(
      # Sidebar ------------------------------------------------------------------
      sidebar = bslib::sidebar(
        position = "left",
        shiny::tags$h3("Control Panel"),
        shinyFiles::shinyDirButton(
          id = ns("button_select_output_model_dir"),
          label = "Choose Folder",
          title = "Choose Destination",
          icon = shiny::icon("folder-open")
        ),
        shiny::textInput(
          inputId = ns("output_model_dir_path"),
          label = shiny::tags$p(shiny::icon("folder"), "Path for saiving the trained Base Model"),
          width = "100%"
        ),
        shiny::actionButton(
          inputId = ns("button_train_tune"),
          label = "Start Training/Tuning",
          icon = shiny::icon("paper-plane")
        ),
        shiny::uiOutput(outputId = ns("sidebar_description"))
      ),
      # Main panel ------------------------------------------------------------------
      bslib::page(
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header(
              "Base Model"
            ),
            bslib::card_body(
              BaseModel_UI(id = ns("BaseModel_BaseModel"))
            )
          ),
          bslib::card(
            bslib::card_header(
              "Dataset"
            ),
            bslib::card_body(
              Dataset_UI(id = ns("BaseModel_Dataset"))
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            "Train and Tune Settings"
          ),
          bslib::card_body(
            TrainTuneSettings_UI(id = ns("BaseModel_TrainTuneSettings"))
          )
        )
      )
    )
  )
}

#' @title Server function for: graphical user interface for base models - train
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param log_dir `string` Path to the directory where the log files should be stored.
#' @param volumes `vector` containing a named vector of available volumes.
#' @param sustain_tracking `list` with the sustainability tracking parameters.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_base_model_train
#' @keywords internal
#' @noRd
#'
BaseModel_Train_Server <- function(id, log_dir, volumes, sustain_tracking) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global variables -----------------------------------------------------------
    log_path <- paste0(log_dir, "/aifeducation_state.log")

    # Control Panel --------------------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_output_model_dir",
      roots = volumes,
      allowDirCreate = TRUE
    )
    shiny::observeEvent(input$button_select_output_model_dir, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_output_model_dir)
      shiny::updateTextInput(
        inputId = "output_model_dir_path",
        value = path
      )
    })

    # Main Panel ------------------------------------------------------------------
    model_architecture_reactive <- BaseModel_Server(
      id = "BaseModel_BaseModel",
      volumes = volumes
    )
    dataset_dir_path_reactive <- Dataset_Server(
      id = "BaseModel_Dataset",
      volumes = volumes
    )
    params_reactive <- TrainTuneSettings_Server(
      id = "BaseModel_TrainTuneSettings",
      model_architecture = model_architecture_reactive
    )

    shiny::observeEvent(input$button_train_tune, {
      model_architecture <- model_architecture_reactive()

      dataset_dir_path <- dataset_dir_path_reactive()

      # Checking ------------------------------------------------------------------

      errors <- c()

      if (dataset_dir_path == "") {
        errors <- append(errors, "Please specify a path to the dataset for the training.")
      } else if (!dir.exists(dataset_dir_path)) {
        errors <- append(errors, paste(
          "Path to the raw texts for the training is not valid - there is no such directory path",
          dQuote(dataset_dir_path)
        ))
      }

      if (input$output_model_dir_path == "") {
        errors <- append(errors, "Please specify a directory path for saiving the trained Base Model.")
      }

      if (inherits(x = model_architecture, what = "errors")) {
        errors <- append(errors, model_architecture)
      } else if (inherits(x = model_architecture, what = "params")) {
        if (!model_architecture$model_exists) {
          errors <- append(errors, paste(
            "There is no model to load in the directory",
            model_architecture$model_dir_path
          ))
        }
      }

      # If there is an error -------------------------------------------------------
      if (length(errors) != 0) {
        error_msg <- paste(errors, collapse = "<br>")

        shinyWidgets::show_alert(
          title = "Train error(s)",
          text = shiny::HTML(error_msg),
          html = TRUE,
          type = "error"
        )
      } else { # No errors ----------------------------------------------------------
        sustain_tracking <- sustain_tracking()

        TransformerType <- list(
          BertModel = "bert",
          DebertaV2ForMaskedLM = "deberta_v2",
          FunnelModel = "funnel",
          LongformerModel = "longformer",
          MPNetForMPLM_PT = "mpnet",
          RobertaModel = "roberta"
        )
        transformer_type <- TransformerType[[model_architecture$model_architecture]]

        model_params <- params_reactive()

        model_params[["ml_framework"]] <- "pytorch"
        model_params[["output_dir"]] <- input$output_model_dir_path
        model_params[["model_dir_path"]] <- model_architecture$model_dir_path
        model_params[["sustain_track"]] <- sustain_tracking$is_sustainability_tracked
        model_params[["sustain_iso_code"]] <- sustain_tracking$sustainability_country
        model_params[["n_workers"]] <- 1
        model_params[["multi_process"]] <- FALSE
        model_params[["keras_trace"]] <- 0
        model_params[["pytorch_trace"]] <- 0
        model_params[["log_dir"]] <- log_dir
        model_params[["log_write_interval"]] <- 2

        start_and_monitor_long_task(
          id = id,
          ExtendedTask_type = "train_transformer",
          ExtendedTask_arguments = list(
            transformer_type = transformer_type,
            dataset_dir_path = dataset_dir_path,
            params = model_params
          ),
          log_path = log_path,
          pgr_use_middle = TRUE,
          pgr_use_bottom = TRUE,
          pgr_use_graphic = TRUE,
          success_type = "train_transformer",
          update_intervall = 2
        )
      }
    })
  })
}
