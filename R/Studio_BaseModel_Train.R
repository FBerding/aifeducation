#' @title Graphical user interface for base models - train
#' @description Functions generates the page for using a [].
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_base_model_train
#' @keywords internal
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
              "Raw Texts"
            ),
            bslib::card_body(
              RawTexts_UI(id = ns("BaseModel_RawTexts"))
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
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_base_model_train
#' @keywords internal
#'
BaseModel_Train_Server <- function(id, log_dir, volumes, sustain_tracking) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global variables -----------------------------------------------------------
    ns <- session$ns
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
    raw_texts_file_path_reactive <- RawTexts_Server(
      id = "BaseModel_RawTexts",
      volumes = volumes
    )
    params_reactive <- TrainTuneSettings_Server(
      id = "BaseModel_TrainTuneSettings",
      model_architecture = model_architecture_reactive
    )

    shiny::observeEvent(input$button_train_tune, {
      model_architecture <- model_architecture_reactive()

      raw_texts_file_path <- raw_texts_file_path_reactive()

      # Checking ------------------------------------------------------------------

      errors <- c()

      if (raw_texts_file_path == "") {
        errors <- append(errors, "Please specify a path to the raw texts for the training.")
      } else if (!file.exists(raw_texts_file_path)) {
        errors <- append(errors, paste(
          "Path to the raw texts for the training is not valid - there is no such file path",
          dQuote(raw_texts_file_path)
        ))
      }

      if (input$output_model_dir_path == "") {
        errors <- append(errors, "Please specify a directory path for saiving the trained Base Model.")
      }

      if (class(model_architecture) == "errors") {
        errors <- append(errors, model_architecture)
      } else if (class(model_architecture) == "params") {
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

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tagList(
              shiny::tags$i(class = "fa fa-exclamation-triangle", style = "color: red;"),
              "Train error(s)"
            ),
            shiny::HTML(error_msg),
            easyClose = FALSE,
            footer = shiny::modalButton("Close")
          )
        )
      } else { # No errors ----------------------------------------------------------
        model_params <- params_reactive()
        print(model_params)

        # model_architecture is:
        # params <- list(
        #   model_exists = model_exists,
        #   model_dir_path = model_path,
        #   model_architecture = model_architecture,
        #   max_position_embeddings = max_position_embeddings
        # )

        # model_params <- list()
        # model_params[["ml_framework"]] <- "pytorch"
        # model_params[["output_dir"]] <- input$output_model_dir_path
        # model_params[["model_dir_path"]] <- model_architecture$model_dir_path
        # model_params[["sustain_track"]] <- sustain_tracking$is_sustainability_tracked
        # model_params[["sustain_iso_code"]] <- sustain_tracking$sustainability_country
        # model_params[["log_dir"]] <- log_dir
        # model_params[["log_write_interval"]] <- 2
        #
        # start_and_monitor_long_task(
        #   id = id,
        #   ExtendedTask_type = "train_transformer",
        #   ExtendedTask_arguments = list(
        #     # transformer_type = params$ai_method,
        #     raw_texts_file_path = raw_texts_file_path,
        #     params = model_params
        #   ),
        #   log_path = log_path,
        #   pgr_use_middle = TRUE,
        #   success_type = "create_transformer",
        #   update_intervall = 2
        # )
      }
    })
  })
}
