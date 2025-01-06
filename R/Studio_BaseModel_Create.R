#' @title Graphical user interface for base models - create
#' @description Functions generates the page for using the [.AIFE*Transformer]s.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_base_model_create
#' @keywords internal
#' @noRd
#'
BaseModel_Create_UI <- function(id) {
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
          label = shiny::tags$p(shiny::icon("folder"), "Path for saiving the created Base Model"),
          width = "100%"
        ),
        shiny::actionButton(
          inputId = ns("button_create"),
          label = "Start Creation",
          icon = shiny::icon("paper-plane")
        )
      ),
      # Main panel ------------------------------------------------------------------
      bslib::page(
        ModelArchitecture_UI(id = ns("BaseModel_ModelArchitecture"))
      )
    )
  )
}

#' @title Server function for: graphical user interface for base models - create.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param log_dir `string` Path to the directory where the log files should be stored.
#' @param volumes `vector` containing a named vector of available volumes.
#' @param sustain_tracking `list` with the sustainability tracking parameters.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_base_model_create
#' @keywords internal
#' @noRd
#'
BaseModel_Create_Server <- function(id, log_dir, volumes, sustain_tracking) {
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
    params_reactive <- ModelArchitecture_Server(
      id = "BaseModel_ModelArchitecture",
      volumes = volumes
    )

    shiny::observeEvent(input$button_create, {
      params <- params_reactive()
      sustain_tracking <- sustain_tracking()

      # Checking ------------------------------------------------------------------

      errors <- c()

      if (params$dataset_dir_path == "") {
        errors <- append(errors, "Please specify a path to the dataset for a vocabulary.")
      } else if (!dir.exists(params$dataset_dir_path)) {
        errors <- append(errors, paste(
          "Path to the dataset for a vocabulary is not valid - there is no such directory path",
          dQuote(params$dataset_dir_path)
        ))
      }

      if (input$output_model_dir_path == "") {
        errors <- append(errors, "Please specify a directory path for saving the model.")
      }

      # If there is an error -------------------------------------------------------
      if (length(errors) != 0) {
        error_msg <- paste(errors, collapse = "<br>")

        shinyWidgets::show_alert(
          title = "Creation error(s)",
          text = shiny::HTML(error_msg),
          html = TRUE,
          type = "error"
        )
      } else { # No errors ----------------------------------------------------------
        model_params <- params
        # Remove ai_method and dataset_dir_path from model_params list
        model_params <- model_params[!names(model_params) %in% c("ai_method", "dataset_dir_path")]
        model_params[["ml_framework"]] <- "pytorch"
        model_params[["model_dir"]] <- input$output_model_dir_path
        model_params[["sustain_track"]] <- sustain_tracking$is_sustainability_tracked
        model_params[["sustain_iso_code"]] <- sustain_tracking$sustainability_country
        model_params[["log_dir"]] <- log_dir
        model_params[["log_write_interval"]] <- 2

        start_and_monitor_long_task(
          id = id,
          ExtendedTask_type = "create_transformer",
          ExtendedTask_arguments = list(
            transformer_type = params$ai_method,
            dataset_dir_path = params$dataset_dir_path,
            params = model_params
          ),
          log_path = log_path,
          pgr_use_middle = TRUE,
          success_type = "create_transformer",
          update_intervall = 2
        )
      }
    })
  })
}
