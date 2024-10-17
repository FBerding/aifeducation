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
          label = "Choose a Base Model",
          title = "Choose Destination",
          icon = shiny::icon("folder-open")
        ),
        shiny::textInput(
          inputId = ns("output_model_dir_path"),
          label = shiny::tags$p(shiny::icon("folder"), "Path to Base Model"),
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
    # log_path <- paste0(log_dir, "/aifeducation_state.log")

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
    BaseModel_Server(
      id = "BaseModel_BaseModel",
      volumes = volumes
    )
  })
}
