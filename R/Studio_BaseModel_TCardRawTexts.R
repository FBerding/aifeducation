#' @title Graphical user interface for displaying raw texts path for training a base model.
#' @description Functions generates the tab within a page for displaying raw texts path for training a base model.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_base_model_train_card_raw_texts
#' @keywords internal
#'
RawTexts_UI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page(
    shinyFiles::shinyFilesButton(
      id = ns("button_select_raw_texts_file"),
      label = "Choose File",
      title = "Please choose a file",
      icon = shiny::icon("file"),
      filetype = c("rda", "rdata"),
      multiple = FALSE
    ),
    shiny::textInput(
      inputId = ns("raw_texts_file_path"),
      label = shiny::tags$p(shiny::icon("file"), "File Path"),
      width = "100%"
    )
  )
}

#' @title Server function for: graphical user interface for raw texts path for training a base model.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return `string` Path to the raw texts for training the transformers.
#'
#' @family studio_gui_base_model_train_card_raw_texts
#' @keywords internal
#'
RawTexts_Server <- function(id, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global variables -----------------------------------------------------------
    ns <- session$ns

    # Control Panel --------------------------------------------------------------
    shinyFiles::shinyFileChoose(
      input = input,
      id = "button_select_raw_texts_file",
      roots = volumes,
      filetype = c("rda", "rdata")
    )
    shiny::observeEvent(input$button_select_raw_texts_file, {
      tmp_file_path <- shinyFiles::parseFilePaths(volumes, input$button_select_raw_texts_file)
      if (nrow(tmp_file_path) > 0) {
        shiny::updateTextInput(
          inputId = "raw_texts_file_path",
          value = tmp_file_path[[1, "datapath"]]
        )
      }
    })

    return(shiny::reactive({
      input$raw_texts_file_path
    }))
  })
}
