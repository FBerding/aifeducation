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
