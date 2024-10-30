#' @title Graphical user interface for displaying dataset directory path for training a base model.
#' @description Functions generates the tab within a page for displaying raw texts path for training a base model.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_base_model_train_card_dataset
#' @keywords internal
#'
Dataset_UI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page(
    shinyFiles::shinyDirButton(
      id = ns("button_select_dataset_dir"),
      label = "Choose Folder",
      title = "Please choose a folder",
      icon = shiny::icon("folder-open")
    ),
    shiny::textInput(
      inputId = ns("dataset_dir_path"),
      label = shiny::tags$p(shiny::icon("folder"), "Dataset Folder"),
      width = "100%"
    )
  )
}

#' @title Server function for: graphical user interface for dataset directory path for training a base model.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return `string` Path to the raw texts for training the transformers.
#'
#' @family studio_gui_base_model_train_card_dataset
#' @keywords internal
#'
Dataset_Server <- function(id, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global variables -----------------------------------------------------------
    ns <- session$ns

    # Control Panel --------------------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_dataset_dir",
      roots = volumes,
      allowDirCreate = FALSE
    )
    shiny::observeEvent(input$button_select_dataset_dir, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_dataset_dir)
      shiny::updateTextInput(
        inputId = "dataset_dir_path",
        value = path
      )
    })

    return(shiny::reactive({
      input$dataset_dir_path
    }))
  })
}
