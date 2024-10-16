#' @title Graphical user interface for displaying base model's path.
#' @description Functions generates the tab within a page for displaying base model's path.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_base_model_train_card_base_model
#' @keywords internal
#'
BaseModel_UI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page(
    shinyFiles::shinyDirButton(
      id = ns("button_select_model"),
      label = "Choose a Base Model",
      title = "Please choose a folder",
      icon = shiny::icon("folder-open")
    ),
    shiny::textInput(
      inputId = ns("model_dir_path"),
      label = shiny::tags$p(shiny::icon("folder"), "Path to Base Model"),
      width = "100%"
    )
  )
}

#' @title Server function for: graphical user interface for base model's path.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_base_model_create
#' @keywords internal
#'
BaseModel_Server <- function(id, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global variables -----------------------------------------------------------
    ns <- session$ns

    # Control Panel --------------------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_model",
      roots = volumes,
      allowDirCreate = FALSE
    )
    shiny::observeEvent(input$button_select_model, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_model)
      shiny::updateTextInput(
        inputId = "model_dir_path",
        value = path
      )
    })

    model_dir <- shiny::eventReactive(input$model_dir_path, {
      if (input$model_dir_path != "") {
        return(input$model_dir_path)
      } else {
        return(NULL)
      }
    })
  })
}
