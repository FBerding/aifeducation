#' @title Graphical user interface for displaying model's descriptions.
#' @description Functions generates the tab within a page for displaying model's descriptions.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_description
#' @keywords internal
#'
Description_UI <- function(id) {
  bslib::page(
    bslib::card(
      bslib::card_header(
        "Bibliographic Description"
      ),
      bslib::card_body(
        shiny::uiOutput(outputId = shiny::NS(id, "bibliographic"))
      )
    ),
    bslib::card(
      bslib::card_header(
        "Model Description"
      ),
      bslib::card_body(
        shinyWidgets::switchInput(
          inputId = shiny::NS(id, "language_select"),
          label = "Language",
          onLabel = "English",
          offLabel = "Native",
          value = TRUE,
          labelWidth = "80px"
        ),
        shiny::uiOutput(outputId = shiny::NS(id, "description"))
      )
    )
  )
}

#' @title Server function for: graphical user interface for displaying model's descriptions.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model Model used for inference.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_description
#' @keywords internal
#'
Description_Server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    # TODO (Yuliia): remove? Variable is not used
    ns <- session$ns

    # Bibliographic Description------------------------------------------------
    output$bibliographic <- shiny::renderUI({
      shiny::req(model())
      return(generate_model_bib_description(model = model()))
    })

    # Model description
    output$description <- shiny::renderUI({
      shiny::req(model())
      return(generate_model_description(model = model(), eng = input$language_select))
    })
  })
}
