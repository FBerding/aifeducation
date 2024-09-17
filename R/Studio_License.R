#' Graphical user interface for showing the license.
#'
#' Functions generates the page for displaying the license.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_license
#' @keywords internal
#'
License_UI <- function(id) {
  shiny::tagList(
    bslib::card(
      bslib::card_header("License"),
      bslib::card_body(
        shiny::uiOutput(
          outputId = shiny::NS(id, "gpl3_license")
        )
      )
    )
  )
}



#' Server function for: graphical user interface for showing the license.
#'
#' Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_license
#' @keywords internal
#'
License_Server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    # TODO (Yuliia): remove? Variable is not used
    ns <- session$ns

    output$gpl3_license <- shiny::renderUI(
      shiny::markdown(
        mds = readLines(
          con = system.file("LICENSE_GPL3.md",
            package = "aifeducation"
          )
        )
      )
    )
  })
}
