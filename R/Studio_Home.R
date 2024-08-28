#' @title Graphical user interface for the start page of AI for Education - Studio
#' @description Functions generates the page for the "home" page of the shiny app.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_pages_and_tabs
#' @keywords internal
#'
Studio_Home_UI <- function(id) {
  shiny::tagList(
    bslib::page(
      bslib::layout_column_wrap(
        bslib::card(
          bslib::card_header(
            "AI for Education - Studio"
          ),
          bslib::card_body(
            shiny::tags$img(
              src = "studio_logo.jpg",
              align = "center",
              width = "100%"
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            "Support"
          ),
          bslib::card_body(
            shiny::tags$a("Package's Home Page",
              href = "https://fberding.github.io/aifeducation/index.html",
              target = "_blank"
            ),
            shiny::tags$a("Introducation to AI for Education - Studio",
              href = "https://fberding.github.io/aifeducation/articles/gui_aife_studio.html",
              target = "_blank"
            )
          )
        )
      )
    )
  )
}
