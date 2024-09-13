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
          bslib::card_header("License"),
          bslib::card_body(
            shiny::tags$p(
              "aifeducation is free software: you can redistribute it and/or modify
              it under the terms of the GNU General Public License version 3 as published by
              the Free Software Foundation."
            ),
            shiny::tags$p(
              "aifeducation is distributed in the hope that it will be useful,
              but WITHOUT ANY WARRANTY; without even the implied warranty of
              MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
              GNU General Public License for more details."
            ),
            shiny::tags$p(
              "You find a copy of this license by clicking on `License`
            on the navbar at the top."
            ),
            shiny::tags$p(
              "According to section 2 of GPL-3 output generated with
                          aifeducation is not covered by this license. Please
                          select an adequate license for output if necessary."
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
