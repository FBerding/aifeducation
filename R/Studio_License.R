# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' Graphical user interface for showing the license.
#'
#' Functions generates the page for displaying the license.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_license
#' @keywords internal
#' @noRd
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
