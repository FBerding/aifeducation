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

#' @title Graphical user interface for displaying FLOPS estimates of a model.
#' @description Functions generates the tab within a page for displaying FLOPS estimates of an object.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_training
#' @keywords internal
#' @noRd
#'
FLOPS_UI <- function(id) {
  bslib::page(
    DT::dataTableOutput(outputId = shiny::NS(id, "ui_flops")),
  )
}

#' @title Server function for: graphical user interface for displaying FLOPS estimates of an object.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model Model used for inference.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @importFrom rlang .data
#'
#' @family studio_gui_training
#' @keywords internal
#' @noRd
#'
FLOPS_Server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    output$ui_flops<-DT::renderDataTable({
      if(inherits(model(), "BaseModelCore")){
        data <- model()$get_flops_estimates()

      } else if(inherits(model(), "TextEmbeddingModel")){
        data <- model()$BaseModel$get_flops_estimates()
      } else {
        data <- NULL
      }
      return(data)
  })
  })
}
