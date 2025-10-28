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

#' @title Graphical user interface for displaying sustainability data of a model.
#' @description Functions generates the tab within a page for displaying sustainability data of an object.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_training
#' @keywords internal
#' @noRd
#'
Sustainability_UI <- function(id) {
  bslib::page(
    DT::dataTableOutput(outputId = shiny::NS(id, "ui_training")),
    DT::dataTableOutput(outputId = shiny::NS(id, "ui_inference"))
  )
}

#' @title Server function for: graphical user interface for displaying sustainability data of an object.
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
Sustainability_Server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    output$ui_training <- DT::renderDataTable({
      if (inherits(model(), "BaseModelCore") ||
        inherits(model(), "TextEmbeddingModel")) {
        if (inherits(model(), "BaseModelCore")) {
          sus_data <- model()$get_sustainability_data("training")
        } else {
          sus_data <- model()$BaseModel$get_sustainability_data("training")
        }
        if (nrow(sus_data) > 0) {
          select_columns <- c(
            "date",
            "task",
            "region.country_iso_code",
            "sustainability_data.duration_sec",
            "sustainability_data.co2eq_kg",
            "sustainability_data.total_energy_kwh"
          )
          sus_data <- sus_data[, select_columns]
          colnames(sus_data) <- c(
            "date",
            "task",
            "region",
            "duration (sec)",
            "co2eq kg",
            "total kwh"
          )
          return(sus_data)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    output$ui_inference <- DT::renderDataTable({
      if (inherits(model(), "BaseModelCore") ||
        inherits(model(), "TextEmbeddingModel")) {
        if (inherits(model(), "BaseModelCore")) {
          sus_data <- model()$get_sustainability_data("inference")
        } else {
          sus_data <- model()$BaseModel$get_sustainability_data("inference")
        }

        if (nrow(sus_data) > 0) {
          select_columns <- c(
            "date",
            "task",
            "region.country_iso_code",
            "sustainability_data.duration_sec",
            "sustainability_data.co2eq_kg",
            "sustainability_data.total_energy_kwh",
            "n",
            "batch",
            "min_seq_len",
            "mean_seq_len",
            "sd_seq_len",
            "max_seq_len"
          )
          sus_data <- sus_data[, select_columns]
          colnames(sus_data) <- c(
            "date",
            "task",
            "region",
            "duration (sec)",
            "co2eq kg",
            "total kwh",
            "n",
            "batch",
            "min_seq_len",
            "mean_seq_len",
            "sd_seq_len",
            "max_seq_len"
          )
          return(sus_data)
        } else {
          return(NULL)
        }
      }
    })
  })
}
