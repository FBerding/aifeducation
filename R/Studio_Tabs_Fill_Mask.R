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

#' @title Graphical user interface for fill-mask tasks.
#' @description Functions generates the tab within a page for using an object of class [TextEmbeddingModel] for
#'   fill-mask tasks.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_text_embedding_model_fill_mask
#' @keywords internal
#'
Fill_Mask_UI <- function(id) {
  bslib::page(
    bslib::layout_column_wrap(
      bslib::card(
        bslib::card_header(
          "Text Sequence"
        ),
        bslib::card_body(
          shiny::uiOutput(outputId = shiny::NS(id, "token_table")),
          shiny::textAreaInput(
            inputId = shiny::NS(id, "txt_for_fill_mask"),
            rows = 5,
            label = "Text",
            width = "100%"
          ),
          shiny::numericInput(
            inputId = shiny::NS(id, "n_fillments_for_fill_mask"),
            label = "N Solutions per mask",
            value = 5,
            min = 1,
            max = 50
          ),
          shiny::actionButton(
            inputId = shiny::NS(id, "fill_mask_start"),
            label = "Calculate Tokens",
            width = "100%",
            icon = shiny::icon("paper-plane")
          )
        )
      ),
      bslib::card(
        bslib::card_header(
          "Estimated Tokens"
        ),
        bslib::card_body(
          bslib::layout_column_wrap(
            shiny::sliderInput(
              inputId = shiny::NS(id, "plot_text_size"),
              min = 1,
              max = 20,
              value = 10,
              step = 0.5,
              label = "Text Size"
            ),
            shiny::numericInput(
              inputId = shiny::NS(id, "select_mask_for_fill_mask"),
              value = 1,
              min = 1,
              max = 1,
              label = "Select Mask Token"
            )
          ),
          shiny::plotOutput(outputId = shiny::NS(id, "scores_for_fill_mask"))
        )
      )
    )
  )
}

#' @title Server function for: graphical user interface for fill-mask tasks.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model Model used for inference.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @importFrom rlang .data
#'
#' @family studio_gui_text_embedding_model_fill_mask
#' @keywords internal
#'
Fill_Mask_Server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    # TODO (Yuliia): Remove? Variable is not used
    ns <- session$ns

    # Render Token table--------------------------------------------------------
    output$token_table <- shiny::renderTable({
      shiny::req(model)
      model()$get_special_tokens()
    })

    # Calculate tokens for the masks--------------------------------------------
    fill_masked_solutions <- shiny::eventReactive(input$fill_mask_start, {
      shiny::req(model)

      solutions <- try(
        model()$fill_mask(
          text = input$txt_for_fill_mask,
          n_solutions = input$n_fillments_for_fill_mask
        ),
        silent = TRUE
      )

      if (methods::is(solutions, class2 = "try-error") == FALSE) {
        shiny::updateNumericInput(
          inputId = "select_mask_for_fill_mask",
          max = length(solutions)
        )

        return(solutions)
      } else {
        display_errors(
          title = "Error",
          size = "l",
          easy_close = TRUE,
          error_messages = "Text does not contain at least one mask token. Please
                 check your input."
        )
        return(NULL)
      }
    })

    # Generate plot-----------------------------------------------------------
    output$scores_for_fill_mask <- shiny::renderPlot(
      {
        plot_data <- fill_masked_solutions()[[input$select_mask_for_fill_mask]]
        plot_data <- plot_data[order(plot_data$score, decreasing = FALSE), ]
        plot_data$token_str <- factor(plot_data$token_str, levels = (plot_data$token_str))
        plot_data <- as.data.frame(plot_data)
        # TODO (Yuliia): .data has no visible binding
        plot <- ggplot2::ggplot(data = plot_data) +
          ggplot2::geom_col(
            ggplot2::aes(
              x = .data$token_str,
              y = .data$score
            )
          ) +
          ggplot2::coord_flip() +
          ggplot2::xlab("tokens") +
          ggplot2::ylab("score") +
          ggplot2::theme_classic() +
          ggplot2::theme(text = ggplot2::element_text(size = input$plot_text_size))
        return(plot)
      },
      res = 2 * 72
    )
  })
}
