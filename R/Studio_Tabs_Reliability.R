#' @title Graphical user interface for displaying the reliability of classifiers.
#' @description Functions generates the tab within a page for displaying infomration on the reliability of classifiers.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_page_classifier_reliability
#' @keywords internal
#'
Reliability_UI <- function(id) {
  bslib::page(
    shiny::uiOutput(outputId = shiny::NS(id, "ui_relability")
    )
  )
}

#' @title Server function for: graphical user interface for displaying the reliability of classifiers.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model Model used for inference.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_page_classifier_reliability
#' @keywords internal
#'
Reliability_Server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns
    measure_labels <- list(
      iota_index = "Iota Index",
      min_iota2 = "Minimum Iota",
      avg_iota2 = "Average Iota",
      max_iota2 = "Maximum Iota",
      min_alpha = "Minimum Alpha",
      avg_alpha = "Average Alpha",
      max_alpha = "Maximum Alpha",
      static_iota_index = "Static Iota Index",
      dynamic_iota_index = "Dynamic Iota Index",
      kalpha_nominal = "Krippendorff's Alpha (Nominal)",
      kalpha_ordinal = "Krippendorff's Alpha (ordinal)",
      kendall = "Kendall's W",
      kappa2_unweighted = "Cohen's Kappa (Unweighted)",
      kappa2_equal_weighted = "Weighted Cohen's Kappa (Equal Weights)",
      kappa2_squared_weighted = "Weighted Cohen's Kappa (Squared Weights)",
      kappa_fleiss = "Fleiss' Kappa for Multiple Raters (Without Exact Estimation)",
      percentage_agreement = "Percentage Agreement",
      balanced_accuracy = "Average Accuracy within each Class",
      gwet_ac = "Gwet's AC1/AC2 Agreement Coefficient"
    )
    measures_scale_level <- c(
      "dynamic_iota_index",
      "kalpha_nominal",
      "kalpha_ordinal",
      "kendall",
      "kappa2_unweighted",
      "kappa2_equal_weighted",
      "kappa2_squared_weighted",
      "kappa_fleiss",
      "percentage_agreement",
      "balanced_accuracy",
      "gwet_ac"
    )
    #-------------
    output$ui_relability <- shiny::renderUI({
      shiny::req(model())
      classifier <- model()

      # Set up measures
      measures_shared <- intersect(
        x = names(measure_labels[measures_scale_level]),
        y = names(classifier$reliability$test_metric_mean)
      )
      reliability_scale <- classifier$reliability$test_metric_mean[measures_shared]
      reliability_scale <- t(as.matrix(reliability_scale))
      colnames(reliability_scale) <- measure_labels[measures_shared]


      # Create ui
      ui <- shiny::tagList(
        bslib::card(
          bslib::card_header(
            "Coding Stream Analysis"
          ),
          bslib::card_body(
            bslib::page_sidebar(
              sidebar = bslib::sidebar(
                position = "right",
                shiny::sliderInput(
                  inputId = ns("codings_stream_text_size"),
                  label = "Text Size",
                  min = 1,
                  max = 20,
                  value = 10,
                  step = 0.25
                ),
                shiny::sliderInput(
                  inputId = ns("codings_stream_labels_size"),
                  label = "Text Size Categories",
                  min = 0.1,
                  max = 5,
                  value = 3,
                  step = 0.1
                ),
                shiny::sliderInput(
                  inputId = ns("codings_stream_key_size"),
                  label = "Key Size",
                  min = 0.1,
                  max = 2,
                  value = 0.1,
                  step = 0.1
                )
              ),
              shiny::plotOutput(outputId = ns("coding_stream_plot")),
              shiny::tags$p("Note: Plot is calculated based on a freely estimated Assignment-Error-Matrix.
                                          The categorical sizes are based on the relative frequencies of the training data.
                                          These sizes are not identical with the sizes of field samples.")
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            "Spectral Analysis"
          ),
          bslib::card_body(
            bslib::page_sidebar(
              sidebar = bslib::sidebar(
                position = "right",
                shiny::sliderInput(
                  inputId = ns("codings_spectral_text_size"),
                  label = "Text Size",
                  min = 1,
                  max = 20,
                  value = 10,
                  step = 0.25
                ),
                shiny::sliderInput(
                  inputId = ns("codings_spectral_number_size"),
                  label = "Number Size",
                  min = 0.1,
                  max = 5,
                  value = 3,
                  step = 0.1
                ),
                shiny::sliderInput(
                  inputId = ns("codings_spectral_key_size"),
                  label = "Key Size",
                  min = 0.1,
                  max = 2,
                  value = 0.1,
                  step = 0.1
                )
              ),
              shiny::plotOutput(outputId = ns("coding_spectral_plot")),
              shiny::tags$p("Note: Plot is calculated based on a freely estimated Assignment-Error-Matrix.
                                          The categorical sizes are based on the relative frequencies of the training data.
                                          These sizes are not identical with the sizes of field samples.")
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            "Measures"
          ),
          bslib::card_body(
            bslib::layout_column_wrap(
              bslib::card(
                bslib::card_header(
                  "Scale Level"
                ),
                bslib::card_body(
                  shiny::renderTable(t(reliability_scale),
                    rownames = TRUE,
                    colnames = FALSE
                  ),
                  shiny::tags$p("Note: Values for Dynamic Iota Index are calculated based on a restricted Assignment-Error-Matrix.")
                )
              ),
              bslib::card(
                bslib::card_header(
                  "Categorical Level"
                ),
                bslib::card_body(
                  shiny::tags$p(shiny::tags$b("Assignment-Error-Matrix")),
                  shiny::renderTable(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$assignment_error_matrix,
                    rownames = TRUE,
                    colnames = TRUE
                  ),
                  shiny::tags$p(shiny::tags$b("Iota")),
                  shiny::renderTable(t(as.matrix(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$iota))),
                  shiny::tags$p(shiny::tags$b("Alpha Reliability")),
                  shiny::renderTable(t(as.matrix(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$alpha_reliability))),
                  shiny::tags$p(shiny::tags$b("Beta Reliability")),
                  shiny::renderTable(t(as.matrix(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$beta_reliability))),
                  shiny::tags$p("Note: All values are calculated based on a freely estimated Assignment-Error-Matrix.")
                )
              ),
              bslib::card(
                bslib::card_header(
                  "Measures - Machine Learning"
                ),
                bslib::card_body(
                  shiny::renderTable(classifier$reliability$standard_measures_mean)
                )
              )
            )
          )
        )
      )
    })

    # Render Plots-------------------------------------------------------------
    output$coding_stream_plot <- shiny::renderPlot(
      expr = {
        classifier <- model()
        plot <- iotarelr::plot_iota2_alluvial(
          object = classifier$reliability$iota_object_end_free,
          label_categories_size = input$codings_stream_labels_size,
          key_size = input$codings_stream_key_size,
          text_size = input$codings_stream_text_size
        )
        return(plot)
      },
      res = 2 * 72
    )

    output$coding_spectral_plot <- shiny::renderPlot(
      expr = {
        classifier <- model()
        plot <- iotarelr::plot_iota(
          object = classifier$reliability$iota_object_end_free,
          number_size = input$codings_spectral_number_size,
          key_size = input$codings_spectral_key_size,
          text_size = input$codings_spectral_text_size
        )
        return(plot)
      },
      res = 2 * 72
    )

    #--------------------------------------------------------------------------
  })
}
