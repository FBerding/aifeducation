#' @title Graphical user interface for displaying train and tune settings of the base model.
#' @description Functions generates the tab within a page for displaying train and tune settings of the base model.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_base_model_train_card_train_tune_settings
#' @keywords internal
#' @noRd
#'
TrainTuneSettings_UI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page(
    shiny::uiOutput(outputId = ns("train_tune_settings"))
  )
}

#' @title Server function for: graphical user interface for train and tune settings of the base model.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model_architecture `list` with model architecture parameters from the BaseModel card.
#' @return Returns `NULL` or `list` of the parameters for training the transformers.
#'
#' @family studio_gui_base_model_train_card_train_tune_settings
#' @keywords internal
#' @noRd
#'
TrainTuneSettings_Server <- function(id, model_architecture) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global variables -----------------------------------------------------------
    ns <- session$ns

    # Parameters -----------------------------------------------
    train_tune_settings_ui <- shiny::eventReactive(model_architecture(), {
      model_architecture <- model_architecture()

      if (inherits(x = model_architecture, what = "errors") || !model_architecture$model_exists) {
        return(NULL)
      }

      # All possible ui parameters ---------------------------------
      # General ui parameters ----------------------------
      p_mask_ui <- shiny::sliderInput(
        inputId = ns("p_mask"),
        label = "Probability of Token Masking",
        value = .15,
        min = .05,
        max = .95,
        step = .01
      )
      chunk_size_ui <- shiny::sliderInput(
        inputId = ns("chunk_size"),
        label = "Chunk Size",
        value = 250,
        min = 100,
        max = model_architecture$max_position_embeddings,
        step = 1
      )
      min_seq_len_ui <- shiny::sliderInput(
        inputId = ns("min_seq_len"),
        label = "Minimal Sequence Length",
        value = 50,
        min = 10,
        max = model_architecture$max_position_embeddings,
        step = 1
      )
      val_size_ui <- shiny::sliderInput(
        inputId = ns("val_size"),
        label = "Validation Size",
        value = .10,
        min = .01,
        max = .99,
        step = .01
      )
      batch_size_ui <- shiny::sliderInput(
        inputId = ns("batch_size"),
        label = "Batch Size",
        value = 12,
        min = 1,
        max = 64,
        step = 1
      )
      n_epoch_ui <- shiny::numericInput(
        inputId = ns("n_epoch"),
        label = "N Epochs",
        value = 50,
        min = 1,
        max = NA,
        step = 1
      )
      learning_rate_ui <- shiny::numericInput(
        inputId = ns("learning_rate"),
        label = "Learning Rate",
        value = 0.003,
        min = 0.0001,
        max = 1,
        step = .001
      )
      full_sequences_only_ui <- shinyWidgets::materialSwitch(
        inputId = ns("full_sequences_only"),
        value = FALSE,
        label = shiny::tags$b("Full Sequences Only"),
        status = "primary"
      )
      # Model-based ui parameters -----------------------
      p_perm_ui <- shiny::sliderInput(
        inputId = ns("p_perm"),
        label = "Probability of Token Permutation",
        value = .15,
        min = .05,
        max = .95,
        step = .01
      )
      whole_word_ui <- shinyWidgets::materialSwitch(
        inputId = ns("whole_word"),
        value = TRUE,
        label = shiny::tags$b("Whole Word Masking"),
        status = "primary"
      )

      # Defining actual parameters list --------------------------------------
      if (model_architecture$model_architecture == "MPNetForMPLM_PT") {
        parameters_list <- bslib::layout_column_wrap(
          shiny::tagList(
            chunk_size_ui,
            min_seq_len_ui,
            p_mask_ui,
            p_perm_ui,
            val_size_ui,
            batch_size_ui
          ),
          shiny::tagList(
            n_epoch_ui,
            learning_rate_ui,
            full_sequences_only_ui,
            whole_word_ui
          )
        )
      } else if (model_architecture$model_architecture %in% c("BertModel", "FunnelModel", "DebertaV2ForMaskedLM")) {
        parameters_list <- bslib::layout_column_wrap(
          shiny::tagList(
            chunk_size_ui,
            min_seq_len_ui,
            p_mask_ui,
            val_size_ui,
            batch_size_ui
          ),
          shiny::tagList(
            n_epoch_ui,
            learning_rate_ui,
            full_sequences_only_ui,
            whole_word_ui
          )
        )
      } else if (model_architecture$model_architecture %in% c("RobertaModel", "LongformerModel")) {
        parameters_list <- bslib::layout_column_wrap(
          shiny::tagList(
            chunk_size_ui,
            min_seq_len_ui,
            p_mask_ui,
            val_size_ui,
            batch_size_ui
          ),
          shiny::tagList(
            n_epoch_ui,
            learning_rate_ui,
            full_sequences_only_ui
          )
        )
      }

      return(parameters_list)
    })

    output$train_tune_settings <- shiny::renderUI({
      train_tune_settings_ui()
    })

    params_reactive <- shiny::reactive({
      model_architecture <- model_architecture()

      if (inherits(x = model_architecture, what = "errors") || !model_architecture$model_exists) {
        return(NULL)
      }

      # Parameters list ----------------------------------
      params_list <- list(
        p_mask = input$p_mask,
        chunk_size = input$chunk_size,
        min_seq_len = input$min_seq_len,
        val_size = input$val_size,
        batch_size = input$batch_size,
        n_epoch = input$n_epoch,
        learning_rate = input$learning_rate,
        full_sequences_only = input$full_sequences_only
      )

      if (model_architecture$model_architecture == "MPNetForMPLM_PT") {
        params_list[["p_perm"]] <- input$p_perm
        params_list[["whole_word"]] <- input$whole_word
      } else if (model_architecture$model_architecture %in% c("BertModel", "FunnelModel", "DebertaV2ForMaskedLM")) {
        params_list[["whole_word"]] <- input$whole_word
      }

      return(params_list)
    })

    return(params_reactive)
  })
}
