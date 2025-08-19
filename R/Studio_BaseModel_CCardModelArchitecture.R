#' @title Graphical user interface for displaying base model's architecture.
#' @description Functions generates the tab within a page for displaying base model's architecture.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_base_model_create_card_architecture
#' @keywords internal
#' @noRd
#'
ModelArchitecture_UI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page(
    bslib::card(
      bslib::card_header(
        "Model Architecture"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          # Column 1: General parameters -------------------------------------
          bslib::card(
            bslib::card_header("General parameters"),
            bslib::card_body(
              shiny::numericInput(
                inputId = ns("max_position_embeddings"),
                label = "Maximal sequence length",
                value = 512,
                min = 100,
                max = 8192,
                step = 1
              ),
              shiny::numericInput(
                inputId = ns("hidden_size"),
                label = "Hidden size",
                value = 768,
                min = 10,
                max = 2048,
                step = 1
              ),
              shiny::numericInput(
                inputId = ns("intermediate_size"),
                label = "Intermediate size",
                value = 3072,
                min = 16,
                max = 16384,
                step = 1
              ),
              shiny::selectInput(
                inputId = ns("hidden_act"),
                label = "Activation Function",
                choices = c("GELU", "ReLU", "silu", "gelu_new")
              ),
              shiny::sliderInput(
                inputId = ns("hidden_dropout_prob"),
                label = "Dropout Probability",
                value = 0.1,
                min = 0,
                max = .99,
                step = .01
              ),
              shiny::sliderInput(
                inputId = ns("attention_probs_dropout_prob"),
                label = "Attention Dropout Probability",
                value = 0.1,
                min = 0,
                max = .99,
                step = .01
              ),
              shiny::sliderInput(
                inputId = ns("num_attention_heads"),
                label = "n Attentions Heads",
                value = 12,
                min = 1,
                max = 56,
                step = 1
              )
            )
          ),
          # Column 2 -------------------------------------------------------
          shiny::tagList(
            # Model-based parameters ---------------------------------
            bslib::card(
              bslib::card_header("Model-based parameters"),
              bslib::card_body(
                shiny::selectInput(
                  inputId = ns("base_architecture_type"),
                  choices = c(
                    "bert",
                    "mpnet",
                    "roberta",
                    "deberta_v2",
                    "funnel",
                    "longformer"
                  ),
                  label = "Base Architecture"
                ),
                shiny::uiOutput(outputId = ns("model_based_config"))
              )
            ),
            # Vocab parameters ---------------------------------
            bslib::card(
              bslib::card_header("Vocabulary"),
              bslib::card_body(
                shinyFiles::shinyDirButton(
                  id = ns("button_select_dataset_dir"),
                  label = "Choose Folder",
                  title = "Please choose a folder",
                  icon = shiny::icon("folder-open")
                ),
                shiny::textInput(
                  inputId = ns("dataset_dir_path"),
                  label = shiny::tags$p(shiny::icon("folder"), "Dataset Folder"),
                  width = "100%"
                ),
                shiny::numericInput(
                  inputId = ns("vocab_size"),
                  label = "Size of Vocabulary",
                  value = 30522,
                  min = 100,
                  max = 200000,
                  step = 1
                ),
                shiny::uiOutput(outputId = ns("model_based_vocab_config"))
              )
            )
          )
        )
      )
    )
  )
}

#' @title Server function for: graphical user interface for base model's architecture
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return `list` of the parameters for creation the transformers.
#'
#' @family studio_gui_base_model_create_card_architecture
#' @keywords internal
#' @noRd
#'
ModelArchitecture_Server <- function(id, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # Global variables -----------------------------------------------------------
    ns <- session$ns

    generalReactiveParams <- shiny::reactive({
      list(
        max_position_embeddings = input$max_position_embeddings,
        hidden_size = input$hidden_size,
        intermediate_size = input$intermediate_size,
        hidden_act = input$hidden_act,
        hidden_dropout_prob = input$hidden_dropout_prob,
        attention_probs_dropout_prob = input$attention_probs_dropout_prob,
        num_attention_heads = input$num_attention_heads,
        ai_method = input$base_architecture_type,
        vocab_size = input$vocab_size,
        dataset_dir_path = input$dataset_dir_path
      )
    })

    # Column 2
    # Model-based parameters ---------------------------------------------------------
    model_based_config_ui <- shiny::eventReactive(input$base_architecture_type, {
      # All model-based parameters -----------------------------------------------
      num_hidden_layer_ui <- shiny::sliderInput(
        inputId = ns("num_hidden_layer"),
        label = "n Hidden Layers",
        value = 12,
        min = 1,
        max = 56,
        step = 1
      )
      target_hidden_size_ui <- shiny::numericInput(
        inputId = ns("target_hidden_size"),
        label = "Target Hidden size",
        value = 64,
        min = 2,
        max = 2048,
        step = 1
      )
      n_blocks_ui <- shiny::sliderInput(
        inputId = ns("n_blocks"),
        label = "n Blocks",
        value = 12,
        min = 1,
        max = 56,
        step = 1
      )
      block_sizes_ui <- shiny::sliderInput(
        inputId = ns("block_sizes"),
        label = "Block Sizes",
        value = 4,
        min = 1,
        max = 56,
        step = 1
      )
      num_decoder_layers_ui <- shiny::sliderInput(
        inputId = ns("num_decoder_layers"),
        label = "n Decoding Layers",
        value = 2,
        min = 1,
        max = 24,
        step = 1
      )
      activation_dropout_ui <- shiny::sliderInput(
        inputId = ns("activation_dropout"),
        label = "Activation Dropout Probability",
        value = 0.0,
        min = 0,
        max = .99,
        step = .01
      )
      attention_window_ui <- shiny::numericInput(
        inputId = ns("attention_window"),
        label = "Size Attention Window",
        value = 512,
        min = 10,
        max = 8192,
        step = 1
      )

      # Get model-based parameters -----------------------------------------------
      if (input$base_architecture_type %in% c("bert", "roberta", "deberta_v2", "mpnet")) {
        model_based_params_list <- shiny::tagList(
          num_hidden_layer_ui
        )
      } else if (input$base_architecture_type == "funnel") {
        model_based_params_list <- shiny::tagList(
          target_hidden_size_ui,
          n_blocks_ui,
          block_sizes_ui,
          num_decoder_layers_ui,
          activation_dropout_ui
        )
      } else if (input$base_architecture_type == "longformer") {
        model_based_params_list <- shiny::tagList(
          num_hidden_layer_ui,
          attention_window_ui
        )
      }
      return(model_based_params_list)
    })

    output$model_based_config <- shiny::renderUI({
      model_based_config_ui()
    })

    modelBasedReactiveParams <- shiny::reactive({
      params <- list()
      if (input$base_architecture_type %in% c("bert", "roberta", "deberta_v2", "mpnet")) {
        params <- list(
          num_hidden_layer = input$num_hidden_layer
        )
      } else if (input$base_architecture_type == "funnel") {
        blocks <- rep(x = input$block_sizes, times = input$n_blocks)

        params <- list(
          target_hidden_size = input$target_hidden_size,
          block_sizes = blocks,
          num_decoder_layers = input$num_decoder_layers,
          activation_dropout = input$activation_dropout
        )
      } else if (input$base_architecture_type == "longformer") {
        params <- list(
          num_hidden_layer = input$num_hidden_layer,
          attention_window = input$attention_window
        )
      }
      return(params)
    })



    # General vocab parameters -------------------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_dataset_dir",
      roots = volumes,
      allowDirCreate = FALSE
    )
    shiny::observeEvent(input$button_select_dataset_dir, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_dataset_dir)
      shiny::updateTextInput(
        inputId = "dataset_dir_path",
        value = path
      )
    })

    # Model-based vocab parameters ---------------------------------------------------------
    model_based_vocab_config_ui <- shiny::eventReactive(input$base_architecture_type, {
      # All model-based vocab parameters -------------------------------------------
      vocab_do_lower_case_ui <- shinyWidgets::materialSwitch(
        inputId = ns("vocab_do_lower_case"),
        value = FALSE,
        label = "Transform to Lower Case",
        status = "primary"
      )
      add_prefix_space_ui <- shinyWidgets::materialSwitch(
        inputId = ns("add_prefix_space"),
        value = FALSE,
        right = TRUE,
        label = "Add Prefix Space",
        status = "primary"
      )
      trim_offsets_ui <- shinyWidgets::materialSwitch(
        inputId = ns("trim_offsets"),
        value = TRUE,
        right = TRUE,
        label = "Trim Offsets",
        status = "primary"
      )

      # Get model-based vocab parameters --------------------------------------------
      if (input$base_architecture_type %in% c("bert", "deberta_v2", "funnel", "mpnet")) {
        model_based_vocab_params_list <- shiny::tagList(
          vocab_do_lower_case_ui
        )
      } else if (input$base_architecture_type %in% c("roberta", "longformer")) {
        model_based_vocab_params_list <- shiny::tagList(
          add_prefix_space_ui,
          trim_offsets_ui
        )
      }
      return(model_based_vocab_params_list)
    })

    output$model_based_vocab_config <- shiny::renderUI({
      model_based_vocab_config_ui()
    })

    modelBasedVocabReactiveParams <- shiny::reactive({
      params <- list()
      if (input$base_architecture_type %in% c("bert", "deberta_v2", "funnel", "mpnet")) {
        params <- list(
          vocab_do_lower_case = input$vocab_do_lower_case
        )
      } else if (input$base_architecture_type %in% c("roberta", "longformer")) {
        params <- list(
          add_prefix_space = input$add_prefix_space,
          trim_offsets = input$trim_offsets
        )
      }
      return(params)
    })

    return(shiny::reactive({
      c(
        generalReactiveParams(),
        modelBasedReactiveParams(),
        modelBasedVocabReactiveParams()
      )
    }))
  })
}
