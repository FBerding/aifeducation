#'Graphical user interface for text embedding models - create
#'
#'Functions generates the page for a creating a new [TextEmbeddingModel].
#'
#'@param id `string` determining the id for the namespace.
#'@return This function does nothing return. It is used to build a page for a shiny app.
#'
#'@family studio_gui_page_text_embedding_model_create
#'@keywords internal
#'
TextEmbeddingModel_Create_UI <- function(id) {
  shiny::tagList(
    bslib::page_sidebar(
      # Sidebar------------------------------------------------------------------
      sidebar = bslib::sidebar(
        position = "left",
        shiny::tags$h3("Control Panel"),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "button_select_base_model"),
          label = "Choose a Base Model",
          title = "Please choose a folder",
          icon = shiny::icon("folder-open")
        ),
        shiny::tags$hr(),
        shiny::textInput(
          inputId = shiny::NS(id, "lm_model_name"),
          label = "Name"
        ),
        shiny::textInput(
          inputId = shiny::NS(id, "lm_model_label"),
          label = "Label"
        ),
        shiny::textInput(
          inputId = shiny::NS(id, "lm_model_language"),
          label = "Language"
        ),
        shiny::tags$hr(),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "start_SaveModal"),
          label = "Save Model",
          title = "Choose Destination",
          icon = shiny::icon("floppy-disk")
        )
      ),
      # Main Page---------------------------------------------------------------
      # Content depends in the selected base model
      bslib::card(
        bslib::card_header(shiny::textOutput(outputId = shiny::NS(id, "path_to_base_model"))),
        bslib::card_body(
          shiny::uiOutput(outputId = shiny::NS(id, "lm_interface_setting"))
        )
      )
    )
  )
}

#'Server function for: graphical user interface for text embedding models - create
#'
#'Functions generates the functionality of a page on the server.
#'
#'@param id `string` determining the id for the namespace.
#'@param log_dir `string` Path to the directory where the log files should be stored.
#'@param volumes `vector` containing a named vector of available volumes.
#'@return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#'@family studio_gui_page_text_embedding_model_create
#'@keywords internal
#'
TextEmbeddingModel_Create_Server <- function(id, log_dir, volumes) {
  moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns=session$ns

    # File system management----------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_base_model",
      roots = volumes,
      # session = session,
      allowDirCreate = FALSE
    )
    path_to_base_model <- shiny::eventReactive(input$button_select_base_model, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_base_model)
      return(path)
    })

    # Start screen for choosing the location for storing the data set-----------
    # Create Save Modal
    save_modal <- create_save_modal(
      id = id,
      title = "Choose Destination",
      easy_close = FALSE,
      size = "l"
    )

    # Implement file connection
    shinyFiles::shinyDirChoose(
      input = input,
      id = "start_SaveModal",
      roots = volumes,
      allowDirCreate = TRUE
    )

    # show save_modal
    observeEvent(input$start_SaveModal, {
      path <- shinyFiles::parseDirPath(volumes, input$start_SaveModal)
      if (!is.null(path) & !identical(path, character(0))) {
        if (path != "") {
          shiny::showModal(save_modal)
          shiny::updateTextInput(
            inputId = "save_modal_directory_path",
            value = path
          )
        }
      }
    })

    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_model_destination_dir",
      roots = volumes,
      # session = session,
      allowDirCreate = TRUE
    )

    shiny::observeEvent(input$button_model_destination_dir, {
      shiny::updateTextInput(
        inputId = ns("destination_dir"),
        value = shinyFiles::parseDirPath(volumes, input$button_model_destination_dir)
      )
    })


    # Load Information on Base Model-----------------------------------------------------------
    # Load the model and extract information
    interface_architecture <- shiny::eventReactive(path_to_base_model(), {
      model_path <- path_to_base_model()
      if (file.exists(paste0(
        model_path,
        "/",
        "tf_model.h5"
      ))) {
        model <- transformers$TFAutoModel$from_pretrained(model_path)
        model_architecture <- model$config$architectures
        max_position_embeddings <- model$config$max_position_embeddings
        if (model_architecture == "FunnelForMaskedLM") {
          max_layer <- sum(model$config$block_repeats * model$config$block_sizes)
        } else {
          max_layer <- model$config$num_hidden_layers
        }
      } else if (file.exists(paste0(
        model_path,
        "/",
        "pytorch_model.bin"
      ))) {
        model <- transformers$AutoModel$from_pretrained(model_path)
        model_architecture <- model$config$architectures
        max_position_embeddings <- model$config$max_position_embeddings
        if (model_architecture == "FunnelForMaskedLM") {
          max_layer <- sum(model$config$block_repeats * model$config$block_sizes)
        } else {
          max_layer <- model$config$num_hidden_layers
        }
      } else if (file.exists(paste0(
        model_path,
        "/",
        "model.safetensors"
      ))) {
        model <- transformers$AutoModel$from_pretrained(model_path)
        model_architecture <- model$config$architectures
        max_position_embeddings <- model$config$max_position_embeddings
        if (model_architecture == "FunnelForMaskedLM") {
          max_layer <- sum(model$config$block_repeats * model$config$block_sizes)
        } else {
          max_layer <- model$config$num_hidden_layers
        }
      } else {
        model_architecture <- NULL
        max_position_embeddings <- NULL
        max_layer <- NULL
      }
      return(list(model_architecture, max_position_embeddings, max_layer))
    })

    # Create UI Main Page-----------------------------------------------------------------
    # Card Header
    output$path_to_base_model <- shiny::renderText({
        path_to_base_model()
    })

    # Card body
    output$lm_interface_setting <- shiny::renderUI({
      if (length(interface_architecture()[[2]]) > 0) {
        max_layer_transformer <- interface_architecture()[[3]]

        if (interface_architecture()[[1]] == "FunnelForMaskedLM" |
          interface_architecture()[[1]] == "FunnelModel") {
          pool_type_choices <- c("cls")
        } else {
          pool_type_choices <- c("average", "cls")
        }

        ui <- shiny::tagList(
            shiny::sliderInput(
              inputId = ns("lm_chunks"),
              label = "N Chunks",
              value = 1,
              min = 1,
              max = 50,
              step = 1
            ),
            shiny::sliderInput(
              inputId = ns("lm_max_length"),
              label = paste("Maximal Sequence Length", "(Max:", interface_architecture()[2], ")"),
              value = interface_architecture()[[2]],
              min = 20,
              max = interface_architecture()[[2]],
              step = 1
            ),
            shiny::sliderInput(
              inputId = ns("lm_overlap"),
              label = paste("N Token Overlap", "(Max:", interface_architecture()[2], ")"),
              value = 0,
              min = 0,
              max = interface_architecture()[[2]],
              step = 1
            ),
            shiny::sliderInput(
              inputId = ns("lm_emb_layers"),
              label = "Layers for Embeddings",
              value = c(
                max(1, floor(0.5 * max_layer_transformer)),
                max(1, floor(2 / 3 * max_layer_transformer))
              ),
              min = 1,
              max = max_layer_transformer,
              step = 1
            ),
            shiny::selectInput(
              inputId = ns("lm_emb_pool_type"),
              label = paste("Pooling Type"),
              choices = pool_type_choices,
              multiple = FALSE
            )

        )
        return(ui)
      } else {
        return(NULL)
      }
    })

    # Save the model to disk----------------------------------------------------
    shiny::observeEvent(input$save_modal_button_continue, {
      model_architecture <- interface_architecture()[[1]]
      if (model_architecture == "BertForMaskedLM" |
        model_architecture == "BertModel") {
        method <- "bert"
      } else if (model_architecture == "FunnelForMaskedLM" |
        model_architecture == "FunnelModel") {
        method <- "funnel"
      } else if (model_architecture == "LongformerForMaskedLM" |
        model_architecture == "LongformerModel") {
        method <- "longformer"
      } else if (model_architecture == "RobertaForMaskedLM" |
        model_architecture == "RobertaModel") {
        method <- "roberta"
      } else if (model_architecture == "DebertaV2ForMaskedLM" |
        model_architecture == "DebertaV2Model") {
        method <- "deberta_v2"
      }

      # Check for errors
      errors <- check_errors_text_embedding_model_create(
        destination_path = input$save_modal_directory_path,
        folder_name = input$save_modal_folder_name,
        path_to_base_model=path_to_base_model(),
        interface_architecture=interface_architecture()
      )


      if (length(errors) == 0) {
        shinyWidgets::show_alert(
          title = "Working",
          text = "Please wait",
          type = "info"
        )

        new_interface <- TextEmbeddingModel$new()
        new_interface$configure(
          model_name = input$lm_model_name,
          model_label = input$lm_model_label,
          model_language = input$lm_model_language,
          max_length = input$lm_max_length,
          overlap = input$lm_overlap,
          chunks = input$lm_chunks,
          emb_layer_min = input$lm_emb_layers[1],
          emb_layer_max = input$lm_emb_layers[2],
          emb_pool_type = input$lm_emb_pool_type,
          ml_framework = "pytorch",
          model_dir = path_to_base_model(),
          method = method
        )

        save_to_disk(
          object = new_interface,
          dir_path = input$save_modal_directory_path,
          folder_name = input$save_modal_folder_name
        )
        rm(new_interface)
        gc()
        shinyWidgets::closeSweetAlert()
        shiny::removeModal()
      } else {
        display_errors(
          title = "Error",
          size = "l",
          easy_close = TRUE,
          error_messages = errors
        )
      }
    })

    # Error handling-----------------------------------------------------------
    shiny::observe({
      if (!identical(path_to_base_model(), character(0))) {
        if (is.null(interface_architecture()[[1]]) &
          is.null(interface_architecture()[[2]])) {
          display_errors(
            title = "Error",
            size = "l",
            easy_close = TRUE,
            error_messages = "There is no model to load in the directory."
          )
        }
      }
    })

    #--------------------------------------------------------------------------
  })
}


