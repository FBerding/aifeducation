TextEmbeddingModel_Use_UI <- function(id) {
  tagList(
    bslib::page_sidebar(
      # Sidebar------------------------------------------------------------------
      sidebar = bslib::sidebar(
        position = "left",
        tags$h3("Control Panel"),
        shinyFiles::shinyDirButton(
          id = shiny::NS(id, "button_select_model"),
          label = "Choose a Model",
          title = "Please choose a folder",
          icon = shiny::icon("folder-open")
        ),
        shiny::uiOutput(outputId = shiny::NS(id, "sidebar_description"))
      ),
      # Main panel---------------------------------------------------------------
      bslib::navset_card_underline(
        bslib::nav_panel(
          title = "Description",
          Description_UI(id=shiny::NS(id,"TextEmbeddingModel_Desc"))
        ),
        bslib::nav_panel(
          title = "Training",
          Training_UI(id=shiny::NS(id,"TextEmbeddingModel_Training"))
        ),
        bslib::nav_panel(
          title = "Fill-Mask",
          Fill_Mask_UI(id=shiny::NS(id,"TextEmbeddingModel_Fill_Mask"))
        ),
        bslib::nav_panel(
          title = "Tokenize/Encode/Decode",
          Tokenize_Encode_Decode_UI(id=shiny::NS(id,"TextEmbeddingModel_TokEnDe"))
        ),
        bslib::nav_panel(
          title = "Embedd Text",
          Embed_UI(id=shiny::NS(id,"TextEmbeddingModel_Embed"))
        )
      )
    )
  )
}

TextEmbeddingModel_Use_Server <- function(id, log_dir, volumes) {
  moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    # File system management----------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_model",
      roots = volumes,
      allowDirCreate = FALSE
    )

    #Load the model and check for errors during loading-------------------------
    model_path=shiny::eventReactive(input$button_select_model, {
      model_path <- shinyFiles::parseDirPath(volumes, input$button_select_model)
      if (length(model_path) > 0) {
        return(model_path)
      } else {
        return(NULL)
      }
    })

    model <- shiny::eventReactive(input$button_select_model, {
      # Get model path
      model_path <- shinyFiles::parseDirPath(volumes, input$button_select_model)
      if (length(model_path) > 0) {
        display_processing(
          title="Working. Please wait.",
          size="l",
          easy_close=FALSE,
          message=""
        )

        #Try to load the model
        model <- try(load_from_disk(model_path),silent = TRUE)

        if ("try-error"%in%class(model) == FALSE) {
          if ("TextEmbeddingModel" %in% class(model)) {
            shiny::removeModal()
            return(model)
            } else {
              display_errors(
                title = "Error",
                size = "l",
                easy_close = TRUE,
                error_messages = "The file does not contain an object of class TextEmbeddingModel."
              )
            return(NULL)
          }
        } else {
          display_errors(
            title = "Error",
            size = "l",
            easy_close = TRUE,
            error_messages = model
          )
          return(NULL)
        }
      }
    })

    #Render the information at the sidebar--------------------------------------
    output$sidebar_description<-shiny::renderUI({
      shiny::req(model())
      return(
        generate_sidebar_information(model())
      )
    })

    #Server functions for the different tabs------------------------------------
    Description_Server(id="TextEmbeddingModel_Desc",
                       model=model)
    Training_Server(id="TextEmbeddingModel_Training",
                    model=model)
    Fill_Mask_Server(id="TextEmbeddingModel_Fill_Mask",
                    model=model)
    Tokenize_Encode_Decode_Server(id="TextEmbeddingModel_TokEnDe",
                                  model=model)
    Embed_Server(id="TextEmbeddingModel_Embed",
                 model=model,
                 volumes=volumes,
                 model_path = model_path(),
                 log_dir = log_dir)
    #--------------------------------------------------------------------------
  })
}
