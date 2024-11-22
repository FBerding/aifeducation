# Create ui-------------------------------------------------------------------
ui <- bslib::page_navbar(
  title = "AI for Education - Studio",
  theme = bslib::bs_theme(
    bootswatch = "flatly"
  ),
  bslib::nav_panel(
    title = "Home",
    aifeducation:::Studio_Home_UI("Home")
  ),
  bslib::nav_panel(
    title = "Data Management",
    bslib::navset_tab(
      bslib::nav_panel(
        title = "DataSetExplorer",
        aifeducation:::DataManagement_DataSetEditorUI("DataSetExplorer")
      ),
      bslib::nav_panel(
        title = "LargeDataSet Creator",
        aifeducation:::DataManagement_RawTextsUI("DataSetRawTexts")
      ),
      bslib::nav_panel(
        title = "Table Editor",
        aifeducation:::DataManagement_TableEditorUI("TableEditor")
      )
    )
  ),
  bslib::nav_panel(
    title = "Base Models",
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        position = "left",
        shiny::tags$h4("Sustainability Tracking"),
        shinyWidgets::materialSwitch(
          inputId = "is_sustainability_tracked",
          label = "Activate",
          value = TRUE,
          right = TRUE,
          status = "success"
        ),
        shiny::selectInput(
          inputId = "sustainability_country",
          label = "Country",
          choices = country_alpha_3_list,
          selected = "DEU"
        )
      ),
      bslib::navset_underline(
        bslib::nav_panel(
          title = "Create",
          aifeducation:::BaseModel_Create_UI("BaseModel_Create")
        ),
        bslib::nav_panel(
          title = "Train",
          aifeducation:::BaseModel_Train_UI("BaseModel_Train")
        )
      )
    )
  ),
  bslib::nav_panel(
    title = "TextEmbeddingModels",
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Use",
        aifeducation:::TextEmbeddingModel_Use_UI("TextEmbeddingModel_Use")
      ),
      bslib::nav_panel(
        title = "Create",
        aifeducation:::TextEmbeddingModel_Create_UI("TextEmbeddingModel_Create")
      ),
      bslib::nav_panel(
        title = "Document",
        aifeducation:::DocumentPage_UI("TextEmbeddingModel_Document", type = "TextEmbeddingModel")
      )
    )
  ),
  bslib::nav_panel(
    title = "FeatureExtractors",
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Use",
        aifeducation:::FeatureExtractors_Use_UI("FeatureExtractors_Use")
      ),
      bslib::nav_panel(
        title = "Create",
        aifeducation:::FeatureExtractors_Create_UI("FeatureExtractors_Create")
      ),
      bslib::nav_panel(
        title = "Document",
        aifeducation:::DocumentPage_UI("FeatureExtractors_Document", type = "FeatureExtractors")
      )
    )
  ),
  bslib::nav_panel(
    title = "Classifiers",
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Use",
        aifeducation:::Classifiers_Use_UI("Classifiers_Use")
      ),
      bslib::nav_panel(
        title = "Create",
        aifeducation:::Classifiers_Create_UI("Classifiers_Create")
      ),
      bslib::nav_panel(
        title = "Document",
        aifeducation:::DocumentPage_UI("Classifiers_Document", type = "Classifiers")
      )
    )
  ),
  bslib::nav_panel(
    title = "License",
    aifeducation:::License_UI("GPL_3_License")
  ),
  bslib::nav_menu(
    title = "Other",
    bslib::nav_item(
      shiny::uiOutput(outputId = "ui_gpu_acceleration")
    )
  )
)

# Server----------------------------------------------------------------------
server <- function(input, output, session) {
  # Set up global variables----------------------------------------------------
  log_dir <- getwd()
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())

  # DataMangement
  aifeducation:::DataManagement_RawTextsServer(
    id = "DataSetRawTexts",
    log_dir = log_dir,
    volumes = volumes
  )
  aifeducation:::DataManagement_DataSetEditorServer(
    id = "DataSetExplorer",
    log_dir = log_dir,
    volumes = volumes
  )
  aifeducation:::DataManagement_TableEditorServer(
    id = "TableEditor",
    log_dir = log_dir,
    volumes = volumes
  )

  # BaseModels

  sustain_tracking <- shiny::reactive({
    return(
      list(
        is_sustainability_tracked = input$is_sustainability_tracked,
        sustainability_country = input$sustainability_country
      )
    )
  })

  aifeducation:::BaseModel_Create_Server(
    id = "BaseModel_Create",
    log_dir = log_dir,
    volumes = volumes,
    sustain_tracking = sustain_tracking
  )
  aifeducation:::BaseModel_Train_Server(
    id = "BaseModel_Train",
    log_dir = log_dir,
    volumes = volumes,
    sustain_tracking = sustain_tracking
  )

  # TextEmbeddingModels
  aifeducation:::TextEmbeddingModel_Create_Server(
    id = "TextEmbeddingModel_Create",
    log_dir = log_dir,
    volumes = volumes
  )
  aifeducation:::TextEmbeddingModel_Use_Server(
    id = "TextEmbeddingModel_Use",
    log_dir = log_dir,
    volumes = volumes
  )
  aifeducation:::DocumentPage_Server(
    id = "TextEmbeddingModel_Document",
    volumes = volumes,
    type = "TextEmbeddingModel"
  )

  # FeatureExtractors
  aifeducation:::FeatureExtractor_Create_Server(
    id = "FeatureExtractors_Create",
    log_dir = log_dir,
    volumes = volumes
  )
  aifeducation:::FeatureExtractors_Use_Server(
    id = "FeatureExtractors_Use",
    log_dir = log_dir,
    volumes = volumes
  )
  aifeducation:::DocumentPage_Server(
    id = "FeatureExtractors_Document",
    volumes = volumes,
    type = "FeatureExtractors"
  )

  # Classifiers
  aifeducation:::Classifiers_Create_Server(
    id = "Classifiers_Create",
    log_dir = log_dir,
    volumes = volumes
  )

  aifeducation:::Classifiers_Use_Server(
    id = "Classifiers_Use",
    log_dir = log_dir,
    volumes = volumes
  )
  aifeducation:::DocumentPage_Server(
    id = "Classifiers_Document",
    volumes = volumes,
    type = "Classifier"
  )

  # License
  aifeducation:::License_Server(
    "GPL_3_License"
  )

  # GPU Acceleration
  output$ui_gpu_acceleration <- shiny::renderUI({
    if (aifeducation:::torch$cuda$is_available()) {
      ui <- shiny::tagList(
        shiny::icon("bolt-lightning"),
        "GPU acceleration available."
      )
    } else {
      ui <- shiny::tagList(
        shiny::icon("xmark"),
        "GPU acceleration not available."
      )
    }
    return(ui)
  })
}

shiny::shinyApp(ui = ui, server = server)
