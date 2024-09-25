# Create ui-------------------------------------------------------------------
ui <- bslib::page_navbar(
  title = "AI for Education - Studio",
  theme = bslib::bs_theme(
    bootswatch = "flatly"
    ),
  bslib::nav_panel(
    title = "Home",
    Studio_Home_UI("Home")
  ),
  bslib::nav_panel(
    title = "Data Management",
    DataManagement_RawTextsUI("DataSetRawTexts")
  ),
  bslib::nav_panel(
    title = "Base Models",
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Create"
      ),
      bslib::nav_panel(
        title = "Train"
      )
    )
  ),
  bslib::nav_panel(
    title = "TextEmbeddingModels",
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Use",
        TextEmbeddingModel_Use_UI("TextEmbeddingModel_Use")
      ),
      bslib::nav_panel(
        title = "Create",
        TextEmbeddingModel_Create_UI("TextEmbeddingModel_Create")
      ),
      bslib::nav_panel(
        title = "Document",
        DocumentPage_UI("TextEmbeddingModel_Document", type = "TextEmbeddingModel")
      )
    )
  ),
  bslib::nav_panel(
    title = "FeatureExtractors",
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Use",
        FeatureExtractors_Use_UI("FeatureExtractors_Use")
      ),
      bslib::nav_panel(
        title = "Create",
        FeatureExtractors_Create_UI("FeatureExtractors_Create")
      ),
      bslib::nav_panel(
        title = "Document",
        DocumentPage_UI("FeatureExtractors_Document", type = "FeatureExtractors")
      )
    )
  ),
  bslib::nav_panel(
    title = "Classifiers",
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Use",
        Classifiers_Use_UI("Classifiers_Use")
      ),
      bslib::nav_panel(
        title = "Create",
        Classifiers_Create_UI("Classifiers_Create")
      ),
      bslib::nav_panel(
        title = "Document",
        DocumentPage_UI("Classifiers_Document", type = "Classifiers")
      )
    )
  ),
  bslib::nav_panel(
    title = "License",
    License_UI("GPL_3_License")
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

  # Functions
  DataManagement_RawTextsServer(
    id = "DataSetRawTexts",
    log_dir = log_dir,
    volumes = volumes
  )

  # TextEmbeddingModels
  TextEmbeddingModel_Create_Server(
    id = "TextEmbeddingModel_Create",
    log_dir = log_dir,
    volumes = volumes
  )
  TextEmbeddingModel_Use_Server(
    id = "TextEmbeddingModel_Use",
    log_dir = log_dir,
    volumes = volumes
  )
  DocumentPage_Server(
    id = "TextEmbeddingModel_Document",
    volumes = volumes,
    type = "TextEmbeddingModel"
  )

  # FeatureExtractors
  FeatureExtractor_Create_Server(
    id = "FeatureExtractors_Create",
    log_dir = log_dir,
    volumes = volumes
  )
  FeatureExtractors_Use_Server(
    id = "FeatureExtractors_Use",
    log_dir = log_dir,
    volumes = volumes
  )
  DocumentPage_Server(
    id = "FeatureExtractors_Document",
    volumes = volumes,
    type = "FeatureExtractors"
  )

  # Classifiers
  Classifiers_Create_Server(
    id = "Classifiers_Create",
    log_dir = log_dir,
    volumes = volumes
  )

  Classifiers_Use_Server(
    id = "Classifiers_Use",
    log_dir = log_dir,
    volumes = volumes
  )
  DocumentPage_Server(
    id = "Classifiers_Document",
    volumes = volumes,
    type = "Classifier"
  )

  # License
  License_Server(
    "GPL_3_License"
  )

  # GPU Acceleration
  output$ui_gpu_acceleration <- shiny::renderUI({
    if (torch$cuda$is_available()) {
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
