start_studio_new <- function() {
  # shiny::devmode()
  base::requireNamespace("future")
  base::requireNamespace("shiny")
  base::requireNamespace("bslib")
  future::plan(future::multisession)
  ui <- bslib::page_navbar(
    title = "AI for Education - Studio",
    theme = bslib::bs_theme(bootswatch = "darkly"),
    nav_panel(
      title = "Data Management",
      DataManagement_RawTextsUI("DataSetRawTexts")
    ),
    nav_panel(
      title = "Base Models",
      navset_underline(
        nav_panel(
          title = "Create"
        ),
        nav_panel(
          title = "Train"
        )
      )
    ),
    nav_panel(
      title = "TextEmbeddingModels",
      navset_tab(
        nav_panel(
          title = "Create",
          TextEmbeddingModel_Create_UI("TextEmbeddingModel_Create")
        ),
        nav_panel(
          title = "Use",
          TextEmbeddingModel_Use_UI("TextEmbeddingModel_Use")
        ),
        nav_panel(
          title = "Document",
          DocumentPage_UI("TextEmbeddingModel_Document")
        )
      )
    ),
    nav_panel(
      title = "FeatureExtractors",
      navset_tab(
        nav_panel(
          title = "Create",
          FeatureExtractors_Create_UI("FeatureExtractors_Create")
        ),
        nav_panel(
          title = "Use",
          TextEmbeddingModel_Use_UI("FeatureExtractors_Use")
        ),
        nav_panel(
          title = "Document",
          DocumentPage_UI("FeatureExtractors_Document")
        )
      )
    ),
    nav_panel(
      title = "Classifiers",
      navset_tab(
        nav_panel(
          title = "Create",
          Classifiers_Create_UI("Classifiers_Create")
        ),
        nav_panel(
          title = "Use",
          Classifiers_Use_UI("Classifiers_Use")
        ),
        nav_panel(
          title = "Document",
          DocumentPage_UI("Classifiers_Document")
        )
      )
    )

    # DataSetUI
    # BaseModels_CreateUI
    # BaseModels_TrainUI

    # TextEmbeddingModel_CreateUI
    # TextEmbeddingModel_UseUI
    # TextEmbeddingModel_DocumentUI

    # TEClassifier_CreateUI
    # TEClassifier_UseUI
    # TEClassifier_DocumentUI
  )

  server <- function(input, output, session) {
    #Set up global variables----------------------------------------------------
    log_dir=getwd()
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())

    #Functions
    DataManagement_RawTextsServer(
      id="DataSetRawTexts",
      log_dir=log_dir,
      volumes=volumes)

    #TextEmbeddingModels
    TextEmbeddingModel_Create_Server(
      id="TextEmbeddingModel_Create",
      log_dir=log_dir,
      volumes=volumes)
    TextEmbeddingModel_Use_Server(
      id="TextEmbeddingModel_Use",
      log_dir=log_dir,
      volumes=volumes
      )
    DocumentPage_Server(
      id="TextEmbeddingModel_Document",
      volumes=volumes,
      type = "TextEmbeddingModel"
    )

    #FeatureExtractors
    DocumentPage_Server(
      id="FeatureExtractors_Document",
      volumes=volumes,
      type = "FeatureExtractors"
    )

    #Classifiers
    Classifiers_Create_Server(
      id="Classifiers_Create",
      log_dir=log_dir,
      volumes=volumes)

    Classifiers_Use_Server(
      id="Classifiers_Use",
      log_dir=log_dir,
      volumes=volumes
      )
    DocumentPage_Server(
      id="Classifiers_Document",
      volumes=volumes,
      type = "FeatureExtractors"
    )


  }
  shinyApp(ui = ui, server = server)
}
