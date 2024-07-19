start_studio_new <- function() {
  #shiny::devmode()
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
      page_navbar(
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
      page_navbar(
        nav_panel(
          title = "Create"
        ),
        nav_panel(
          title = "Use"
        ),
        nav_panel(
          title = "Document"
        )
      )
    ),
    nav_panel(
      title = "Classification",
      page_navbar(
        nav_panel(
          title = "Create"
        ),
        nav_panel(
          title = "Use"
        ),
        nav_panel(
          title = "Document"
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
    DataManagement_RawTextsServer("DataSetRawTexts")
  }
  shinyApp(ui = ui, server = server)
}
