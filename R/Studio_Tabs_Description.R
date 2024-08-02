Description_UI<-function(id){
  bslib::page(
    bslib::card(
      bslib::card_header(
        "Bibliographic Description"
      ),
      bslib::card_body(
        shiny::uiOutput(outputId = shiny::NS(id,"bibliographic"))
      )
    ),
    bslib::card(
      bslib::card_header(
        "Model Description"
      ),
      bslib::card_body(
        shinyWidgets::switchInput(
          inputId = shiny::NS(id,"language_select"),
          label = "Language",
          onLabel="English",
          offLabel = "Native",
          value = TRUE,
          labelWidth = "80px"
        ),
        shiny::uiOutput(outputId = shiny::NS(id,"description"))
      )
    )
  )
}

Description_Server=function(id,model){
  moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    # Bibliographic Description------------------------------------------------
    output$bibliographic<-shiny::renderUI({
      shiny::req(model())
      return(generate_model_bib_description(model=model()))
    })

    # Model description
    output$description<-shiny::renderUI({
      shiny::req(model())
      return(generate_model_description(model=model(),eng=input$language_select))
    })



  })
}
