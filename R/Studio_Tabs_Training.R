Training_UI<-function(id){
  bslib::page(
    bslib::page_sidebar(
      sidebar=bslib::sidebar(
        position = "right",
        shiny::sliderInput(inputId = shiny::NS(id,"text_size"),
                           label = "Text Size",
                           min = 1,
                           max = 20,
                           step = 0.5,
                           value = 12),
        shiny::numericInput(inputId = shiny::NS(id,"y_min"),
                            label = "Y Min",
                            value = 0),
        shiny::numericInput(inputId = shiny::NS(id,"y_max"),
                            label = "Y Max",
                            value = 20)
      ),
      shiny::plotOutput(
        outputId = shiny::NS(id,"training_plot")
      )
    )
  )
}





Training_Server=function(id,model){
  moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    ns <- session$ns

    # plot------------------------------------------------
    output$training_plot<-shiny::renderPlot({
      shiny::req(model)
      plot_data=model()$last_training$history

      #if(!is.null(plot_data)){
        y_min=input$y_min
        y_max=input$y_max

        val_loss_min=min(plot_data$val_loss)
        best_model_epoch=which(x=(plot_data$val_loss)==val_loss_min)

        plot<-ggplot2::ggplot(data=plot_data)+
          ggplot2::geom_line(ggplot2::aes(x=.data$epoch,y=.data$loss,color="train"))+
          ggplot2::geom_line(ggplot2::aes(x=.data$epoch,y=.data$val_loss,color="validation"))+
          ggplot2::geom_vline(xintercept = best_model_epoch,
                              linetype="dashed")

        plot=plot+ggplot2::theme_classic()+
          ggplot2::ylab("value")+
          ggplot2::coord_cartesian(ylim=c(y_min,y_max))+
          ggplot2::xlab("epoch")+
          ggplot2::scale_color_manual(values = c("train"="red",
                                                 "validation"="blue",
                                                 "test"="darkgreen"))+
          ggplot2::theme(text = ggplot2::element_text(size = input$text_size),
                         legend.position="bottom")
        return(plot)
      #} else {
      #  return(NULL)
      #}
    },res = 72*2)
  })
}
