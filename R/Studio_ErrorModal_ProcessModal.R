display_errors=function(
  title="Error",
  size="l",
  easy_close=TRUE,
  error_messages
  ){

  error_modal <- shiny::modalDialog(
    title = title,
    size = size,
    easyClose = easy_close,
    footer = shiny::modalButton("Close"),
    shiny::tagList(error_messages)
  )
  shiny::showModal(error_modal)
}

display_processing=function(
    title="Working. Please wait.",
    size="l",
    easy_close=FALSE,
    message=""
){

  processing_modal <- shiny::modalDialog(
    title = title,
    size = size,
    easyClose = easy_close,
    footer = shiny::modalButton("Close"),
    shiny::tagList(message)
  )
  shiny::showModal(processing_modal)

  #Give system time to display the modal
  Sys.sleep(2)
}
