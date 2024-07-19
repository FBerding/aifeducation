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
