#' @title Show error modal
#' @description Function for displaying a modal that reports errors to the user.
#'
#' @param title `string` Title of the modal.
#' @param size `string` Size of the modal. Possible are `"m"`, `"s"`, `"l"`, and `"xl"`.
#' @param easy_close `bool` If `TRUE`, the modal dialog can be dismissed by clicking outside the dialog box, or be
#'   pressing the Escape key. If `FALSE` the modal must be dismissed by clicking on a modalButton or from a call
#'   removeModal on the server.
#' @param error_messages `shiny::tagList` containing the html elements to display on the modal.
#'
#' @return Function does nothing return. It displays a modal.
#'
#' @family studio_modals
#' @keywords internal
#'
display_errors <- function(
    title = "Error",
    size = "l",
    easy_close = TRUE,
    error_messages) {
  error_modal <- shiny::modalDialog(
    title = title,
    size = size,
    easyClose = easy_close,
    footer = shiny::modalButton("Close"),
    shiny::tagList(error_messages)
  )
  shiny::showModal(error_modal)
}

#' @title Show processing modal
#' @description Function for displaying a modal that reports to the user that the app is currently working.
#'
#' @param title `string` Title of the modal.
#' @param size `string` Size of the modal. Possible are `"m"`, `"s"`, `"l"`, and `"xl"`.
#' @param easy_close `bool` If `TRUE`, the modal dialog can be dismissed by clicking outside the dialog box, or be
#'   pressing the Escape key. If `FALSE` the modal must be dismissed by clicking on a modalButton or from a call
#'   removeModal on the server.
#' @param message `shiny::tagList` containing the html elements to display on the modal.
#'
#' @return Function does nothing return. It displays a modal.
#'
#' @family studio_modals
#' @keywords internal
#'
display_processing <- function(
    title = "Working. Please wait.",
    size = "l",
    easy_close = FALSE,
    message = "") {
  processing_modal <- shiny::modalDialog(
    title = title,
    size = size,
    easyClose = easy_close,
    footer = shiny::modalButton("Close"),
    shiny::tagList(message)
  )
  shiny::showModal(processing_modal)

  # Give system time to display the modal
  Sys.sleep(2)
}
