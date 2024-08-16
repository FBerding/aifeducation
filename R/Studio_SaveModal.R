#' Show save modal
#'
#' Function for displaying a modal that reports errors to the user.
#'
#' @param id `string` Namespace id for the input and output elements.
#' @param ns `function` for setting the correct namespace of the input and output elements.
#' @param title `string` Title of the modal.
#' @param size `string` Size of the modal. Possible are `"m"`, `"s"`, `"l"`, and `"xl"`.
#' @param easy_close `bool` If `TRUE`, the modal dialog can be dismissed by clicking
#' outside the dialog box, or be pressing the Escape key. If `FALSE` the modal
#' must be dismissed by clicking on a modalButton or from a call removeModal on the server.
#'
#' @details
#' If `ns` is `NULL` the function uses the argument `id`. In all other cases `ns` is used
#' for setting the correct ids and namespaces. The application of `id` or `ns` depends on the position
#' of the app where the modal should be used.
#'
#' The events have to be implemented manually in the corresponding server function.
#'
#' @return Function returns a shiny modal.
#'
#' @family studio_modals
#' @keywords internal
#'
create_save_modal <- function(id,
                              ns=NULL,
                              title = "Choose Destination",
                              easy_close = FALSE,
                              size = "l") {
  if(!is.null(ns)){
    ui <- shiny::tagList(
      shiny::textInput(
        inputId = ns("save_modal_directory_path"),
        label = shiny::tags$p(shiny::icon("folder"), "Directory")
      ),
      shiny::textInput(
        inputId = ns("save_modal_folder_name"),
        label = shiny::tags$p(shiny::icon("file"), "Folder Name")
      ),
      shiny::actionButton(
        inputId = ns("save_modal_button_continue"),
        label = "Continue",
        icon = shiny::icon("paper-plane")
      )
    )
  } else {
    ui <- shiny::tagList(
      shiny::textInput(
        inputId = shiny::NS(id,"save_modal_directory_path"),
        label = shiny::tags$p(shiny::icon("folder"), "Directory")
      ),
      shiny::textInput(
        inputId = shiny::NS(id,"save_modal_folder_name"),
        label = shiny::tags$p(shiny::icon("file"), "Folder Name")
      ),
      shiny::actionButton(
        inputId = shiny::NS(id,"save_modal_button_continue"),
        label = "Continue",
        icon = shiny::icon("paper-plane")
      )
    )
  }


  modal <- shiny::modalDialog(
    title = title,
    easyClose = easy_close,
    size = size,
    ui,
    footer = shiny::modalButton("Cancel")
  )
  return(modal)
}




