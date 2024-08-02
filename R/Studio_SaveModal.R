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




