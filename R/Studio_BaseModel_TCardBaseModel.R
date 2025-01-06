#' @title Graphical user interface for displaying base model's path.
#' @description Functions generates the tab within a page for displaying base model's path.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_base_model_train_card_base_model
#' @keywords internal
#' @noRd
#'
BaseModel_UI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page(
    shinyFiles::shinyDirButton(
      id = ns("button_select_model"),
      label = "Choose a Base Model",
      title = "Please choose a folder",
      icon = shiny::icon("folder-open")
    ),
    shiny::textInput(
      inputId = ns("model_dir_path"),
      label = shiny::tags$p(shiny::icon("folder"), "Path to the created Base Model"),
      width = "100%"
    )
  )
}

#' @title Server function for: graphical user interface for base model's path.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param volumes `vector` containing a named vector of available volumes.
#' @return Returns errors or `list` with the base model parameters.
#'
#' @family studio_gui_base_model_train_card_base_model
#' @keywords internal
#' @noRd
#'
BaseModel_Server <- function(id, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    # Control Panel --------------------------------------------------------------
    shinyFiles::shinyDirChoose(
      input = input,
      id = "button_select_model",
      roots = volumes,
      allowDirCreate = FALSE
    )
    shiny::observeEvent(input$button_select_model, {
      path <- shinyFiles::parseDirPath(volumes, input$button_select_model)
      shiny::updateTextInput(
        inputId = "model_dir_path",
        value = path
      )
    })

    model_architecture_reactive <- shiny::reactive({
      errors <- c()

      if (input$model_dir_path == "") {
        errors <- append(errors, "Please specify a directory path to the created model.")
      } else if (!dir.exists(input$model_dir_path)) {
        errors <- append(errors, paste(
          "Path to the directory with a base model is not valid - there is no such directory path",
          dQuote(input$model_dir_path)
        ))
      }

      if (length(errors) != 0) {
        class(errors) <- "errors"
        return(errors)
      }

      print(input$model_dir_path)

      shinyWidgets::show_alert(
        title = "Loading",
        text = "Please wait",
        type = "info",
        closeOnClickOutside = FALSE,
        showCloseButton = FALSE
      )

      model_exists <- FALSE
      model_path <- input$model_dir_path
      model_architecture <- NULL
      max_position_embeddings <- NULL

      h5_exists <- file.exists(paste0(model_path, "/", "tf_model.h5"))
      bin_exists <- file.exists(paste0(model_path, "/", "pytorch_model.bin"))
      safetensors_exists <- file.exists(paste0(model_path, "/", "model.safetensors"))

      if (h5_exists || bin_exists || safetensors_exists) {
        model_exists <- TRUE

        if (h5_exists) {
          model <- transformers$TFAutoModel$from_pretrained(model_path)
        } else {
          model <- transformers$AutoModel$from_pretrained(model_path)
        }
        model_architecture <- model$config$architectures
        max_position_embeddings <- model$config$max_position_embeddings
      }
      shinyWidgets::closeSweetAlert()

      params <- list(
        model_exists = model_exists,
        model_dir_path = model_path,
        model_architecture = model_architecture,
        max_position_embeddings = max_position_embeddings
      )

      class(params) <- "params"
      return(params)
    })

    return(model_architecture_reactive)
  })
}
