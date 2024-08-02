generate_sidebar_information <- function(model) {
  # Prepare output
  if (is.null(model)) {
    model_label <- NULL
  } else {
    model_label <- model$get_model_info()$model_label
  }

  max_tokens <- (model$get_model_info()$model_max_size - model$get_transformer_components()$overlap) * model$get_transformer_components()$chunks + model$get_model_info()$model_max_size

  if (!is.null(model$get_transformer_components()$aggregation)) {
    aggegation <- shiny::tags$p("Hidden States Aggregation: ", model$get_transformer_components()$aggregation)
  } else {
    aggegation <- NULL
  }

  if (!is.null(model$get_transformer_components()$emb_pool_type)) {
    pool_type <- model$get_transformer_components()$emb_pool_type
    min_layer <- model$get_transformer_components()$emb_layer_min
    max_layer <- model$get_transformer_components()$emb_layer_max
  } else {
    pool_type <- NULL
    min_layer <- NULL
    max_layer <- NULL
  }

  if (methods::isClass(Class = "data.frame", where = model$get_sustainability_data())) {
    if (is.na(model$get_sustainability_data()[1, 1]) == FALSE) {
      kwh <- round(sum(model$get_sustainability_data()[, "sustainability_data.total_energy_kwh"]), 3)
    } else {
      kwh <- "not estimated"
    }
  } else {
    kwh <- "not estimated"
  }

  if (methods::isClass(Class = "data.frame", where = model$get_sustainability_data())) {
    if (is.na(model$get_sustainability_data()[1, 1]) == FALSE) {
      co2 <- round(sum(model$get_sustainability_data()[, "sustainability_data.co2eq_kg"]), 3)
    } else {
      co2 <- "not estimated"
    }
  } else {
    co2 <- "not estimated"
  }

  ui <- shiny::tagList(
    shiny::tags$p(shiny::tags$b("Model:")),
    shiny::tags$p(model_label),
    shiny::tags$hr(),
    shiny::tags$p("Method: ", model$get_model_info()$model_method),
    shiny::tags$p("Max Tokens per Chunk: ", model$get_model_info()$model_max_size),
    shiny::tags$p("Max Chunks: ", model$get_transformer_components()$chunks),
    shiny::tags$p("Token Overlap: ", model$get_transformer_components()$overlap),
    shiny::tags$p("Max Tokens: ", max_tokens),
    shiny::tags$p("Hidden States Aggregation: ", aggegation),
    shiny::tags$p("Pool Type: ", pool_type),
    shiny::tags$p("Embedding Layers - Min: ", min_layer),
    shiny::tags$p("Embedding Layers - Max: ", max_layer),
    shiny::tags$hr(),
    shiny::tags$p("Energy Consumption (kWh): ", kwh),
    shiny::tags$p("Carbon Footprint (CO2eq. kg): ", co2)
  )
  return(ui)
}



generate_model_description <- function(model, eng) {
  if (!is.null(model)) {
    if (eng == TRUE) {
      ui <- shiny::tagList(
        shiny::tags$h3("Abstract"),
        if (!is.null(model$get_model_description()$abstract_eng)) {
          shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$abstract_eng))
        },
        shiny::tags$h3("Description"),
        if (!is.null(model$get_model_description()$eng)) {
          shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$eng))
        }
      )
    } else {
      ui <- shiny::tagList(
        shiny::tags$h3("Abstract"),
        if (!is.null(model$get_model_description()$abstract_native)) {
          shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$abstract_native))
        },
        shiny::tags$h3("Description"),
        if (!is.null(model$get_model_description()$native)) {
          shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$native))
        }
      )
    }
    return(ui)
  } else {
    return(NULL)
  }
}

generate_model_bib_description <- function(model) {

  pub_info=model$get_publication_info()
  ui <- shiny::tagList(
    if (!is.null(pub_info$developed_by$authors)) {
      shiny::tags$p("Developers: ", paste(
        format(
          x = pub_info$developed_by$authors,
          include = c("given", "family")
        ),
        collapse = ", "
      ))
    },
    if (!is.null(pub_info$developed_by$citation)) {
      shiny::tags$p("Citation: ", pub_info$developed_by$citation)
    },
    if (!is.null(pub_info$modifided_by$authors)) {
      shiny::tags$p("Modifiers: ", paste(
        format(
          x = pub_info$modifided_by$authors,
          include = c("given", "family")
        ),
        collapse = ", "
      ))
    },
    if (!is.null(pub_info$modifided_by$citation)) {
      shiny::tags$p("Citation: ", pub_info$modifided_by$citation)
    },
    if (!is.null(pub_info$modifided_by$citation)) {
      shiny::tags$p("Language: ", get_model_info()$model_language)
    },
  )

  return(ui)
}

generate_doc_input_developers <- function(ns, model, type = "developers") {
  if (type == "developers") {
    pup_info_for <- "developed_by"
    pup_info_titles <- "Developers"
  } else if (type == "modifiers") {
    pup_info_for <- "modified_by"
    pup_info_titles <- "Modifiers"
  }

  widgets <- NULL
  for (j in 1:10) {
    pup_info <- model$get_publication_info()[[pup_info_for]]$authors
    widgets[[j]] <- list(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::textInput(
            inputId = ns(paste0("doc_", pup_info_titles, "_fist_name_", j)),
            label = paste("Given Name", j),
            value = pup_info[[j]]$given,
            width = "100%"
          )
        ),
        shiny::column(
          width = 4,
          shiny::textInput(
            inputId = ns(paste0("doc_", pup_info_titles, "_last_name_", j)),
            label = paste("Family Name", j),
            value = pup_info[[j]]$family,
            width = "100%"
          )
        ),
        shiny::column(
          width = 4,
          shiny::textInput(
            inputId = ns(paste0("doc_", pup_info_titles, "_mail_", j)),
            label = paste("Mail", j),
            value = pup_info[[j]]$email,
            width = "100%"
          )
        )
      )
    )
  }

  ui <- shiny::tagList(
    shiny::tabPanel(
      title = pup_info_titles,
      shiny::textInput(
        inputId = ns(paste0("doc_", pup_info_for, "_citation")),
        label = "Citation",
        value = model$get_publication_info()[[pup_info_for]]$citation
      ),
      shiny::textInput(
        inputId = ns(paste0("doc_", pup_info_for, "_url")),
        label = "URL",
        value = model$get_publication_info()[[pup_info_for]]$url
      ),
      widgets,
      shiny::actionButton(
        inputId = ns(paste0("doc_", pup_info_for, "_save")),
        label = "Save",
        icon = shiny::icon("floppy-disk")
      )
    )
  )
  return(ui)
}


generate_doc_input_text_editor <- function(ns, model, language = "eng", type = "abstract") {
  if (language == "eng") {
    if (type == "abstract") {
      documention_title <- "Abstract English"
      documentation_keyword <- "keywords_eng"
      documention_part <- "abstract_eng"
      documentation_field <- "abstract_eng"
    } else {
      documention_title <- "Description English"
      documention_part <- "description_eng"
      documentation_field <- "eng"
    }
  } else {
    if (type == "abstract") {
      documention_title <- "Abstract Native"
      documentation_keyword <- "keywords_native"
      documention_part <- "abstract_native"
      documentation_field <- "abstract_native"
    } else {
      documention_title <- "Description Native"
      documention_part <- "description_native"
      documentation_field <- "native"
    }
  }

  ui <- shiny::tagList(
    bslib::layout_column_wrap(
      bslib::card(
        bslib::card_header("Editor"),
        bslib::card_body(
          shiny::textAreaInput(
            inputId = ns(paste0("doc_editor_", documention_part)),
            label = "Editor",
            rows = 6,
            width = "100%",
            value = model$get_model_description()[[documentation_field]]
          ),
          if (type == "abstract") {
            shiny::textInput(
              inputId = ns(paste0("doc_editor_", documention_part, "_keywords")),
              value = model$get_model_description()[[documentation_keyword]],
              label = "Keywords",
              width = "100%"
            )
          },
          shiny::actionButton(
            inputId = ns(paste0("doc_editor_", documention_part, "_preview_button")),
            label = "Preview",
            icon = shiny::icon("eye")
          ),
          shiny::actionButton(
            inputId = ns(paste0("doc_editor_", documention_part, "_save_button")),
            label = "Save",
            icon = shiny::icon("floppy-disk")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Preview"),
        bslib::card_body(
          shiny::uiOutput(outputId = ns(paste0("doc_editor_", documention_part, "_preview")))
        )
      )
    )
  )
  return(ui)
}

load_and_check_embeddings=function(dir_path){
  if(!is.null(dir_path)){
    if(file.exists(dir_path)==TRUE){
      display_processing(title="Working. Please wait.",
                         size="l",
                         easy_close=FALSE,
                         message="")
      #Wait for modal
      Sys.sleep(1)
      embeddings=load_from_disk(dir_path)
      if(("EmbeddedText" %in% class(embeddings))==TRUE|
         "LargeDataSetForTextEmbeddings" %in%class(embeddings)){
        shiny::removeModal()
        return(embeddings)
      } else {
        shiny::removeModal()
        display_errors(
          title="Error",
          size="l",
          easy_close=TRUE,
          error_messages="The file contains data in an unsupported format.
              Text embeddings must be of class 'LargeDataSetForTextEmbeddings' or 'EmbeddedText'. Please
              check data. Data embeddings should always be created via data
              preparation of this user interfache or with the corresponding
              method of the TextEmbeddingModel."
        )
        rm(embeddings)
        gc()
        return(NULL)
      }
    } else {
      shiny::removeModal()
      display_errors(
        title="Error",
        size="l",
        easy_close=TRUE,
        error_messages="The file does not exist on the path."
      )
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

load_and_check_target_data=function(file_path){
  if(!is.null(file_path)){
    if(file.exists(file_path)==TRUE){
      display_processing(title="Working. Please wait.",
                         size="l",
                         easy_close=FALSE,
                         message="")

      #extension=stringr::str_split_fixed(file_path,pattern="\\.",n=Inf)
      #extension=extension[1,ncol(extension)]
      #extension=stringr::str_to_lower(extension)
      extension=stringi::stri_split_fixed(file_path,pattern=".")[[1]]
      extension=stringi::stri_trans_tolower(extension[[length(extension)]])

      if(extension=="csv"|extension=="txt"){
        target_data=try(
          as.data.frame(
            utils::read.csv(
              file = file_path,
              header = TRUE)),
          silent = TRUE)
      } else if(extension=="xlsx"){
        target_data=try(
          as.data.frame(
            readxl::read_xlsx(
              path=file_path,
              sheet = 1,
              col_names = TRUE)),
          silent=TRUE)
      } else if (extension%in%c("rda","rdata")){
        object_name=load(file = file_path)
        target_data=get(x=object_name)
        target_data=try(
          as.data.frame(target_data),
          silent = TRUE)
      } else {
        target_data=NA
      }

      #Final Check
      if(is.character(target_data)){
        shiny::removeModal()
        display_errors(
          title="Error",
          size="l",
          easy_close=TRUE,
          error_messages="Data can not be loaded as data frame. Please check your data."
        )
        return(NULL)
      } else {
        if("id"%in%colnames(target_data)){
          rownames(target_data)=target_data$id
          shiny::removeModal()
          return(target_data)
        } else {
          shiny::removeModal()
          display_errors(
            title="Error",
            size="l",
            easy_close=TRUE,
            error_messages="Data does not contain a column named 'id'. This
                       column is necessary to match the text embeddings to their
                       corresponding targets. Please check your data."
          )
          return(NULL)
        }
      }
    } else {
      shiny::removeModal()
      display_errors(
        title="Error",
        size="l",
        easy_close=TRUE,
        error_messages="The file does not exist on the path."
      )
      return(NULL)
    }
  } else {
    return(NULL)
  }
}
