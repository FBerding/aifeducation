# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Generate sidebar
#' @description Function for generating a sidebar containing information on a model.
#'
#' @param model Model for which the sidebar should be generated.
#'
#' @return Returns a `shiny::tagList` containing the html elements for the user interface. The content of the list
#'   depends on the kind of model passed to this function.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
generate_sidebar_information <- function(model) {
  if ("TextEmbeddingModel" %in% class(model)) {
    # Prepare output
    if (is.null(model)) {
      model_label <- NULL
    } else {
      model_label <- model$get_model_info()$model_label
    }

    max_tokens <- (model$get_basic_components()$max_length - model$get_transformer_components()$overlap) * model$get_transformer_components()$chunks + model$get_basic_components()$max_length

    # TODO (Yuliia): remove? Variable is not used
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
      shiny::tags$p("# Parameter: ", model$count_parameter()),
      shiny::tags$p("Method: ", model$get_basic_components()$method),
      shiny::tags$p("Max Tokens per Chunk: ", model$get_basic_components()$max_length),
      shiny::tags$p("Max Chunks: ", model$get_transformer_components()$chunks),
      shiny::tags$p("Token Overlap: ", model$get_transformer_components()$overlap),
      shiny::tags$p("Max Tokens: ", max_tokens),
      shiny::tags$p("Pool Type: ", pool_type),
      shiny::tags$p("Embedding Layers - Min: ", min_layer),
      shiny::tags$p("Embedding Layers - Max: ", max_layer),
      shiny::tags$hr(),
      shiny::tags$p("Energy Consumption (kWh): ", kwh),
      shiny::tags$p("Carbon Footprint (CO2eq. kg): ", co2)
    )
  } else if ("TEClassifierRegular" %in% class(model) ||
    "TEClassifierProtoNet" %in% class(model)) {
    if (is.null(model)) {
      model_label <- NULL
    } else {
      model_label <- model$get_model_info()$model_label
    }

    if (model$get_sustainability_data()$sustainability_tracked == TRUE) {
      kwh <- round(model$get_sustainability_data()$sustainability_data$total_energy_kwh, 3)
      co2 <- round(model$get_sustainability_data()$sustainability_data$co2eq_kg, 3)
    } else {
      kwh <- "not estimated"
      co2 <- "not estimated"
    }


    ui <- shiny::tagList(
      shiny::tags$p(shiny::tags$b("Model:")),
      shiny::tags$p(model_label),
      shiny::tags$hr(),
      shiny::tags$p("# Parameter: ", model$count_parameter()),
      shiny::tags$p("Synthetic Cases: ", model$last_training$config$use_sc),
      shiny::tags$p("Pseudo Labeling: ", model$last_training$config$use_pl),
      shiny::tags$hr(),
      shiny::tags$p("Energy Consumption (kWh): "),
      shiny::tags$p(kwh),
      shiny::tags$p("Carbon Footprint (CO2eq. kg): "),
      shiny::tags$p(co2)
    )
  }



  return(ui)
}


#' @title Generate model description
#' @description Function for generating the html elements describing a model.
#'
#' @param model Model for which the description should be generated.
#' @param eng `bool` If `TRUE` the generation assumes the description to be in English. If `FALSE` it assumes native
#'   language.
#'
#' @return Returns a `shiny::tagList` containing the html elements for the user interface. The content of the list
#'   depends on the kind of model passed to this function. If the `model` is `NULL` function returns `NULL`.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
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

#' @title Generate description of bibliographic information
#' @description Function for generating the html elements reporting a model's bibliographic information.
#'
#' @param model Model for which the description should be generated.
#'
#' @return Returns a `shiny::tagList` containing the html elements for the user interface.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
generate_model_bib_description <- function(model) {
  pub_info <- model$get_publication_info()
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
      shiny::tags$p("Language: ", model$get_model_info()$model_language)
    },
  )

  return(ui)
}

#' @title Generate widgets for documenting the bibliographic information of a model
#' @description Function generates the input widgets for documenting the bibliographic information of  a model. This
#'   includes the names of the involved persons, mail e-mail addresses, urls, and citation.
#'
#' @param ns `function` for setting the namespace of the input elements. This should be `session$ns`.
#' @param model Model for which the description should be generated.
#' @param type `string` determining if the widgets should be generated for documenting the developers (`type =
#'   "developers"`) or the modifiers (`type = "modifiers"`).
#'
#' @return Returns a `shiny::tagList` containing the html elements for the user interface.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
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

#' @title Generate widgets for documenting a model
#' @description Function generates the input widgets for documenting a model.
#'
#' @param ns `function` for setting the namespace of the input elements. This should be `session$ns`.
#' @param model Model for which the description should be generated.
#' @param language `string` determining if the documentation should be saved in English (`language = "eng"`) or in the
#'   model's native language (`language = "native"`).
#' @param type `string` determining if the input refers to the abstract (`type = "abstract"`) or the main documentation
#'   (`type = "documentation"`).
#'
#' @return Returns a `shiny::tagList` containing the html elements for the user interface.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
generate_doc_input_text_editor <- function(ns, model, language = "eng", type = "abstract") {
  # TODO (Yuliia): remove? Variable "documentation_title" is not used
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

#' @title Load and check embeddings
#' @description Function for checking and loading text embeddings in AI for Education - Studio.
#'
#' @param dir_path `string` path to the directory containing the embeddings.
#'
#' @return If there are any errors an error modal is displayed by calling the function [display_errors]. If there are no
#'   errors the function returns embeddings as an object of class [LargeDataSetForTextEmbeddings] or [EmbeddedText]. In
#'   the case of erros the function returns `NULL`.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
load_and_check_embeddings <- function(dir_path) {
  if (!is.null(dir_path)) {
    if (file.exists(dir_path) == TRUE) {
      display_processing(
        title = "Working. Please wait.",
        size = "l",
        easy_close = FALSE,
        message = ""
      )
      # Wait for modal
      Sys.sleep(1)
      embeddings <- load_from_disk(dir_path)
      if (("EmbeddedText" %in% class(embeddings)) == TRUE ||
        "LargeDataSetForTextEmbeddings" %in% class(embeddings)) {
        shiny::removeModal()
        return(embeddings)
      } else {
        shiny::removeModal()
        display_errors(
          title = "Error",
          size = "l",
          easy_close = TRUE,
          error_messages = "The file contains data in an unsupported format.
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
        title = "Error",
        size = "l",
        easy_close = TRUE,
        error_messages = "The file does not exist on the path."
      )
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

#' @title Load and check target data
#' @description Function for checking and loading target data in AI for Education - Studio.
#'
#' @param file_path `string` path to the file containing the target data.
#'
#' @return If there are any errors an error modal is displayed by calling the function [display_errors]. If there are no
#'   errors the function returns a `data.frame` containing the target data. In the case of erros the function returns
#'   `NULL`.
#'
#' @importFrom stringi stri_split_fixed
#' @importFrom stringi stri_trans_tolower
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
load_and_check_target_data <- function(file_path) {
  if (!is.null(file_path)) {
    if (file.exists(file_path) == TRUE) {
      display_processing(
        title = "Working. Please wait.",
        size = "l",
        easy_close = FALSE,
        message = ""
      )

      # extension=stringr::str_split_fixed(file_path,pattern="\\.",n=Inf)
      # extension=extension[1,ncol(extension)]
      # extension=stringr::str_to_lower(extension)
      extension <- stringi::stri_split_fixed(file_path, pattern = ".")[[1]]
      extension <- stringi::stri_trans_tolower(extension[[length(extension)]])

      if (extension == "csv" || extension == "txt") {
        target_data <- try(
          as.data.frame(
            utils::read.csv(
              file = file_path,
              header = TRUE
            )
          ),
          silent = TRUE
        )
      } else if (extension == "xlsx") {
        target_data <- try(
          as.data.frame(
            readxl::read_xlsx(
              path = file_path,
              sheet = 1,
              col_names = TRUE
            )
          ),
          silent = TRUE
        )
      } else if (extension %in% c("rda", "rdata")) {
        object_name <- load(file = file_path)
        target_data <- get(x = object_name)
        target_data <- try(
          as.data.frame(target_data),
          silent = TRUE
        )
      } else {
        target_data <- NA
      }

      # Final Check
      if (is.character(target_data)) {
        shiny::removeModal()
        display_errors(
          title = "Error",
          size = "l",
          easy_close = TRUE,
          error_messages = "Data can not be loaded as data frame. Please check your data."
        )
        return(NULL)
      } else {
        if ("id" %in% colnames(target_data)) {
          rownames(target_data) <- target_data$id
          shiny::removeModal()
          return(target_data)
        } else {
          shiny::removeModal()
          display_errors(
            title = "Error",
            size = "l",
            easy_close = TRUE,
            error_messages = "Data does not contain a column named 'id'. This
                       column is necessary to match the text embeddings to their
                       corresponding targets. Please check your data."
          )
          return(NULL)
        }
      }
    } else {
      shiny::removeModal()
      display_errors(
        title = "Error",
        size = "l",
        easy_close = TRUE,
        error_messages = "The file does not exist on the path."
      )
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

#' @title Check and ensure a valid empty argument
#' @description Function replaces empty input from an input widget into a valid empty argument for long running tasks.
#'   The valid empty argument is `NULL`.
#'
#' @param object Object to be transformed.
#'
#' @return Returns the object. Only in the case that the object is `NULL` or `object == ""` the function returns `NULL`
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
transform_input <- function(object) {
  res <- NULL
  if (!is.null(object) && object != "") res <- object
  return(res)
}

#' @title Checks for an empty input from an input widget
#' @description unction replaces checks for empty input produced by an input widget. These empty values are `NULL` and
#'   `""`.
#'
#' @param input Object to be transformed.
#'
#' @return Returns `TRUE` if input is `NULL` or `""`.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
check_for_empty_input <- function(input) {
  return(is.null(input) || input == "")
}

#' @title Checks and transforms an numeric input
#' @description Function ensured that a numeric input is returned or an empty value (`NULL`). This function should only
#'   be applied if a numeric input is expected.
#'
#' @param input Object to be transformed.
#'
#' @return Returns the input as a numeric input or `NULL` if input is `NULL` or `""`.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
check_numeric_input <- function(input) {
  res <- NULL
  if (!is.null(input) && input != "") res <- as.numeric(input)
  return(res)
}

#' @title Load target data for long running tasks
#' @description Function loads the target data for a long running task.
#'
#' @param file_path `string` Path to the file storing the target data.
#' @param selectet_column `string` Name of the column containing the target data.
#'
#' @details This function assumes that the target data is stored as a columns with the cases in the rows and the
#' categories in the columns. The ids of the cases must be stored in a column called "id".
#'
#' @return Returns a named factor containing the target data.
#'
#' @family studio_utils
#' @export
long_load_target_data <- function(file_path, selectet_column) {
  extension <- stringi::stri_split_fixed(file_path, pattern = ".")[[1]]
  extension <- stringi::stri_trans_tolower(extension[[length(extension)]])

  if (extension == "csv" || extension == "txt") {
    target_data <- try(
      as.data.frame(
        utils::read.csv(
          file = file_path,
          header = TRUE
        )
      ),
      silent = TRUE
    )
  } else if (extension == "xlsx") {
    target_data <- try(
      as.data.frame(
        readxl::read_xlsx(
          path = file_path,
          sheet = 1,
          col_names = TRUE
        )
      ),
      silent = TRUE
    )
  } else if (extension %in% c("rda", "rdata")) {
    object_name <- load(file = file_path)
    target_data <- get(x = object_name)
    target_data <- try(
      as.data.frame(target_data),
      silent = TRUE
    )
  } else {
    stop("Could not load data.")
  }

  # Final Check
  if (is.character(target_data)) {
    stop("Data can not be loaded as data frame. Please check your data.")
  }
  if ("id" %in% colnames(target_data)) {
    rownames(target_data) <- target_data$id
  } else {
    stop("Data does not contain a column named 'id'. This
                       column is necessary to match the text embeddings to their
                       corresponding targets. Please check your data.")
  }

  target_factor <- as.factor(target_data[[selectet_column]])
  names(target_factor) <- target_data$id

  return(target_factor)
}

#' @title Prepare history data of objects
#' @description Function for preparing the history data of a model in order to be plotted in AI for Education - Studio.
#'
#' @param model Model for which the data should be prepared.
#' @param final `bool` If `TRUE` the history data of the final training is used for the data set.
#' @param use_pl `bool` If `TRUE` data preparation assumes that pseudo labeling was applied during the training of the
#'   model.
#' @param pl_step `int` If `use_pl=TRUE` select the step within pseudo labeling for which the data should be prepared.

#' @return Returns a named `list` with the training history data of the model. The
#' reported measures depend on the provided model.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
prepare_training_history <- function(model,
                                     final = FALSE,
                                     use_pl = FALSE,
                                     pl_step = NULL) {
  plot_data <- model$last_training$history

  if (is.null_or_na(final)) final <- FALSE

  # Get standard statistics
  n_epochs <- model$last_training$config$epochs
  index_final <- length(model$last_training$history)


  # Get information about the existence of a training, validation, and test data set
  # Get Number of folds for the request
  if (final==FALSE) {
    n_folds <- length(model$last_training$history) - 1
    measures <- names(plot_data[[1]])
    if (!use_pl) {
      n_sample_type <- nrow(plot_data[[1]][[measures[1]]])
    } else {
      n_sample_type <- nrow(plot_data[[1]][[as.numeric(pl_step)]][[measures[1]]])
    }
  } else {
    n_folds <- 1
    measures <- names(plot_data[[index_final]])
    if (use_pl==FALSE) {
      n_sample_type <- nrow(plot_data[[index_final]][[measures[1]]])
    } else {
      n_sample_type <- nrow(plot_data[[index_final]][[as.numeric(pl_step)]][[measures[1]]])
    }
  }


  if (n_sample_type == 3) {
    sample_type_name <- c("train", "validation", "test")
  } else {
    sample_type_name <- c("train", "validation")
  }

  # Create array for saving the data-------------------------------------------
  result_list <- NULL
  for (j in 1:length(measures)) {
    measure <- measures[j]
    measure_array <- array(
      dim = c(
        n_folds,
        n_sample_type,
        n_epochs
      ),
      dimnames = list(fold = NULL, sample_type = sample_type_name, epoch = NULL)
    )

    final_data_measure <- matrix(
      data = NA,
      nrow = n_epochs,
      ncol = 3 * n_sample_type + 1
    )
    colnames(final_data_measure) <- c(
      "epoch",
      paste0(
        sample_type_name,
        c(
          rep("_min", times = n_sample_type),
          rep("_mean", times = n_sample_type),
          rep("_max", times = n_sample_type)
        )
      )
    )
    final_data_measure[, "epoch"] <- seq.int(from = 1, to = n_epochs)

    if (final==FALSE) {
      for (i in 1:n_folds) {
        if (use_pl==FALSE) {
          measure_array[i, , ] <- plot_data[[i]][[measure]]
        } else {
          measure_array[i, , ] <- plot_data[[i]][[as.numeric(pl_step)]][[measure]]
        }
      }
    } else {
      if (!use_pl) {
        measure_array[1, , ] <- plot_data[[index_final]][[measure]]
      } else {
        measure_array[1, , ] <- plot_data[[index_final]][[as.numeric(pl_step)]][[measure]]
      }
    }

    for (i in 1:n_epochs) {
      final_data_measure[i, "train_min"] <- min(measure_array[, "train", i])
      final_data_measure[i, "train_mean"] <- mean(measure_array[, "train", i])
      final_data_measure[i, "train_max"] <- max(measure_array[, "train", i])

      final_data_measure[i, "validation_min"] <- min(measure_array[, "validation", i])
      final_data_measure[i, "validation_mean"] <- mean(measure_array[, "validation", i])
      final_data_measure[i, "validation_max"] <- max(measure_array[, "validation", i])

      if (n_sample_type == 3) {
        final_data_measure[i, "test_min"] <- min(measure_array[, "test", i])
        final_data_measure[i, "test_mean"] <- mean(measure_array[, "test", i])
        final_data_measure[i, "test_max"] <- max(measure_array[, "test", i])
      }
    }
    result_list[j] <- list(final_data_measure)
  }

  # Finalize data---------------------------------------------------------------
  names(result_list) <- measures
  return(result_list)
}

#' @title Generate description for text embeddings
#' @description Function generates a description for the underling [TextEmbeddingModel] of
#' give text embeddings.
#'
#' @param embeddings Object of class [LargeDataSetForTextEmbeddings] or [EmbeddedText].
#' @return Returns a `shiny::tagList` containing the html elements for the user interface.
#'
#' @family studio_utils
#' @keywords internal
#'
create_data_embeddings_description <- function(embeddings) {
  model_info <- embeddings$get_model_info()
  info_table <- matrix(
    nrow = 3,
    ncol = 4,
    data = ""
  )
  info_table[1, 1] <- "Model Method:"
  info_table[2, 1] <- "Pooling Type:"
  info_table[3, 1] <- "Model Language:"

  info_table[1, 2] <- model_info$model_method
  info_table[2, 2] <- model_info$param_emb_pool_type
  info_table[3, 2] <- model_info$model_language

  info_table[1, 3] <- "Tokens per Chunk:"
  info_table[2, 3] <- "Max Chunks:"
  info_table[3, 3] <- "Token Overlap:"

  info_table[1, 4] <- model_info$param_seq_length
  info_table[2, 4] <- model_info$param_chunks
  info_table[3, 4] <- model_info$param_overlap

  ui <- list(
    bslib::value_box(
      value = embeddings$n_rows(),
      title = "Number of Cases",
      showcase = shiny::icon("list")
    ),
    shiny::tags$h3("Model:", model_info$model_label),
    shiny::tags$p("Name:", model_info$model_name),
    shiny::tags$p("Created", model_info$model_date),
    shiny::renderTable(
      expr = info_table,
      colnames = FALSE
    )
  )

  return(ui)
}

#' @title Function for setting up AI for Education - Studio
#' @description This functions checks if all nevessary R packages and python packages are available for using AI for
#'   Education - Studio. In the case python is not initialized it will set the conda environment to `"aifeducation"`. In
#'   the case python is already initialized it checks if the app can be run within the current environment.
#'
#' @return Function does not return anything. It is used for preparing python and R
#' in order to run AI for Education - Studio.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
check_and_prepare_for_studio <- function() {
  message("Checking R Packages.")
  r_packages <- c(
    "ggplot2",
    "rlang",
    "shiny",
    "shinyFiles",
    "shinyWidgets",
    "sortable",
    "bslib",
    "future",
    "promises",
    "DT",
    "readtext",
    "readxl"
  )

  missing_r_packages <- NULL
  for (i in 1:length(r_packages)) {
    if (!requireNamespace(r_packages[i], quietly = TRUE, )) {
      missing_r_packages <- append(
        x = missing_r_packages,
        values = r_packages[i]
      )
    }
  }

  if (length(missing_r_packages) > 0) {
    install_now <- utils::askYesNo(
      msg = paste(
        "The following R packages are missing for Aifeducation Studio.",
        missing_r_packages,
        "Do you want to install them now?"
      ),
      default = TRUE,
      prompts = getOption("askYesNo", gettext(c("Yes", "No")))
    )
    if (install_now) {
      utils::install.packages(missing_r_packages)
    } else {
      stop("Some necessary R Packages are missing.")
    }
  }

  message("Setting the correct conda environment.")
  if (!reticulate::py_available(FALSE)) {
    message("Python is not initalized.")
    if (!reticulate::condaenv_exists("aifeducation")) {
      stop("Aifeducation studio requires a conda environment 'aifeducation' with
      specific python libraries. Please install this. Please refer to the corresponding
      vignette for more details.")
    } else {
      message("Setting conda environment to 'aifeducation'.")
      reticulate::use_condaenv("aifeducation")
      message("Initializing python.")
      if (!reticulate::py_available(TRUE)) {
        stop("Python cannot be initalized. Please check your installation of python.")
      }
    }
  } else {
    message("Python is initalized. Try to start Aifeducation Studio with the current environment.")
  }

  message("Checking machine learning frameworks.")
  available_ml_frameworks <- NULL
  if (check_aif_py_modules(trace = FALSE, check = "pytorch")) {
    available_ml_frameworks <- append(available_ml_frameworks, values = "pytorch")
  }
  if (is.null(available_ml_frameworks)) {
    stop("No available machine learning frameworks found.")
  }

  # Set Transformer Logger to Error
  set_transformers_logger(level = "ERROR")
  # Disable tqdm progressbar
  transformers$logging$disable_progress_bar()
  datasets$disable_progress_bars()
}

#' @title Generate widgets for licensing a model
#' @description Function generates the input widgets for licensing a model.
#'
#' @param ns `function` for setting the namespace of the input elements. This should be `session$ns`.
#' @param model Model for which the description should be generated.
#'
#' @return Returns a `shiny::tagList` containing the html elements for the user interface.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
generate_doc_input_licensing_editor <- function(ns, model) {
  ui <- shiny::tagList(
    bslib::card(
      bslib::card_header("Editor"),
      bslib::card_body(
        shiny::textInput(
          inputId = ns(paste0("doc_editor_", "documentation_license")),
          label = "Model License",
          width = "100%",
          value = model$get_software_license()
        ),
        shiny::textInput(
          inputId = ns(paste0("doc_editor_", "software_license")),
          label = "Documentation License",
          width = "100%",
          value = model$get_documentation_license()
        ),
        shiny::actionButton(
          inputId = ns(paste0("doc_editor_", "licensing", "_save_button")),
          label = "Save",
          icon = shiny::icon("floppy-disk")
        )
      )
    )
  )
  return(ui)
}
