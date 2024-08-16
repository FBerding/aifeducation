#'Check for input errors - classifier create
#'
#'Functions check for input errors in AI for Education - Studio in the case of
#'the creation of classifiers.
#'
#'@param destination_path `string` Path to the destination directory.
#'@param folder_name `string` Name of the folder to be created within the destination directory.
#'@param path_to_embeddings `vector` Path to the folder where the embeddings are stored.
#'@param path_to_target_data `string` Path to the folder where the target data are stored.
#'@param path_to_feature_extractor `string` Path to the folder where the [TEFeatureExtractor] is stored. If
#'no [TEFeatureExtractor] should be used this arugment must be `NULL`.
#'@param model_name `string` Name for the model.
#'@param model_label `string` Label of the model.
#'
#'@return Returns a `list` containing the elements of a user interface for
#'displaying the errors. If no errors ware found function returns `NULL`.
#'
#'@family studio_gui_page_classifier_create
#'@keywords internal
#'
check_errors_create_classifier <- function(destination_path,
                                           folder_name,
                                           path_to_embeddings,
                                           path_to_target_data,
                                           path_to_feature_extractor,
                                           model_name,
                                           model_label) {
  # List for gathering errors
  error_list <- NULL

  # Destination
  if (dir.exists(destination_path) == FALSE) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "The target directory does not exist. Please check path."
    ))
  }

  if (check_for_empty_input(folder_name)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Folder name is not set."
    ))
  }



  # Embeddings
  if (dir.exists(path_to_embeddings) == FALSE) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Directory which should store embeddings does not exist."
    ))
  } else {
    embeddings <- try(load_from_disk(path_to_embeddings), silent = TRUE)
    if ("try-error" %in% class(embeddings)) {
      error_list[length(error_list) + 1] <- list(shiny::tags$p(
        embeddings
      ))
    } else if (!("LargeDataSetForTextEmbeddings" %in% class(embeddings) |
                 "EmbeddedText" %in% class(embeddings))) {
      error_list[length(error_list) + 1] <- list(shiny::tags$p(
        "Directory which should store embeddings does not contain an object of class 'LargeDataSetForTextEmbeddings'
        or 'EmbeddedText'."
      ))
    }
  }

  # Target Data

  # FeatureExtractor
  if (check_for_empty_input(path_to_feature_extractor)==FALSE) {
    if (dir.exists(path_to_feature_extractor) == FALSE) {
      error_list[length(error_list) + 1] <- list(shiny::tags$p(
        "Directory which should store the TEFeatureExtractor does not exist."
      ))
    } else {
      feature_extractor <- try(load_from_disk(path_to_feature_extractor), silent = TRUE)
      if ("try-error" %in% class(feature_extractor)) {
        error_list[length(error_list) + 1] <- list(shiny::tags$p(
          feature_extractor
        ))
      } else if (!("TEFeatureExtractor" %in% class(feature_extractor))) {
        error_list[length(error_list) + 1] <- list(shiny::tags$p(
          "Directory which should contain a feature extractor does not contain an object of class
          TEFeatureExtractor."
        ))
      } else {
        if (feature_extractor$get_text_embedding_model_name() != embeddings$get_text_embedding_model_name()) {
          error_list[length(error_list) + 1] <- list(shiny::tags$p(
            "The TextEmbeddingModel of the TEFeatureExtractor and the provided embeddings
            are not compatible."
          ))
        }
      }
    }
  }


  # Model Nane and Model Label
  if (check_for_empty_input(model_name)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Name of the classifier ist not set."
    ))
  }

  if (check_for_empty_input( model_label)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Label of the classifier ist not set."
    ))
  }

  if (length(error_list) > 0) {
    tmp_ui_error <- NULL
    for (i in 1:length(error_list)) {
      tmp_ui_error[length(tmp_ui_error) + 1] <- list(
        shiny::tags$p(error_list[i])
      )
    }
    return(tmp_ui_error)
  } else {
    return(NULL)
  }
}

#'Check for input errors - create data set for raw texts
#'
#'Functions check for input errors in AI for Education - Studio in the case of
#'the creation of classifiers.
#'
#'@param source_path `string` Path of the directory that stores the raw texts.
#'@param destination_path `string` Path to the directory where the data set should be saved.
#'@param include_txt `bool` If `TRUE` .txt files are included.
#'@param include_pdf `bool`If `TRUE` ,pdf files are included.
#'@param include_xlsx `bool` If `TRUE` .xlsx files are included.
#'@param excel_id_column `string` Name of the column containing the id of texts.
#'@param excel_text_column `string` Name of the column containing the texts.
#'@param excel_license_column `string` Name of the column containing the licenses of texts.
#'@param excel_bib_entry_column `string` Name of the column containing the bibliographic information of texts.
#'
#'@return Returns a `list` containing the elements of a user interface for
#'displaying the errors. If no errors ware found function returns `NULL`.
#'
#'@family studio_gui_page_data_management
#'@keywords internal
#'
check_errors_create_dataset_raw_texts <- function(source_path,
                                                  destination_path,
                                                  folder_name,
                                                  include_txt,
                                                  include_pdf,
                                                  include_xlsx,
                                                  excel_id_column,
                                                  excel_text_column,
                                                  excel_license_column,
                                                  excel_bib_entry_column) {
  # List for gathering errors
  error_list <- NULL

  # Check if all inputs are correctly set
  if (dir.exists(source_path) == FALSE) {
    error_list[length(error_list) + 1] <- "Source directory does not exist. Please check
      your directory path."
  }
  if (is.null(destination_path) | destination_path == "") {
    error_list[length(error_list) + 1] <- "Path to the output directory is missing."
  }
  if (is.null(folder_name) | folder_name == "") {
    error_list[length(error_list) + 1] <- "File name for the text dataset is missing."
  }
  if (dir.exists(dirname(destination_path)) == FALSE) {
    error_list[length(error_list) + 1] <- "Target directory does not exist. Please check
      if a directory exists for saving your data."
  }
  if (include_txt == FALSE &
      include_pdf == FALSE &
      include_xlsx == FALSE) {
    error_list[length(error_list) + 1] <- "No file types selected. Please select
      at least one file type."
  }

  if (length(error_list) > 0) {
    tmp_ui_error <- NULL
    for (i in 1:length(error_list)) {
      tmp_ui_error[length(tmp_ui_error) + 1] <- list(
        shiny::tags$p(error_list[i])
      )
    }
    return(tmp_ui_error)
  } else {
    return(NULL)
  }
}



#'Check for input errors - feature extracotors create
#'
#'Functions check for input errors in AI for Education - Studio in the case of
#'the creation of classifiers.
#'
#'@param destination_path `string` Path to the destination directory.
#'@param folder_name `string` Name of the folder to be created within the destination directory.
#'@param path_to_embeddings `vector` Path to the folder where the embeddings are stored.
#'@param model_name `string` Name for the model.
#'@param model_label `string` Label of the model.
#'@param features `int` Number of dimensions the text embeddings should be reduced to.
#'
#'@return Returns a `list` containing the elements of a user interface for
#'displaying the errors. If no errors ware found function returns `NULL`.
#'
#'@family studio_gui_page_feature_extractor_create
#'@keywords internal
#'
check_errors_create_feature_extractor <- function(destination_path,
                                                  folder_name,
                                                  path_to_embeddings,
                                                  features,
                                                  model_name,
                                                  model_label) {
  # List for gathering errors
  error_list <- NULL

  # Destination
  if (dir.exists(destination_path) == FALSE) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "The target directory does not exist. Please check path."
    ))
  }

  if (check_for_empty_input(folder_name)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Folder name is not set."
    ))
  }



  # Embeddings
  if (dir.exists(path_to_embeddings) == FALSE) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Directory which should store embeddings does not exist."
    ))
  } else {
    embeddings <- try(load_from_disk(path_to_embeddings), silent = TRUE)
    if ("try-error" %in% class(embeddings)) {
      error_list[length(error_list) + 1] <- list(shiny::tags$p(
        embeddings
      ))
    } else if (!("LargeDataSetForTextEmbeddings" %in% class(embeddings) |
                 "EmbeddedText" %in% class(embeddings))) {
      error_list[length(error_list) + 1] <- list(shiny::tags$p(
        "Directory which should store embeddings does not contain an object of class 'LargeDataSetForTextEmbeddings'
        or 'EmbeddedText'."
      ))
    } else {
      if(embeddings$get_original_features()<=features){
        error_list[length(error_list) + 1] <- list(shiny::tags$p(
          paste("Target dimension is",features,". This value must be smaller
                as the orignal number of features which is",embeddings$get_original_features(),".")
        ))
      }
    }
  }



  # Model Name and Model Label
  if (check_for_empty_input(model_name)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Name of the classifier ist not set."
    ))
  }

  if (check_for_empty_input(model_label)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Label of the classifier ist not set."
    ))
  }

  if (length(error_list) > 0) {
    tmp_ui_error <- NULL
    for (i in 1:length(error_list)) {
      tmp_ui_error[length(tmp_ui_error) + 1] <- list(
        shiny::tags$p(error_list[i])
      )
    }
    return(tmp_ui_error)
  } else {
    return(NULL)
  }
}



#'Check for input errors - classifier predict
#'
#'Functions check for input errors in AI for Education - Studio in the case of
#'the creation of classifiers.
#'
#'@param embeddings Object of class [LargeDataSetForTextEmbeddings] or [EmbeddedText].
#'@param model `Object of class [TEClassifierRegular] or [TEClassifierProtoNet].
#'
#'@return Returns a `list` containing the elements of a user interface for
#'displaying the errors. If no errors ware found function returns `NULL`.
#'
#'@family studio_gui_page_classifier_predict
#'@keywords internal
#'
check_errors_predict_classifier <- function(embeddings,
                                            model) {
  # List for gathering errors
  error_list <- NULL

  # Embeddings
  if (!("LargeDataSetForTextEmbeddings" %in% class(embeddings) |
        "EmbeddedText" %in% class(embeddings))) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "Directory which should store embeddings does not contain an object of class 'LargeDataSetForTextEmbeddings'
        or 'EmbeddedText'."
    ))
  }

  # Embeddings compatibilty
  if (model$get_text_embedding_model_name() != embeddings$get_text_embedding_model_name()) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p(
      "The TextEmbeddingModel of the classifier and the TextEmbeddingModel of the provided
      data are not the same."
    ))
  }



  if (length(error_list) > 0) {
    tmp_ui_error <- NULL
    for (i in 1:length(error_list)) {
      tmp_ui_error[length(tmp_ui_error) + 1] <- list(
        shiny::tags$p(error_list[i])
      )
    }
    return(tmp_ui_error)
  } else {
    return(NULL)
  }
}


#'Check for input errors - text embedding model embed
#'
#'Functions check for input errors in AI for Education - Studio.
#'
#'@param destination_path `string` Path to the directory where the data set should be saved.
#'@param folder_name `string` Name of the folder to be created within the destination directory.
#'@param path_to_raw_texts `string` Path to the folder where the data set with the raw texts is stored.
#'@param batch_size `int` Batch size.
#'
#'@return Returns a `list` containing the elements of a user interface for
#'displaying the errors. If no errors ware found function returns `NULL`.
#'
#'@family studio_gui_text_embedding_model_embed
#'@keywords internal
#'
check_errors_text_embedding_model_embed <- function(destination_path,
                                                    folder_name,
                                                    path_to_raw_texts,
                                                    batch_size) {
  # List for gathering errors
  error_list <- NULL

  # Check if all inputs are correctly set
  if (!dir.exists(destination_path)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p("The destination directory does not
                                                   exist. Please check your directory path
                                                   and/or create that directory."))
  }
  if (is.null(folder_name) | folder_name == "") {
    error_list[length(error_list) + 1] <- "Folder name for the dataset storing the embeddings is missing."
  }

  if (!file.exists(path_to_raw_texts)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p("There is no file at the current path."))
  } else {
    raw_texts <- try(load_from_disk(dir_path = path_to_raw_texts), silent = TRUE)
    if ("try-error" %in% class(raw_texts)) {
      error_list[length(error_list) + 1] <- raw_texts
    } else {
      if (!"LargeDataSetForText" %in% class(raw_texts)) {
        error_list[length(error_list) + 1] <- paste("The object is not of class LargeDataSetForText")
      }
    }
  }


  # summary
  if (length(error_list) > 0) {
    tmp_ui_error <- NULL
    for (i in 1:length(error_list)) {
      tmp_ui_error[length(tmp_ui_error) + 1] <- list(
        shiny::tags$p(error_list[i])
      )
    }
    return(tmp_ui_error)
  } else {
    return(NULL)
  }
}



#'Check for input errors - text embedding model embed
#'
#'Functions check for input errors in AI for Education - Studio.
#'
#'@param destination_path `string` Path to the directory where the data set should be saved.
#'@param folder_name `string` Name of the folder to be created within the destination directory.
#'@param path_to_base_model `string` Path to the folder where the base model is stored.
#'@param interface_architecture `list` containing information about the base model.
#'
#'@return Returns a `list` containing the elements of a user interface for
#'displaying the errors. If no errors ware found function returns `NULL`.
#'
#'@family studio_gui_page_text_embedding_model_create
#'@keywords internal
#'
check_errors_text_embedding_model_create <- function(destination_path,
                                                     folder_name,
                                                     path_to_base_model,
                                                     interface_architecture) {
  # List for gathering errors
  error_list <- NULL

  # Check if all inputs are correctly set
  if (!dir.exists(destination_path)) {
    error_list[length(error_list) + 1] <- list(shiny::tags$p("The destination directory does not
                                                   exist. Please check your directory path
                                                   and/or create that directory."))
  }
  if (is.null(folder_name) | folder_name == "") {
    error_list[length(error_list) + 1] <- "File name for the text dataset is missing."
  }

  if (!identical(path_to_base_model, character(0))) {
    if (is.null(interface_architecture[[1]]) &
        is.null(interface_architecture[[2]])) {
      error_list[length(error_list) + 1] <- "There is no model to load in the directory."
    }
  }

  if (length(error_list) > 0) {
    tmp_ui_error <- NULL
    for (i in 1:length(error_list)) {
      tmp_ui_error[length(tmp_ui_error) + 1] <- list(
        shiny::tags$p(error_list[i])
      )
    }
    return(tmp_ui_error)
  } else {
    return(NULL)
  }
}
