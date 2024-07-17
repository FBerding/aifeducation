#' Saving objects created with 'aifeducation'
#'
#' Function for saving objects created with 'aifeducation'.
#'
#' @param object Object of class [TEClassifierRegular],
#' [TEClassifierProtoNet],  [TEFeatureExtractor], [TextEmbeddingModel], [LargeDataSetForTextEmbeddings],
#' [LargeDataSetForText] or [EmbeddedText] which should be saved.
#' @param dir_path Path to the directory where the should model is stored.
#' @param folder_name `string` Name of the folder where the files should be stored.
#' @return Function does not return a value. It saves the model to disk.
#' @return No return value, called for side effects.
#'
#' @family Saving and Loading
#'
#' @export
save_to_disk <- function(object,
                         dir_path,
                         folder_name) {
  # Check class of object
  check_class(
    object,
    c(
      "TEClassifierRegular",
      "TEClassifierProtoNet",
      "TEFeatureExtractor",
      "TextEmbeddingModel",
      "LargeDataSetForTextEmbeddings",
      "LargeDataSetForText",
      "EmbeddedText"
    ),
    FALSE
  )
  check_type(dir_path, "string", FALSE)
  check_type(folder_name, "string", FALSE)

  # Create path to save location
  save_location <- paste0(dir_path, "/", folder_name)

  # Create path to r_interface
  path_r_interface <- paste0(save_location, "/", "r_interface.rda")

  # Check directory
  if (dir.exists(dir_path) == FALSE) {
    dir.create(dir_path)
  }
  if (dir.exists(save_location) == FALSE) {
    dir.create(save_location)
  }

  # Save R object
  save(object, file = path_r_interface)

  # Save Python objects and additional files
  object$save(
    dir_path = dir_path,
    folder_name = folder_name
  )

  # Save additional files
  if (methods::is(object, "TEClassifierRegular") |
    methods::is(object, "TEClassifierProtoNet")) {
    # save feature extractor if part of the model
    if (object$model_config$use_fe == TRUE) {
      object$feature_extractor$save(
        dir_path = save_location,
        folder_name = "feature_extractor"
      )
    }
  }
}


#' Loading objects created with 'aifeducation'
#'
#' Function for loading objects created with 'aifeducation'.
#'
#' @param dir_path Path to the directory where the model is stored.
#' @return Returns an object of class [TEClassifierRegular],
#' [TEClassifierProtoNet],  [TEFeatureExtractor], [TextEmbeddingModel], [LargeDataSetForTextEmbeddings],
#' [LargeDataSetForText] or [EmbeddedText].
#'
#' @family Saving and Loading
#'
#' @export
load_from_disk <- function(dir_path) {
  # Load the Interface to R
  interface_path <- paste0(dir_path, "/r_interface.rda")

  # Check for r_interface.rda
  if (file.exists(interface_path) == FALSE) {
    stop("There is no file r_interface.rda in the selected directory")
  }

  # Load interface
  name_interface <- load(interface_path)
  loaded_object <- get(x = name_interface)

  if (methods::is(loaded_object, "TEClassifierRegular") |
    methods::is(loaded_object, "TEClassifierProtoNet")) {
    # Call load method
    loaded_object$load(dir_path)

    # Load feature extractor if part of the model
    if (loaded_object$model_config$use_fe == TRUE) {
      loaded_object$feature_extractor$load(
        dir_path = paste0(dir_path, "/feature_extractor")
      )
    }
  } else if (methods::is(loaded_object, "TEFeatureExtractor")) {
    loaded_object$load(dir_path)
  } else if (methods::is(loaded_object, "TextEmbeddingModel")) {
    if (loaded_object$get_model_info()$model_method %in% c("glove_cluster", "lda") == FALSE) {
      loaded_object$load(dir_path)
    }
  } else if (methods::is(loaded_object, "LargeDataSetForTextEmbeddings") |
    methods::is(loaded_object, "LargeDataSetForText")) {
    loaded_object$load(dir_path)
  }

  return(loaded_object)
}
