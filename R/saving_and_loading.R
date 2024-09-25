#' @title Saving objects created with 'aifeducation'
#' @description Function for saving objects created with 'aifeducation'.
#'
#' @param object Object of class [TEClassifierRegular], [TEClassifierProtoNet],  [TEFeatureExtractor],
#'   [TextEmbeddingModel], [LargeDataSetForTextEmbeddings], [LargeDataSetForText] or [EmbeddedText] which should be
#'   saved.
#' @param dir_path `string` Path to the directory where the should model is stored.
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
  create_dir(dir_path, FALSE)
  create_dir(save_location, FALSE)

  # Create config and save to disk
  config_file <- create_config(object)
  save(config_file, file = path_r_interface)

  # Save Python objects and additional files
  object$save(
    dir_path = dir_path,
    folder_name = folder_name
  )
}


#' @title Loading objects created with 'aifeducation'
#' @description Function for loading objects created with 'aifeducation'.
#'
#' @param dir_path `string` Path to the directory where the model is stored.
#' @return Returns an object of class [TEClassifierRegular], [TEClassifierProtoNet],  [TEFeatureExtractor],
#'   [TextEmbeddingModel], [LargeDataSetForTextEmbeddings], [LargeDataSetForText] or [EmbeddedText].
#'
#' @family Saving and Loading
#'
#' @export
load_from_disk <- function(dir_path) {
  loaded_config <- load_R_interface(dir_path)

  if (loaded_config$class == "TEClassifierRegular") {
    model <- TEClassifierRegular$new()
  } else if (loaded_config$class == "TEClassifierProtoNet") {
    model <- TEClassifierProtoNet$new()
  } else if (loaded_config$class == "TEFeatureExtractor") {
    model <- TEFeatureExtractor$new()
  } else if (loaded_config$class == "TextEmbeddingModel") {
    model <- TextEmbeddingModel$new()
  } else if (loaded_config$class == "LargeDataSetForTextEmbeddings") {
    model <- LargeDataSetForTextEmbeddings$new()
  } else if (loaded_config$class == "LargeDataSetForText") {
    model <- LargeDataSetForText$new()
  } else {
    stop("Class type not supported.")
  }

  # load and update model
  model$load_from_disk(dir_path = dir_path)
  return(model)
}


load_R_interface <- function(dir_path) {
  # Load the Interface to R
  interface_path <- paste0(dir_path, "/r_interface.rda")

  # Check for r_interface.rda
  if (file.exists(interface_path) == FALSE) {
    stop("There is no file r_interface.rda in the selected directory")
  }

  # Load interface
  name_interface <- load(interface_path)
  loaded_object <- get(x = name_interface)
  return(loaded_object)
}

#' Create config for R interfaces
#'
#' Function creates a config that can be saved to disk. It is used during loading
#' an object from disk in order to set the correct configuration.
#'
#' @return Returns a `list` that contains the class of the object, the public, and
#' private fields.
#'
#' @family Saving and Loading
#'
#' @export
create_config <- function(object) {
  config <- object$get_all_fields()
  config["class"] <- class(object)[1]
  return(config)
}
