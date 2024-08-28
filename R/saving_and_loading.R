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
  loaded_object <- load_R_interface(dir_path)

  if (methods::is(loaded_object, "TEClassifierRegular") &
    !methods::is(loaded_object, "TEClassifierProtoNet")) {
    model <- TEClassifierRegular$new()
  } else if (methods::is(loaded_object, "TEClassifierProtoNet")) {
    model <- TEClassifierProtoNet$new()
  } else if (methods::is(loaded_object, "TEFeatureExtractor")) {
    model <- TEFeatureExtractor$new()
  } else if (methods::is(loaded_object, "TextEmbeddingModel")) {
    model <- TextEmbeddingModel$new()
  } else if (methods::is(loaded_object, "LargeDataSetForTextEmbeddings")) {
    model <- LargeDataSetForTextEmbeddings$new()
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
