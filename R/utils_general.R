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

#' @title Check if NULL or NA
#' @description Function for checking if an object is `NULL` or .
#'
#' @param object An object to test.
#' @return Returns `FALSE` if the object is not `NULL` and not `NA`. Returns `TRUE` in all other cases.
#'
#' @family Utils Developers
#' @keywords internal
#' @noRd
is.null_or_na <- function(object) {
  if (is.null(object)) {
    return(TRUE)
  } else {
    if (sum(is.na(object)) == length(object)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @title Combine embeddings in array form
#' @description Function combines two array by adding an array to another array along the
#' first dimension.
#'
#' @param ... `array` or `list` of `array`s. Arrays to be combined. All array must have row names.
#'
#' @return Returns the combined array
#'
#' @family Utils Developers
#' @keywords internal
#' @noRd
array_form_bind <- function(...) {
  objects <- list(...)
  arrays <- NULL

  for (object in objects) {
    if (is.list(object)) {
      for (j in seq_len(length(object))) {
        arrays[length(arrays) + 1] <- list(object[[j]])
      }
    } else {
      arrays[length(arrays) + 1] <- list(object)
    }
  }

  # arrays <- list(...)
  if (length(arrays) > 1) {
    total_rows <- 0

    dimension <- dim(arrays[[1]])

    for (i in seq_len(length(arrays))) {
      total_rows <- total_rows + dim(arrays[[i]])[1]

      # Check number of dimensions
      if (sum(dim(arrays[[i]])[-1] != dimension[-1])) {
        stop("The dimensions of the array differ.")
      }
    }

    combined_array <- array(
      data = NA,
      dim = c(total_rows, dimension[2], dimension[3])
    )

    intercept <- 0
    row_names <- NULL


    for (i in seq_len(length(arrays))) {
      index <- seq.int(
        from = 1,
        to = nrow(arrays[[i]]),
        by = 1
      ) + intercept

      combined_array[index, , ] <- arrays[[i]]

      intercept <- intercept + length(index)

      row_names <- append(x = row_names, rownames(arrays[[i]]))
    }

    rownames(combined_array) <- row_names
    return(combined_array)
  } else {
    return(arrays[[1]])
  }
}



#' @title Generate ID suffix for objects
#' @description Function for generating an ID suffix for objects of class [TextEmbeddingModel],
#'   [TEClassifierRegular], and [TEClassifierProtoNet].
#'
#' @param length `int` determining the length of the id suffix.
#' @return Returns a `string` of the requested length.
#' @family Utils Developers
#' @export
generate_id <- function(length = 16) {
  id_suffix <- NULL
  sample_values <- c(
    "a", "A",
    "b", "B",
    "c", "C",
    "d", "D",
    "e", "E",
    "f", "F",
    "g", "G",
    "h", "H",
    "i", "I",
    "j", "J",
    "k", "K",
    "l", "L",
    "m", "M",
    "n", "N",
    "o", "O",
    "p", "P",
    "q", "Q",
    "r", "R",
    "s", "S",
    "t", "T",
    "u", "U",
    "v", "V",
    "w", "W",
    "x", "X",
    "y", "Y",
    "z", "Z",
    seq(from = 0, to = 9, by = 1)
  )


  id_suffix <- sample(
    x = sample_values,
    size = length,
    replace = TRUE
  )
  id_suffix <- paste(id_suffix, collapse = "")
  return(id_suffix)
}

#' @title Number of cores for multiple tasks
#' @description Function for getting the number of cores that should be used
#' for parallel processing of tasks. The number of cores is set to 75 % of the
#' available cores. If the environment variable `CI` is set to `"true"` or if the
#' process is running on cran `2` is returned.
#'
#' @importFrom parallel detectCores
#'
#' @return Returns `int` as the number of cores.
#'
#' @family Utils Developers
#' @export
auto_n_cores <- function() {
  if (
    Sys.getenv("CI") == "true" ||
      Sys.getenv("NOT_CRAN") == "true" ||
      Sys.getenv("_R_CHECK_LIMIT_CORES_") == "true"
  ) {
    n_cores <- min(2, parallel::detectCores())
  } else {
    n_cores <- floor(parallel::detectCores() * 0.75)
  }
  return(n_cores = max(1, n_cores))
}

#' @title Create object
#'
#' @description  Support function for creating objects.
#' @param class `string` Name of the class to be created.#'
#' @return Returns an object of the requested class.#'
#' @family Utils Developers
#' @export
create_object=function(class){
  if(class=="TEClassifierRegular"){
    return(suppressMessages(TEClassifierRegular$new()))
  } else if(class=="TEClassifierProtoNet"){
    return(suppressMessages(TEClassifierProtoNet$new()))
  } else if(class=="TEClassifierSequential"){
    return(TEClassifierSequential$new())
  }else if(class=="TEClassifierSequentialPrototype"){
    return(TEClassifierSequentialPrototype$new())
  }else if(class=="TEClassifierParallel"){
    return(TEClassifierParallel$new())
  }else if(class=="TEClassifierParallelPrototype"){
    return(TEClassifierParallelPrototype$new())
  }else if(class=="TEFeatureExtractor"){
    return(TEFeatureExtractor$new())
  }  else if (class == "TextEmbeddingModel") {
    return(TextEmbeddingModel$new())
  } else if (class == "LargeDataSetForTextEmbeddings") {
   return(LargeDataSetForTextEmbeddings$new())
  } else if (class == "LargeDataSetForText") {
    return(LargeDataSetForText$new())
  } else if (class == "EmbeddedText") {
    return(EmbeddedText$new())
  } else if(class%in%unlist(AIFETrType)){
    return(aife_transformer.make(type=class,init_trace = FALSE))
  } else {
    stop("Object is not implemented in this function.")
  }
}

#' @title Detect base model's architecture
#' @description Function for detecting the base type of a base model.
#'
#' @param model A model of the transformer library.
#'
#' @return Returns a `string` describing base model's architecture.
#'
#' @family Utils Developers
#' @keywords internal
#' @noRd
detect_base_model_type=function(model){
  if(("transformers.configuration_utils.PretrainedConfig")%in%class(model)){
    type_string=model$architectures
  } else {
    type_string=model$config
  }

  if(stringi::stri_detect(str=tolower(type_string),regex = "^funnel([:alnum:]*)")){
    return("funnel")
  } else if(stringi::stri_detect(str=tolower(type_string),regex = "^bert([:alnum:]*)")){
    return("bert")
  } else if(stringi::stri_detect(str=tolower(type_string),regex = "^debertav2([:alnum:]*)")){
    return("deberta_v2")
  } else if(stringi::stri_detect(str=tolower(type_string),regex = "^mpnet([:alnum:]*)")){
    return("mpnet")
  } else if(stringi::stri_detect(str=tolower(type_string),regex = "^longformer([:alnum:]*)")){
    return("longformer")
  } else if(stringi::stri_detect(str=tolower(type_string),regex = "^roberta([:alnum:]*)")){
    return("roberta")
  } else if(stringi::stri_detect(str=tolower(type_string),regex = "^modernbert([:alnum:]*)")){
    return("modernbert")
  } else {
    stop("Architectue for the model could not be detected.")
  }
}
