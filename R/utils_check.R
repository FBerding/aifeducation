#' @title Check class
#' @description Function for checking if an object is of a specific class.
#'
#' @param object Any R object.
#' @param classes `vector` containing the classes as strings which the object should belong to.
#' @param allow_NULL `bool` If `TRUE` allow the object to be `NULL`.
#' @return Function does nothing return. It raises an error if the object is not of the specified class.
#'
#' @family Utils
#' @keywords internal
#'
check_class <- function(object, classes, allow_NULL = FALSE) {
  if (!is.null(object)) {
    classes_object <- class(object)
    check_results <- sum(classes_object %in% classes)
    if (check_results < 1) {
      stop(
        paste(
          "Class of", quote(object), "must be:",
          paste(classes, collapse = ", ")
        )
      )
    }
  }

  if (!allow_NULL && is.null(object)) {
    stop(
      paste(
        quote(object), "is NULL. It must be:",
        paste(classes, collapse = ", ")
      )
    )
  }
}

#' @title Check type
#' @description Function for checking if an object is of a specific type
#'
#' @param object Any R object.
#' @param type `string` containing the type as string which the object should belong to.
#' @param allow_NULL `bool` If `TRUE` allow the object to be `NULL`.
#' @return Function does nothing return. It raises an error if the object is not of the specified type.
#'
#' @family Utils
#' @keywords internal
#'
check_type <- function(object, type = "bool", allow_NULL = FALSE) {
  if (!allow_NULL && is.null(object)) {
    stop("Object is not allowed to be NULL")
  }

  if (!is.null(object)) {
    if (type == "bool") {
      if (!isTRUE(object) && !isFALSE(object)) {
        stop(paste(quote(object), "must be TRUE or FALSE"))
      }
    } else if (type == "int") {
      if (!is.numeric(object) || (object %% 1) != 0) {
        stop(paste(quote(object), "must be an integer"))
      }
    } else if (type == "double") {
      if (!is.double(object)) {
        stop(paste(quote(object), "must be double"))
      }
    } else if (type == "string") {
      if (!is.character(object)) {
        stop(paste(quote(object), "must be a string"))
      }
    } else if (type == "vector") {
      if (!is.vector(object)) {
        stop(paste(quote(object), "must be a vector"))
      }
    } else if (type == "list") {
      if (!is.list(object)) {
        stop(paste(quote(object), "must be a list"))
      }
    } else {
      warning(paste0("There is no implemented check for type '", quote(type), "'."))
    }
  }
}
