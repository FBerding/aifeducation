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
#' @noRd
#'
check_class <- function(object, classes, allow_NULL = FALSE) {
  if (!is.null(object)) {
    classes_object <- class(object)
    check_results <- sum(classes_object %in% classes)
    if (check_results < 1) {
      stop(
        paste(
          "Class of", dQuote(object), "must be:",
          paste(classes, collapse = ", ")
        )
      )
    }
  }

  if (!allow_NULL && is.null(object)) {
    stop(
      paste(
        dQuote(object), "is NULL. It must be:",
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
#' @noRd
#'
check_type <- function(object, type = "bool", allow_NULL = FALSE) {
  if (!allow_NULL && is.null(object)) {
    stop("Object is not allowed to be NULL")
  }

  if (!is.null(object)) {
    if (type == "bool") {
      if (!isTRUE(object) && !isFALSE(object)) {
        stop(paste(dQuote(object), "must be TRUE or FALSE"))
      }
    } else if (type == "int") {
      if (!is.numeric(object) || (object %% 1) != 0) {
        stop(paste(dQuote(object), "must be an integer"))
      }
    } else if (type == "double") {
      if (!is.double(object)) {
        stop(paste(dQuote(object), "must be double"))
      }
    } else if (type == "string") {
      if (!is.character(object)) {
        stop(paste(dQuote(object), "must be a string"))
      }
    } else if (type == "vector") {
      if (!is.vector(object)) {
        stop(paste(dQuote(object), "must be a vector"))
      }
    } else if (type == "list") {
      if (!is.list(object)) {
        stop(paste(dQuote(object), "must be a list"))
      }
    } else {
      warning(paste0("There is no implemented check for type", dQuote(type), "."))
    }
  }
}

#' @title Check numpy array to be writable
#' @description Function for checking if a numpy array is writable.
#'
#' @param np_array A numpy array.
#' @return Function returns `TRUE` if the numpy array is writable. It retuns `FALSE`
#' if the array is not writable.
#'
#' @family Utils
#' @keywords internal
#' @noRd
#'
numpy_writeable <- function(np_array) {
  if(!inherits(x=np_array,what=c("numpy.ndarray"))){
    stop("Provided object is no numpy array")
  }
return(reticulate::py_to_r(np_array$flags["WRITEABLE"]))
}
