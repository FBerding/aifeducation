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
check_class <- function(object,object_name=NULL, classes, allow_NULL = FALSE) {
  if (!is.null(object)) {
    classes_object <- class(object)
    check_results <- sum(classes_object %in% classes)
    if (check_results < 1) {
      stop(
        paste(
          "Class of", object_name, "must be:",
          paste(classes, collapse = ", ")
        )
      )
    }
  }

  if (!allow_NULL && is.null(object)) {
    stop(
      paste(
        object_name, "is NULL. It must be:",
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
#' @param min `double` or `int` Minimal value for the object.
#' @param max `double` or `int` Maximal value for the object.
#' @param allowed_values `vector` of `string`s determining the allowed values. If all strings are allowed
#' set this argument to `NULL`.
#' @return Function does nothing return. It raises an error if the object is not of the specified type.
#'
#' @family Utils
#' @keywords internal
#' @noRd
#'
check_type <- function(object,object_name=NULL, type = "bool", allow_NULL = FALSE, min = NULL, max = NULL, allowed_values = NULL) {
  if(is.null(object_name)){
    tmp_name=dQuote(object)
  } else {
    tmp_name=object_name
  }

  if (!allow_NULL && is.null(object)) {
    stop(paste(tmp_name,"is not allowed to be NULL"))
  }

  if (!is.null(object)) {
    #--------------------
    if (type == "bool") {
      if (!isTRUE(object) && !isFALSE(object)) {
        stop(paste(tmp_name, "must be TRUE or FALSE"))
      }
      #------------------------
    } else if (type == "int") {
      if (!is.numeric(object) || (object %% 1) != 0) {
        stop(paste(tmp_name, "must be an integer"))
      }
      #---------------------------
    } else if (type == "double") {
      if (!is.numeric(object)) {
        stop(paste(tmp_name, "must be double"))
      } else {
        if (!(min <= object & object <= max)) {
          stop(paste(tmp_name, "must greater or equal", min, "and smaller or equal", max))
        }
      }
    } else if (type == "(double") {
      if (!is.numeric(object)) {
        stop(paste(tmp_name, "must be double"))
      } else {
        if (!(min < object & object <= max)) {
          stop(paste(tmp_name, "must greater ", min, "and smaller or equal", max))
        }
      }
    } else if (type == "double)") {
      if (!is.numeric(object)) {
        stop(paste(tmp_name, "must be double"))
      } else {
        if (!(min <= object & object <= max)) {
          stop(paste(tmp_name, "must greater or equal", min, "and smaller", max))
        }
      }
    } else if (type == "(double)") {
      if (!is.numeric(object)) {
        stop(paste(tmp_name, "must be double"))
      } else {
        if (!(min <= object & object <= max)) {
          stop(paste(tmp_name, "must greater", min, "and smaller", max))
        }
      }
    } else if (type == "string") {
      #------------------------------
      if (!is.character(object)) {
        stop(paste(tmp_name, "must be a string"))
      } else {
        if (!is.null(allowed_values)) {
          if (object %in% allowed_values == FALSE) {
            stop(paste(tmp_name, "must be one of the following:", allowed_values, collapse = ", "))
          }
        }
      }
    } else if (type == "vector") {
      if (!is.vector(object)) {
        stop(paste(tmp_name, "must be a vector"))
      }
    } else if (type == "list") {
      if (!is.list(object)) {
        stop(paste(tmp_name, "must be a list"))
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
#' @return Function returns `TRUE` if the numpy array is writable. It returns `FALSE`
#' if the array is not writable.
#'
#' @family Utils
#' @keywords internal
#' @noRd
#'
numpy_writeable <- function(np_array) {
  if (!inherits(x = np_array, what = c("numpy.ndarray"))) {
    stop("Provided object is no numpy array")
  }
  return(reticulate::py_to_r(np_array$flags["WRITEABLE"]))
}

#' @title Compares and checks the versions of packages
#' @description Function for checking if a package is later as another package.
#'
#' @param a Version string of the first package.
#' @param b Version string of the second package.
#' @param `string` Operator determining the kind of comparison. Allowed values are
#' * `"=="` for equality.
#' * `">="` if package a should be equal or later as package b.
#' * `">"` if package a should be later as package b.
#' * `"<="` if package a should be equal or earlier as package b.
#' * `"<"` if package a should be earlier as package b.
#' @return Function returns `TRUE` if the numpy array is writable. It returns `FALSE`
#' if the array is not writable.
#'
#' @importFrom utils compareVersion
#'
#' @family Utils
#' @keywords internal
#' @noRd
#'
check_versions <- function(a, operator = "==", b) {
  a=as.character(a)
  b=as.character(b)
  res <- utils::compareVersion(a = a, b = b)
  if (operator == "==") {
    if (res == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (operator == ">=") {
    if (res >= 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (operator == ">") {
    if (res > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (operator == "<=") {
    if (res <= 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (operator == "<") {
    if (res < 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @title Check class and type
#' @description Function for checking if an object is of a specific type or class.
#'
#' @param object Any R object.
#' @param type_classes `vector` of `string`s containing the type or classes which the object should belong to.
#' @param allow_NULL `bool` If `TRUE` allow the object to be `NULL`.
#' @param min `double` or `int` Minimal value for the object.
#' @param max `double` or `int` Maximal value for the object.
#' @param allowed_values `vector` of `string`s determining the allowed values. If all strings are allowed
#' set this argument to `NULL`.
#' @return Function does nothing return. It raises an error if the object is not of the specified type.
#'
#' @note parameter `min`, `max`, and `allowed_values` do not apply if `type_classes` is a class.
#' @note allowed_values does only apply if `type_classes` is `string`.
#'
#' @family Utils
#' @keywords internal
#' @noRd
#'
check_class_and_type <- function(object,object_name=NULL, type_classes = "bool", allow_NULL = FALSE, min = NULL, max = NULL, allowed_values = NULL) {
  if (length(type_classes)==sum(type_classes %in% c("bool", "int", "double", "(double", "double)", "(double)", "string", "vector", "list"))) {
    check_type(
      object = object,
      object_name=object_name,
      type = type_classes,
      allow_NULL = allow_NULL,
      min = min, max = max,
      allowed_values = allowed_values
    )
  } else {
    check_class(
      object = object,
      object_name = object_name,
      classes = type_classes,
      allow_NULL = allow_NULL
    )
  }
}

#--------------------------------------------------------------------------
check_all_args=function(args){
  arg_dict=get_param_dict()
  #Select only arguments that occur in args and var group
  shared_var_names=intersect(x=names(arg_dict),y=names(args))
  #Check every argument
  for(var in shared_var_names){
    current_def=arg_dict[[var]]
    check_class_and_type(
      object=args[[var]],
      object_name=var,
      type_classes = current_def$type,
      allow_NULL = current_def$allow_null,
      min = current_def$min,
      max = current_def$max,
      allowed_values = current_def$allowed_values
    )
  }
}

is_valid_and_exportable_param=function(arg_name,param_dict){
  param_dict_entry=param_dict[[arg_name]]
  if(max(param_dict_entry$type%in%c("bool", "int", "double", "(double", "double)", "(double)", "string", "vector", "list"))&
     !arg_name%in%c("log_dir","log_write_interval")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
