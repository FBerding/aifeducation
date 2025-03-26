#' @title Get current conda environment
#' @description Function for getting the active conda environment.
#'
#' @return Returns the name of the current conda environment as a `string`.
#' In the case that python is not active or conda is not used this function
#' raises an error.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
get_current_conda_env <- function() {
  if (reticulate::py_available() == TRUE) {
    current_sessions <- reticulate::py_config()
    if (current_sessions$conda == "True") {
      current_conda_env <- base::strsplit(
        x = current_sessions$pythonhome,
        split = "/",
        fixed = TRUE
      )
      current_conda_env <- current_conda_env[[1]][length(current_conda_env[[1]])]
      return(current_conda_env)
    } else {
      stop("No conda environment active.")
    }
  } else {
    stop("Python session not active.")
  }
}

#' @title Get current conda environment
#' @description Function for getting the active conda environment.
#'
#' @return Returns the name of the current conda environment as a `string`.
#' In the case that python is not active or conda is not used this function
#' raises an error.
#'
#' @family studio_utils
#' @keywords internal
#' @noRd
#'
get_current_venv <- function() {
  if (reticulate::py_available() == TRUE) {
    current_sessions <- reticulate::py_config()
    if (current_sessions$conda == "False") {
      current_venv <- base::strsplit(
        x = current_sessions$pythonhome,
        split = "/",
        fixed = TRUE
      )
      current_venv <- current_venv[[1]][length(current_venv[[1]])]
      return(current_venv)
    } else {
      stop("No virtual environment active.")
    }
  } else {
    stop("Python session not active.")
  }
}

#' @title Get versions of python components
#' @description Function for requesting a summary of the versions of all
#' critical python components.
#'
#' @return Returns a list that contains the version number of python and
#' the versions of critical python packages. If a package is not available
#' version is set to `NA`.
#'
#' @family Utils
#' @importFrom reticulate py_module_available
#' @importFrom reticulate py_config
#' @importFrom reticulate import
#' @export
#'
get_py_package_versions <- function() {
  list_of_packages <- c(
    "torch",
    "pyarrow",
    "transformers",
    "tokenizers",
    "pandas",
    "datasets",
    "codecarbon",
    "safetensors",
    "torcheval",
    "accelerate",
    "numpy"
  )

  versions <- vector(length = length(list_of_packages) + 1)
  names(versions) <- c("python", list_of_packages)
  versions["python"] <- as.character(reticulate::py_config()$version)
  for (package in list_of_packages) {
    versions[package] <-get_py_package_version(package)
  }
  return(versions)
}

#' @title Get versions of a specific python package
#' @description Function for requesting the version of a specific python package.
#'
#' @return Returns the version as `string` or `NA` if the package does not exist.
#'
#' @family Utils
#' @importFrom reticulate import
#' @export
get_py_package_version<-function(package_name){
  if (reticulate::py_module_available(package_name) == TRUE) {
    tmp_package <- reticulate::import(module = package_name, delay_load = FALSE)
    return(as.character(tmp_package["__version__"]))
  } else {
    return(NA)
  }
}
