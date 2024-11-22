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
