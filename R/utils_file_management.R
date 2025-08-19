#' @title Create directory if not exists
#' @description Check whether the passed `dir_path` directory exists. If not, creates a new directory and prints a `msg`
#'   message if `trace` is `TRUE`.
#'
#' @param dir_path `string` A new directory path that should be created.
#' @param trace `bool` Whether a `msg` message should be printed.
#' @param msg `string` A message that should be printed if `trace` is `TRUE`.
#' @param msg_fun `func` Function used for printing the message.
#'
#' @return `TRUE` or `FALSE` depending on whether the shiny app is active.
#' @family Utils File Management Developers
#' @export
#'
create_dir <- function(dir_path, trace, msg = "Creating Directory", msg_fun = TRUE) {
  if (!dir.exists(dir_path)) {
    output_message(msg, trace, msg_fun)
    dir.create(dir_path, recursive = TRUE)
  }
}

#' @title Get file extension
#' @description Function for requesting the file extension
#'
#' @param file_path `string` Path to a file.
#' @return Returns the extension of a file as a string.
#'
#' @family Utils File Management Developers
#' @export
#'
get_file_extension <- function(file_path) {
  extension <- stringi::stri_split_fixed(file_path, pattern = ".")[[1]]
  extension <- stringi::stri_trans_tolower(extension[[length(extension)]])
  return(extension)
}
