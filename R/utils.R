#' @title Increment aifeducation progress bar
#' @description Used to update aifeducation progress bar to 1.
#'
#' @param value `int` Current value of progress bar.
#' @param total `int` Total value of progress bar.
#' @param title `string` Title of progress bar.
#'
#' @return New incremented value.
#' @family Utils
#' @keywords internal
#' @noRd
increment_aife_progress_bar <- function(value, total, title) {
  value <- value + 1
  update_aifeducation_progress_bar(
    value = value,
    total = total,
    title = title
  )
  return(value)
}

#' @title Print message
#' @description Prints a message `msg` if `trace` parameter is `TRUE` with current date.
#'
#' @param msg `string` Message that should be printed.
#' @param trace `bool` Silent printing (`FALSE`) or not (`TRUE`).
#'
#' @return This function returns nothing.
#' @family Utils
#' @keywords internal
#' @noRd
print_message <- function(msg, trace) {
  if (trace) message(paste(date(), msg))
}

#' @title Is shiny app active?
#' @description Check whether the shiny app is active.
#'
#' @return `TRUE` or `FALSE` depending on whether the shiny app is active.
#' @family Utils
#' @keywords internal
#' @noRd
is_shinyapp_active <- function() {
  shiny_available <- requireNamespace("shiny", quietly = TRUE)
  shiny_widgets_available <- requireNamespace("shinyWidgets", quietly = TRUE)
  shiny_running <- shiny::isRunning()

  return(shiny_available && shiny_widgets_available && shiny_running)
}


#' @title Create directory if not exists
#' @description Check whether the passed `dir_path` directory exists. If not, creates a new directory and prints a `msg`
#'   message if `trace` is `TRUE`.
#'
#' @param dir_path `string` A new directory path that should be created.
#' @param trace `bool` Whether a `msg` message should be printed.
#' @param msg `string` A message that should be printed if `trace` is `TRUE`.
#'
#' @return `TRUE` or `FALSE` depending on whether the shiny app is active.
#' @family Utils
#' @keywords internal
#' @noRd
create_dir <- function(dir_path, trace, msg = "") {
  if (!dir.exists(dir_path)) {
    print_message(msg, trace)
    dir.create(dir_path)
  }
}
