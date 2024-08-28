#' @title Is shiny app active?
#' @description Check whether the shiny app is active.
#'
#' @return `TRUE` or `FALSE` depending on whether the shiny app is active.
#' @family Auxiliary GUI Functions
#' @keywords internal
#' @noRd
is_shinyapp_active <- function() {
  shiny_available <- requireNamespace("shiny", quietly = TRUE)
  shiny_widgets_available <- requireNamespace("shinyWidgets", quietly = TRUE)
  shiny_running <- shiny::isRunning()

  return(shiny_available && shiny_widgets_available && shiny_running)
}

#' @title Update a progress bar in aifeducation shiny app by identifier
#' @description This function updates a progress bar in aifeducation shiny app by identifier. The progress bar reports
#'   the current state of the overall process.
#'
#' @param id `string` An identifier used to update the progress bar.
#' @param value `int` Value describing the current step of the process.
#' @param total `int` Total number of steps of the process.
#' @param title `string` Title displaying in the top of the progress bar.
#' @return Function does nothing returns. It updates the progress bar with the passed identifier `id`.
#'
#' @family Auxiliary GUI Functions
#' @keywords internal
#' @noRd
update_progress_bar <- function(id, value, total, title) {
  shinyWidgets::updateProgressBar(
    id = id,
    value = value,
    total = total,
    title = title
  )
}

#' @title Update master progress bar in aifeducation shiny app
#' @description This function updates the master progress bar in aifeducation shiny app. The progress bar reports the
#'   current state of the overall process.
#'
#' @param value `int` Value describing the current step of the process.
#' @param total `int` Total number of steps of the process.
#' @param title `string` Title displaying in the top of the progress bar.
#' @return Function does nothing returns. It updates the progress bar with the id `"pgr_bar_aifeducation"`.
#'
#' @family Auxiliary GUI Functions
#' @export
update_aifeducation_progress_bar <- function(value, total, title = NULL) {
  if (is_shinyapp_active()) update_progress_bar("pgr_bar_aifeducation", value, total, title)
}

#' @title Update epoch progress bar in aifeducation shiny app
#' @description This function updates the epoch progress bar in aifeducation shiny app. The progress bar reports the
#'   current state of the overall process.
#'
#' @param value `int` Value describing the current step of the process.
#' @param total `int` Total number of steps of the process.
#' @param title `string` Title displaying in the top of the progress bar.
#' @return Function does nothing returns. It updates the progress bar with the id `"pgr_bar_aifeducation_epochs"`.
#'
#' @details This function is called very often during training a model. Thus, the function does not check the
#'   requirements for updating the progress bar to reduce computational time. The check for fulfilling the necessary
#'   conditions must be implemented separately.
#'
#' @family Auxiliary GUI Functions
#' @export
update_aifeducation_progress_bar_epochs <- function(value, total, title = NULL) {
  update_progress_bar("pgr_bar_aifeducation_epochs", value, total, title)
}

#' @title Update step/batch progress bar in aifeducation shiny app
#' @description This function updates the step/batch progress bar in aifeducation shiny app. The progress bar reports
#'   the current state of the overall process.
#'
#' @param value `int` Value describing the current step of the process.
#' @param total `int` Total number of steps of the process.
#' @param title `string` Title displaying in the top of the progress bar.
#' @return Function does nothing returns. It updates the progress bar with the id `"pgr_bar_aifeducation_steps"`.
#'
#' @details This function is called very often during training a model. Thus, the function does not check the
#'   requirements for updating the progress bar to reduce computational time. The check for fulfilling the necessary
#'   conditions must be implemented separately.
#'
#' @family Auxiliary GUI Functions
#' @export
update_aifeducation_progress_bar_steps <- function(value, total, title = NULL) {
  update_progress_bar("pgr_bar_aifeducation_steps", value, total, title)
}

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