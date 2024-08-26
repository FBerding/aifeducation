#' Aifeducation Studio
#'
#' Functions starts a shiny app that represents Aifeducation Studio
#'
#' @return This function does nothing return. It is used to start a shiny app.
#'
#' @family Graphical User Interface
#'
#' @export
start_aifeducation_studio <- function() {
  # Prepare for studio
  check_and_prepare_for_studio()

  # Set up for long running tasks
  future::plan(future::multisession)

  # Create App------------------------------------------------------------------
  shiny::shinyAppDir(
    appDir = system.file("studio_app",
      package = "aifeducation"
    )
  )
}
