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

#' @title Aifeducation Studio
#' @description Functions starts a shiny app that represents Aifeducation Studio.
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
  # future::plan(future::multisession)

  # Create App------------------------------------------------------------------
  shiny::shinyAppDir(
    appDir = system.file("studio_app",
      package = "aifeducation"
    )
  )
}
