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

#' @title Is shiny app active?
#' @description Check whether the shiny app is active.
#'
#' @return `TRUE` or `FALSE` depending on whether the shiny app is active.
#' @family Auxiliary GUI Functions
#' @keywords internal
#' @noRd
is_shinyapp_active <- function() {
  .Deprecated(msg = "`is_shinyapp_active()` function will be removed from `aifeduction` package in the future")

  shiny_available <- requireNamespace("shiny", quietly = TRUE)
  shiny_widgets_available <- requireNamespace("shinyWidgets", quietly = TRUE)
  shiny_running <- shiny::isRunning()

  return(shiny_available && shiny_widgets_available && shiny_running)
}
