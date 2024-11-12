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

#' @title Show save modal
#' @description Function for displaying a modal that reports errors to the user.
#'
#' @param id `string` Namespace id for the input and output elements.
#' @param ns `function` for setting the correct namespace of the input and output elements.
#' @param title `string` Title of the modal.
#' @param easy_close `bool` If `TRUE`, the modal dialog can be dismissed by clicking outside the dialog box, or be
#'   pressing the Escape key. If `FALSE` the modal must be dismissed by clicking on a modalButton or from a call
#'   removeModal on the server.
#' @param size `string` Size of the modal. Possible are `"m"`, `"s"`, `"l"`, and `"xl"`.
#'
#' @details If `ns` is `NULL` the function uses the argument `id`. In all other cases `ns` is used for setting the
#' correct ids and namespaces. The application of `id` or `ns` depends on the position of the app where the modal should
#' be used.
#'
#' The events have to be implemented manually in the corresponding server function.
#'
#' @return Function returns a shiny modal.
#'
#' @family studio_modals
#' @keywords internal
#' @noRd
#'
create_save_modal <- function(id,
                              ns = NULL,
                              title = "Choose Destination",
                              easy_close = FALSE,
                              size = "l") {
  str_dir <- "save_modal_directory_path"
  str_folder <- "save_modal_folder_name"
  str_btn <- "save_modal_button_continue"
  if (!is.null(ns)) {
    ids <- c(ns(str_dir), ns(str_folder), ns(str_btn))
  } else {
    ids <- c(shiny::NS(id, str_dir), shiny::NS(id, str_folder), shiny::NS(id, str_btn))
  }

  ui <- shiny::tagList(
    shiny::textInput(
      inputId = ids[1],
      label = shiny::tags$p(shiny::icon("folder"), "Directory")
    ),
    shiny::textInput(
      inputId = ids[2],
      label = shiny::tags$p(shiny::icon("file"), "Folder Name")
    ),
    shiny::actionButton(
      inputId = ids[3],
      label = "Continue",
      icon = shiny::icon("paper-plane")
    )
  )

  modal <- shiny::modalDialog(
    title = title,
    easyClose = easy_close,
    size = size,
    ui,
    footer = shiny::modalButton("Cancel")
  )
  return(modal)
}
