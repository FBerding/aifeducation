#' @title Graphical user interface for displaying train and tune settings of the base model.
#' @description Functions generates the tab within a page for displaying train and tune settings of the base model.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_base_model_train_card_train_tune_settings
#' @keywords internal
#'
TrainTuneSettings_UI <- function(id) {
  bslib::page(
    shiny::uiOutput(outputId = "train_tune_settings")
  )
}
