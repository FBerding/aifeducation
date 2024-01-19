#'Update master progress bar in aifeducation shiny app.
#'
#'This function updates the master progress bar in aifeducation shiny app. The
#'progress bar reports the current state of the overall process.
#'
#'@param value \code{int} Value describing the current step of the process.
#'@param total \code{int} Total number of steps of the process.
#'@param title \code{string} Title displaying in the top of the progress bar.
#'@return Function does nothing returns. It updates the progress bar with the id
#'\code{"pgr_bar_aifeducation"}.
#'
#'@family Auxiliary GUI Functions
#'@keywords internal
update_aifeducation_progress_bar<-function(value,total,title=NULL){
  if(requireNamespace("shiny") & requireNamespace("shinyWidgets")){
    if(shiny::isRunning()){
      updateProgressBar(id="pgr_bar_aifeducation",
                        value = value,
                        total=total,
                        title = title)
    }
  }
}


#'Update epoch progress bar in aifeducation shiny app.
#'
#'This function updates the epoch progress bar in aifeducation shiny app. The
#'progress bar reports the current state of the overall process.
#'
#'@param value \code{int} Value describing the current step of the process.
#'@param total \code{int} Total number of steps of the process.
#'@param title \code{string} Title displaying in the top of the progress bar.
#'@return Function does nothing returns. It updates the progress bar with the id
#'\code{"pgr_bar_aifeducation_epochs"}.
#'
#'@details This function is called very often during training a model. Thus, the
#'function does not check the requirements for updating the progress bar to reduce
#'computational time. The check for fulfilling the necessary conditions must be
#'implemented separately.
#'
#'@family Auxiliary GUI Functions
#'@keywords internal
update_aifeducation_progress_bar_epochs<-function(value,total,title=NULL){
      updateProgressBar(id="pgr_bar_aifeducation_epochs",
                        value = value,
                        total=total,
                        title = title)
}


#'Update step/batch progress bar in aifeducation shiny app.
#'
#'This function updates the step/batch progress bar in aifeducation shiny app. The
#'progress bar reports the current state of the overall process.
#'
#'@param value \code{int} Value describing the current step of the process.
#'@param total \code{int} Total number of steps of the process.
#'@param title \code{string} Title displaying in the top of the progress bar.
#'@return Function does nothing returns. It updates the progress bar with the id
#'\code{"pgr_bar_aifeducation_steps"}.
#'
#'@details This function is called very often during training a model. Thus, the
#'function does not check the requirements for updating the progress bar to reduce
#'computational time. The check for fulfilling the necessary conditions must be
#'implemented separately.
#'
#'@family Auxiliary GUI Functions
#'@keywords internal
update_aifeducation_progress_bar_steps<-function(value,total,title=NULL){
      updateProgressBar(id="pgr_bar_aifeducation_steps",
                        value = value,
                        total=total,
                        title = title)

}


