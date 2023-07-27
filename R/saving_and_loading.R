#'Loading models created with aifeducation
#'
#'Function for loading models created with aifeducation.
#'
#'@param model_dir Path to the directory where the model is stored.
#'
#'@family Saving and Loading
#'
#'@export
load_ai_model<-function(model_dir){
  #Load the Interface to R
  interface_path=paste0(model_dir,"/r_interface.rda")

  if(file.exists(interface_path)==TRUE){
    name_interface<-load(interface_path)

    loaded_model<-get(x=name_interface)

    if(methods::is(loaded_model,"TextEmbeddingClassifierNeuralNet")){
      loaded_model$load_model(model_dir)
    } else if (methods::is(loaded_model,"TextEmbeddingModel")){
      if(loaded_model$basic_components$method%in%c("glove_cluster","lda")==FALSE){
        loaded_model$load_model(model_dir)
      }
    }
    return(loaded_model)
  } else {
    stop("There is no file r_interface.rda in the selected directory")
  }
}

#'Saving models created with aifeducation
#'
#'Function for saving models created with aifeducation.
#'@param model Object of class \link{TextEmbeddingClassifierNeuralNet} or
#'\link{TextEmbeddingModel} which should be saved.
#'@param model_dir Path to the directory where the should model is stored.
#'@param save_format Format for saving the model. \code{"tf"} for SavedModel
#'or \code{"h5"} for HDF5. Only relevant if the model is of class \link{TextEmbeddingClassifierNeuralNet}.
#'It is recommended to use \code{"tf"}.
#'
#'@family Saving and Loading
#'
#'@export
save_ai_model<-function(model,model_dir,save_format="tf"){
  if(methods::is(model,"TextEmbeddingClassifierNeuralNet") |
     methods::is(model,"TextEmbeddingModel")){

    final_model_dir_path=paste0(model_dir,"/",model$get_model_info()$model_name)

    if(dir.exists(final_model_dir_path)==FALSE){
      dir.create(final_model_dir_path)
    }

    save(model,file = paste0(final_model_dir_path,"/r_interface.rda"))

    if(methods::is(model,"TextEmbeddingClassifierNeuralNet")){
      model$save_model(dir_path = final_model_dir_path,save_format=save_format)
    } else {
      if(model$basic_components$method%in%c("glove_cluster","lda")==FALSE){
        model$save_model(model_dir = final_model_dir_path)
      }
    }
  } else {
    stop("Function supports only objects of class TextEmbeddingClassifierNeuralNet or
         TextEmbeddingModel")
  }
}
