#'Loading models created with 'aifeducation'
#'
#'Function for loading models created with 'aifeducation'.
#'
#'@param model_dir Path to the directory where the model is stored.
#'@param ml_framework \code{string} Determines the machine learning framework
#'for using the model. Possible are \code{ml_framework="pytorch"} for 'pytorch',
#'\code{ml_framework="tensorflow"} for 'tensorflow', and \code{ml_framework="auto"}.
#'for using the framework used when saving the model.
#'@return Returns an object of class \link{TextEmbeddingClassifierNeuralNet} or
#'\link{TextEmbeddingModel}.
#'
#'@family Saving and Loading
#'
#'@importFrom utils compareVersion
#'
#'@export
load_ai_model<-function(model_dir,ml_framework=aifeducation_config$get_framework()){

  if((ml_framework %in%c("pytorch","tensorflow","auto","not_specified"))==FALSE){
    stop("ml_framework must be 'tensorflow', 'pytorch' or 'auto'.")
  }

  if(ml_framework=="not_specified"){
    stop("The global machine learning framework is not set. Please use
             aifeducation_config$set_global_ml_backend() directly after loading
             the library to set the global framework. ")
  }

  #Load the Interface to R
  interface_path=paste0(model_dir,"/r_interface.rda")

  if(file.exists(interface_path)==TRUE){
    name_interface<-load(interface_path)

    loaded_model<-get(x=name_interface)

    if(methods::is(loaded_model,"TextEmbeddingClassifierNeuralNet")){
      aifeducation_version<-loaded_model$get_package_versions()[[1]]$aifeducation
    } else {
      aifeducation_version<-loaded_model$get_package_versions()$aifeducation
    }

    #For aifeducation 0.2.0 and lower-----------------------------------------
    if(utils::compareVersion(as.character(aifeducation_version),"0.2.0")<=0){
      if(methods::is(loaded_model,"TextEmbeddingClassifierNeuralNet")){
        loaded_model$load_model(model_dir)
      } else if (methods::is(loaded_model,"TextEmbeddingModel")){
        if(loaded_model$get_model_info()$model_method%in%c("glove_cluster","lda")==FALSE){
          loaded_model$load_model(model_dir)
        }
      }
      return(loaded_model)
    } else {
      #For aifeducation 0.2.1 and higher-----------------------------------------
      if(methods::is(loaded_model,"TextEmbeddingClassifierNeuralNet")){
        loaded_model$load_model(
          dir_path=model_dir,
          ml_framework=ml_framework)
      } else if (methods::is(loaded_model,"TextEmbeddingModel")){
        if(loaded_model$get_model_info()$model_method%in%c("glove_cluster","lda")==FALSE){
          loaded_model$load_model(
            model_dir=model_dir,
            ml_framework=ml_framework)
        }
      }
      return(loaded_model)
    }
  } else {
    stop("There is no file r_interface.rda in the selected directory")
  }
}

#'Saving models created with 'aifeducation'
#'
#'Function for saving models created with 'aifeducation'.
#'
#'@param model Object of class \link{TextEmbeddingClassifierNeuralNet} or
#'\link{TextEmbeddingModel} which should be saved.
#'@param model_dir Path to the directory where the should model is stored.
#'@param dir_name Name of the folder that will be created at \code{model_dir}.
#'If\code{dir_name=NULL} the model's name will be used. If additionally \code{append_ID=TRUE}
#'the models's name and ID will be used for generating a name for that directory.
#'@param save_format Only relevant for \link{TextEmbeddingClassifierNeuralNet}.
#'Format for saving the model. For 'tensorflow'/'keras' models
#'\code{"keras"} for 'Keras v3 format',
#'\code{"tf"} for SavedModel
#'or \code{"h5"} for HDF5.
#'For 'pytorch' models \code{"safetensors"} for 'safetensors' or
#'\code{"pt"} for 'pytorch via pickle'.
#'Use \code{"default"} for the standard format. This is keras for
#''tensorflow'/'keras' models and safetensors for 'pytorch' models.
#'@return Function does not return a value. It saves the model to disk.
#'@param append_ID \code{bool} \code{TRUE} if the ID should be appended to
#'the model directory for saving purposes. \code{FALSE} if not.
#'@return No return value, called for side effects.
#'
#'@family Saving and Loading
#'
#'@export
save_ai_model<-function(model,
                        model_dir,
                        dir_name=NULL,
                        save_format="default",
                        append_ID=TRUE){
  if(methods::is(model,"TextEmbeddingClassifierNeuralNet") |
     methods::is(model,"TextEmbeddingModel")){

    if(is.null(dir_name)){
      if(append_ID==TRUE){
        final_model_dir_path=paste0(model_dir,"/",model$get_model_info()$model_name)
      } else {
        final_model_dir_path=paste0(model_dir,"/",model$get_model_info()$model_name_root)
      }
    } else {
      final_model_dir_path=paste0(model_dir,"/",dir_name)
    }


    if(dir.exists(final_model_dir_path)==FALSE){
      dir.create(final_model_dir_path)
    }

    #Save R Interface------------------------------
    save(model,file = paste0(final_model_dir_path,"/r_interface.rda"))

    #---------------------
    if(methods::is(model,"TextEmbeddingClassifierNeuralNet")){
      model$save_model(dir_path = final_model_dir_path,save_format=save_format)
    } else {
      #TextEmbeddingModels
      if(model$get_model_info()$model_method%in%c("glove_cluster","lda")==FALSE){
        model$save_model(model_dir = final_model_dir_path)
      }
    }
  } else {
    stop("Function supports only objects of class TextEmbeddingClassifierNeuralNet or
         TextEmbeddingModel")
  }
}
