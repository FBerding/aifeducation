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
#'@family Saving and Loading Deprecated
#'
#'@importFrom utils compareVersion
#'
#'@export
load_ai_model<-function(model_dir,ml_framework="pytorch"){

  if((ml_framework %in%c("pytorch","tensorflow","auto"))==FALSE){
    stop("ml_framework must be 'tensorflow', 'pytorch' or 'auto'.")
  }

  #Load the Interface to R
  interface_path=paste0(model_dir,"/r_interface.rda")

  if(file.exists(interface_path)==TRUE){
    name_interface<-load(interface_path)

    loaded_model<-get(x=name_interface)

    if(methods::is(loaded_model,"TextEmbeddingClassifierNeuralNet")){
      aifeducation_version<-loaded_model$get_package_versions()[[1]]$aifeducation
    } else if(methods::is(loaded_model,"TEClassifierRegular")|
              methods::is(loaded_model,"TEClassifierProtoNet")){
      aifeducation_version<-loaded_model$get_package_versions()$r_package_versions$aifeducation
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
      if(methods::is(loaded_model,"TextEmbeddingClassifierNeuralNet")|
         methods::is(loaded_model,"TEClassifierRegular")|
         methods::is(loaded_model,"TEClassifierProtoNet")){
        loaded_model$load_model(
          dir_path=model_dir,
          ml_framework=ml_framework)
        if(         methods::is(loaded_model,"TEClassifierRegular")|
                    methods::is(loaded_model,"TEClassifierProtoNet")){
          if(loaded_model$model_config$use_fe==TRUE){
            loaded_model$feature_extractor$load_model(
              dir_path=paste0(model_dir,"/feature_extractor"),
              ml_framework=ml_framework
            )
          }
        }

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
#'@family Saving and Loading Deprecated
#'
#'@export
save_ai_model<-function(model,
                        model_dir,
                        dir_name=NULL,
                        save_format="auto",
                        append_ID=TRUE){
  if(methods::is(model,"TextEmbeddingClassifierNeuralNet")|
     methods::is(model,"TEClassifierRegular")|
     methods::is(model,"TEClassifierProtoNet") |
     methods::is(model,"TextEmbeddingModel")){

    #Check for valid save formats----------------------------------------------
    if(methods::is(model,"TextEmbeddingClassifierNeuralNet")|
       methods::is(model,"TEClassifierRegular")|
       methods::is(model,"TEClassifierProtoNet")){
      if(model$get_ml_framework()=="pytorch"){
        if(save_format%in%c("auto","default","pt","safetensors")==FALSE){
          stop("For classifiers based on 'pytorch' only 'pt' and 'safetensors' are
          valid save formats.")
        }
      } else if(model$get_ml_framework()=="tensorflow"){
        if(save_format%in%c("auto","default","h5","tf","keras")==FALSE){
          stop("For classifiers based on 'tensorflow' only 'h5', 'tf', and 'keras' are
          valid save formats.")
        }
      }
    } else if(methods::is(model,"TextEmbeddingModel")){
      if(model$get_model_info()$model_method%in%c("glove_cluster","lda")==FALSE){
        if(model$get_ml_framework()=="pytorch"){
          if(save_format%in%c("auto","default","pt","safetensors")==FALSE){
            stop("For TextEmbeddingModels based on 'pytorch' only 'pt' and 'safetensors' are
          valid save formats.")
          }
        } else if(model$get_ml_framework()=="tensorflow"){
          if(save_format%in%c("auto","default","tf","keras")==FALSE){
            stop("For TextEmbeddingModels based on 'tensorflow' only 'h5' is a
          valid save format.")
          }
        }
      }
    }


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

    #Get Package Version
    if(methods::is(model,"TextEmbeddingClassifierNeuralNet")){
      aifeducation_version<-model$get_package_versions()[[1]]$aifeducation
    } else if(methods::is(model,"TEClassifierRegular")|
              methods::is(model,"TEClassifierProtoNet")){
      aifeducation_version<-model$get_package_versions()$r_package_versions$aifeducation
    } else {
      aifeducation_version<-model$get_package_versions()$aifeducation
    }

    #Save ML-Model---------------------
    if(utils::compareVersion(as.character(aifeducation_version),"0.3.0")<=0){
      if(methods::is(model,"TextEmbeddingClassifierNeuralNet")){
        model$save_model(dir_path = final_model_dir_path,
                         save_format=save_format)
      } else {
        #TextEmbeddingModels
        if(model$get_model_info()$model_method%in%c("glove_cluster","lda")==FALSE){
          model$save_model(model_dir = final_model_dir_path)
        }
      }
    } else {
      if(methods::is(model,"TextEmbeddingClassifierNeuralNet")|
         methods::is(model,"TEClassifierRegular")|
         methods::is(model,"TEClassifierProtoNet")){
        model$save_model(dir_path = final_model_dir_path,
                         save_format=save_format)
        if(methods::is(model,"TEClassifierRegular")|
           methods::is(model,"TEClassifierProtoNet")){
          if(model$model_config$use_fe==TRUE){
            if(dir.exists(paste0(final_model_dir_path,"/feature_extractor"))==FALSE){
              dir.create(paste0(final_model_dir_path,"/feature_extractor"))
            }
            model$feature_extractor$save_model(
              dir_path = paste0(final_model_dir_path,"/feature_extractor"),
              save_format=save_format
            )
          }
        }
      } else {
        #TextEmbeddingModels
        if(model$get_model_info()$model_method%in%c("glove_cluster","lda")==FALSE){
          model$save_model(model_dir = final_model_dir_path,
                           save_format=save_format)
        }
      }
    }


  } else {
    stop("Function supports only objects of class TextEmbeddingClassifierNeuralNet or
         TextEmbeddingModel")
  }
}
