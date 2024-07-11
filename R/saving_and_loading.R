#'Saving objects created with 'aifeducation'
#'
#'Function for saving objects created with 'aifeducation'.
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
save_to_disk<-function(object,
                        model_dir,
                        folder_name){
  #Check class of object

  #Create path to save location
  save_location=paste0(model_dir,"/",folder_name)

  #Create path to r_interface
  path_r_interface=paste0(save_location,"/","r_interface.rda")

  #Check directory
  if(dir.exists(model_dir)==FALSE){
    dir.create(model_dir)
  }
  if(dir.exists(save_location)==FALSE){
    dir.create(save_location)
  }

  object$save(model_dir=model_dir,
              folder_name=folder_name)
  save(object,file = path_r_interface)
  if(methods::is(object,"TEClassifierRegular")|
     methods::is(object,"TEClassifierProtoNet")){

    #save feature extractor if part of the model
    if(object$model_config$use_fe==TRUE){
      object$feature_extractor$save(
        model_dir=save_location,
        folder_name="feature_extractor")
    }
  }
}






#'Loading objects created with 'aifeducation'
#'
#'Function for loading objects created with 'aifeducation'.
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
load_from_disk<-function(model_dir){

  #Load the Interface to R
  interface_path=paste0(model_dir,"/r_interface.rda")

  #Check for r_interface.rda
  if(file.exists(interface_path)==FALSE){
    stop("There is no file r_interface.rda in the selected directory")
  }

  #Load interface
    name_interface<-load(interface_path)
    loaded_object<-get(x=name_interface)

    if(methods::is(loaded_object,"TEClassifierRegular")|
       methods::is(loaded_object,"TEClassifierProtoNet")){

      #Call load method
      loaded_object$load(model_dir)

      #Load feature extractor if part of the model
      if(loaded_object$model_config$use_fe==TRUE){
        loaded_object$feature_extractor$load(
          model_dir=paste0(model_dir,"/feature_extractor"))
      }
    } else if(methods::is(loaded_object,"TEFeatureExtractor")){
      loaded_object$load(model_dir=model_dir)

    } else if(methods::is(loaded_object,"TextEmbeddingModel")){
      if(loaded_object$get_model_info()$model_method%in%c("glove_cluster","lda")==FALSE){
        loaded_object$load(model_dir=model_dir)
      }
    }

    return(loaded_object)

}

