#'Installing necessary python modules to an environment
#'
#'Function for installing the necessary python modules
#'
#'@param envname \code{string} Name of the environment where the packages should
#'be installed.
#'@param install \code{character} determining which machine learning frameworks
#'should be installed. \code{install="all"}  for 'pytorch' and 'tensorflow'.
#'\code{install="pytorch"}  for 'pytorch', and \code{install="tensorflow"}  for 'tensorflow'.
#'@param tf_version \code{string} determining the desired version of 'tensorflow'.
#'@param pytorch_cuda_version \code{string} determining the desired version of 'cuda' for
#''PyTorch'.
#'@param python_version \code{string} Python version to use.
#'@param remove_first \code{bool} If \code{TRUE} removes the environment completely before
#'recreating the environment and installing the packages. If \code{FALSE} the packages
#'are installed in the existing environment without any prior changes.
#'@param cpu_only \code{bool} \code{TRUE} installs the cpu only version of the
#'machine learning frameworks.
#'@return Returns no values or objects. Function is used for installing the
#'necessary python libraries in a conda environment.
#'@importFrom reticulate conda_create
#'@importFrom reticulate conda_remove
#'@importFrom reticulate condaenv_exists
#'@importFrom reticulate py_install
#'@importFrom utils compareVersion
#'@family Installation and Configuration
#'@export
install_py_modules<-function(envname="aifeducation",
                             install="pytorch",
                             tf_version="<=2.14",
                             pytorch_cuda_version="12.1",
                             python_version="3.9",
                             remove_first=FALSE,
                             cpu_only=FALSE){
  relevant_modules<-c("transformers",
                      "tokenizers",
                      "pandas",
                      "datasets",
                      "codecarbon")
  relevant_modules_pt<-c("safetensors",
                         "torcheval",
                         "accelerate")

  #Check Arguments
  if(!(install%in%c("all","pytorch","tensorflow"))){
    stop("install must be all, pytorch or tensorflow.")
  }

  if(reticulate::condaenv_exists(envname = envname)==TRUE){
    if(remove_first==TRUE){
      reticulate::conda_remove(envname = envname)
      reticulate::conda_create(
        envname = envname,
        channel=c("conda-forge"),
        python_version = python_version
      )
    }
  } else {
    reticulate::conda_create(
      envname = envname,
      channel=c("conda-forge"),
      python_version = python_version
    )
  }

  #Tensorflow Installation
  if(install=="all" | install=="tensorflow"){
    if(cpu_only==TRUE){
      reticulate::conda_install(
        packages = c(
          "tensorflow-cpu",
          "tf-keras"),
        envname = envname,
        conda = "auto",
        pip = TRUE)
    } else {
      reticulate::conda_install(
        packages = c(
          paste0("tensorflow",tf_version),
          "tf-keras"),
        envname = envname,
        conda = "auto",
        pip = TRUE)

      reticulate::conda_install(
        packages = c(
          "cudatoolkit",
          "cuDNN"),
        envname = envname,
        conda = "auto",
        pip = FALSE)
    }

    reticulate::conda_install(
      packages = relevant_modules,
      envname = envname,
      conda = "auto",
      pip = TRUE
    )
  }

    if(install=="all" | install=="pytorch"){
        reticulate::conda_install(
          packages = c(
            "pytorch",
            paste0("pytorch-cuda","=",pytorch_cuda_version)),
          envname = envname,
          channel=c("pytorch","nvidia"),
          conda = "auto",
          pip = FALSE)

      reticulate::conda_install(
        packages = c(relevant_modules,relevant_modules_pt),
        envname = envname,
        conda = "auto",
        pip = TRUE
      )
    }

}

#'Check if all necessary python modules are available
#'
#'This function checks if all  python modules necessary for the package
#'aifeducation to work are available.
#'@param trace \code{bool} \code{TRUE} if a list with all modules and their
#'availability should be printed to the console.
#'@param check \code{string} determining the machine learning framework to check for.
#'\code{check="pytorch"} for 'pytorch', \code{check="tensorflow"} for 'tensorflow',
#'and \code{check="all"} for both frameworks.
#'@return The function prints a table with all relevant packages and shows
#' which modules are available or unavailable.
#'@return If all relevant modules are available, the functions returns \code{TRUE}.
#'In all other cases it returns \code{FALSE}
#'@family Installation and Configuration
#'@export
check_aif_py_modules<-function(trace=TRUE, check="all"){
  if(!(check%in%c("all","pytorch","tensorflow"))){
    stop("check must be all, pytorch or tensorflow.")
  }

  general_modules=c("os",
                    "transformers",
                    "tokenizers",
                    "datasets",
                    "codecarbon")
  pytorch_modules=c("torch",
                    "torcheval",
                    "safetensors",
                    "accelerate",
                    "pandas")
  tensorflow_modules=c("tensorflow")

  if(check=="all"){
    relevant_modules<-c(general_modules,
                        pytorch_modules,
                        tensorflow_modules)
  } else if(check=="pytorch"){
    relevant_modules<-c(general_modules,
                        pytorch_modules)
  } else if(check=="tensorflow"){
    relevant_modules<-c(general_modules,
                        tensorflow_modules)
  }


  matrix_overview=matrix(data=NA,
                         nrow = length(relevant_modules),
                         ncol= 2)
  colnames(matrix_overview)=c("module","available")
  matrix_overview<-as.data.frame(matrix_overview)
  for(i in 1:length(relevant_modules)){
    matrix_overview[i,1]<-relevant_modules[i]
    matrix_overview[i,2]<-reticulate::py_module_available(relevant_modules[i])
  }

  if(trace==TRUE){
    print(matrix_overview)
  }

  if(sum(matrix_overview[,2])==length(relevant_modules)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#'Setting cpu only for 'tensorflow'
#'
#'This functions configurates 'tensorflow' to use only cpus.
#'@return This function does not return anything. It is used for its
#'side effects.
#'@note os$environ$setdefault("CUDA_VISIBLE_DEVICES","-1")
#'@family Installation and Configuration
#'@export
set_config_cpu_only<-function(){
  os$environ$setdefault("CUDA_VISIBLE_DEVICES","-1")
}

#'Setting gpus' memory usage
#'
#'This function changes the memory usage of the gpus to allow computations
#'on machines with small memory. With this function, some computations of large
#'models may be possible but the speed of computation decreases.
#'@return This function does not return anything. It is used for its
#'side effects.
#'@note This function sets TF_GPU_ALLOCATOR to \code{"cuda_malloc_async"} and
#'sets memory growth to \code{TRUE}.
#'@family Installation and Configuration
#'@export
set_config_gpu_low_memory<-function(){
  os$environ$setdefault("TF_GPU_ALLOCATOR","cuda_malloc_async")
  gpu = tf$config$list_physical_devices("GPU")
  if(length(gpu)>0){
    for(i in 1:length(gpu)){
      tf$config$experimental$set_memory_growth(gpu[[i]], TRUE)
    }
  }
}

#'Sets the level for logging information in tensor flow.
#'
#'This function changes the level for logging information with 'tensorflow'.
#'
#'@param level \code{string} Minimal level that should be printed to console. Five
#'levels are available: FATAL, ERROR, WARN, INFO, and DEBUG.
#'@return This function does not return anything. It is used for its
#'side effects.
#'@family Installation and Configuration
#'@export
set_config_tf_logger<-function(level="ERROR"){
  logger<-tf$get_logger()
  logger$setLevel(level)
}

#'Sets the level for logging information in tensor flow.
#'
#'This function changes the level for logging information with 'tensorflow' via
#'the os environment. This function must be called before importing 'tensorflow'.
#'
#'@param level \code{string} Minimal level that should be printed to console. Four
#'levels are available: INFO, WARNING, ERROR and NONE.
#'@return This function does not return anything. It is used for its
#'side effects.
#'@family Installation and Configuration
#'@export
set_config_os_environ_logger<-function(level="ERROR"){
  if(level=="ERROR"){
    level_int="2"
  } else if (level=="WARNING"){
  level_int="1"
  } else if (level=="INFO"){
    level_int="0"
  } else if(level=="NONE"){
    level_int="3"
  }

  os$environ$setdefault("TF_CPP_MIN_LOG_LEVEL","2")
}

#'Sets the level for logging information of the 'transformers' library.
#'
#'This function changes the level for logging information of the 'transformers' library.
#'It influences the output printed to console for creating and training transformer models as well as
#'\link{TextEmbeddingModel}s.
#'
#'@param level \code{string} Minimal level that should be printed to console. Four
#'levels are available: INFO, WARNING, ERROR and DEBUG
#'@return This function does not return anything. It is used for its
#'side effects.
#'@family Installation and Configuration
#'@export
set_transformers_logger<-function(level="ERROR"){
  if(level=="ERROR"){
    transformers$utils$logging$set_verbosity_error()
  } else if (level=="WARNING"){
    transformers$utils$logging$set_verbosity_warning()
  } else if (level=="INFO"){
    transformers$utils$logging$set_verbosity_info()
  } else if(level=="DEBUG"){
    transformers$utils$logging$set_verbosity_debug()
  }
}

#'R6 class for settting the global machine learning framework.
#'
#'R6 class for setting the global machine learning framework to 'PyTorch' or
#''tensorflow'.
#'
#'@param backend \code{string} Determines the machine learning framework
#'for using with 'keras'. Possible are \code{keras_framework="pytorch"} for 'pytorch',
#'\code{keras_framework="tensorflow"} for 'tensorflow'.
#'@return The function does nothing return. It is used for its side effects.
#'
#'@family Installation and Configuration
AifeducationConfiguration<-R6::R6Class(
  classname = "aifeducationConfiguration",
  private = list(
    ml_framework_config=list(
    global_ml_framework="not_specified",
    TextEmbeddingFramework="not_specified",
    ClassifierFramework="not_specified")
  ),
  public = list(
    #'@description Method for requesting the used machine learning framework.
    #'@return Returns a \code{string} containing the used machine learning framework
    #'for \link{TextEmbeddingModel}s as well as for \link{TextEmbeddingClassifierNeuralNet}.
    get_framework=function(){
      return(private$ml_framework_config)
    },
    #'@description Method for setting the global machine learning framework.
    #'@param backend \code{string} Framework to use for training and inference.
    #'\code{backend="tensorflow"} for 'tensorflow' and \code{backend="pytorch"}
    #'for 'PyTorch'.
    #'@return This method does nothing return. It is used for setting the global
    #'configuration of 'aifeducation'.
    #'@importFrom utils compareVersion
    set_global_ml_backend=function(backend){

      if((backend %in% c("tensorflow","pytorch"))==FALSE) {
        stop("backend must be 'tensorflow' or 'pytorch'.")
      }

      #if(private$TextEmbeddingFramework=="not_specified"){
      #    private$TextEmbeddingFramework=backend
      #    private$ClassifierFramework=backend

      private$ml_framework_config$global_ml_framework=backend
      private$ml_framework_config$TextEmbeddingFramework=backend
      private$ml_framework_config$ClassifierFramework=backend
      os$environ$setdefault("KERAS_BACKEND","tensorflow")
      cat("Global Backend set to:",backend,"\n")

      #} else {
      #  warning("The global machine learning framework has already been set.
      #      If you would like to change the framework please restart the
      #      session and set framework to the desired backend.")
      #}

    },
    #'@description Method for checking if the global ml framework is set.
    #'@return Return \code{TRUE} if the global machine learning framework is set.
    #'Otherwise \code{FALSE}.
    global_framework_set=function(){
      if(private$ml_framework_config$global_ml_framework=="not_specified"){
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  )
)

#' R6 object of class AifeducationConfiguration
#'
#' Object for managing setting the machine learning framework of a session.
#'
#'@family Installation and Configuration
#'@export
aifeducation_config<-NULL
