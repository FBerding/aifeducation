#'Installing necessary python modules to an environment
#'
#'Function for installing the necessary python modules
#'
#'@param envname \code{string} Name of the environment where the packages should
#'be installed.
#'@return Returns no values or objects. Function is used for installing the
#'necessary python libraries in a conda environment.
#'@importFrom reticulate conda_create
#'@importFrom reticulate py_install
#'@family Installation and Configuration
#'@export
install_py_modules<-function(envname="aifeducation"){
  relevant_modules<-c("transformers",
                      "tokenizers",
                      "datasets",
                      "codecarbon",
                      "accelerate")

  reticulate::conda_create(
    envname = envname,
    channel=c("conda-forge")
  )

  reticulate::conda_install(
    packages = c(
      "tensorflow",
      "torch",
      "keras"),
    envname = envname,
    conda = "auto",
    pip = TRUE
  )

  reticulate::conda_install(
    packages = relevant_modules,
    envname = envname,
    conda = "auto",
    pip = TRUE
  )

  py_package_list<-reticulate::py_list_packages(
    envname = envname
  )
  keras_version<-py_package_list[which(py_package_list$package=="keras"),"version"]
  if(keras_version<"3.0.0"){
    reticulate::conda_install(
      packages = "keras-core",
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
#'@return The function prints a table with all relevant packages and shows
#' which modules are available or unavailable.
#'@return If all relevant modules are available, the functions returns \code{TRUE}.
#'In all other cases it returns \code{FALSE}
#'@family Installation and Configuration
#'@export
check_aif_py_modules<-function(trace=TRUE){
  relevant_modules<-c("os",
                      "transformers",
                      "tokenizers",
                      "datasets",
                      "torch",
                      "keras",
                      "tensorflow",
                      "codecarbon")
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
#'@param level \code{string} Minimal level that should be printed to console. Five
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

#'R6 class for settting the global machine learning framework.
#'
#'R6 class for setting the global machine learning framework to 'PyTorch' or
#''tensorflow' depending of the available version of 'keras'.
#'
#'@param backend \code{string} Determines the machine learning framework
#'for using with 'keras'. Possible are \code{keras_framework="pytorch"} for 'pytorch',
#'\code{keras_framework="tensorflow"} for 'tensorflow'.
#'@return The function does nothing return. It is used for its side effects.
#'@note This function must be called directly after loading 'aifeducation' to take effect.
#'@note Please note that using classifier objects with 'PyTorch' requires keras of
#'at least version 3. If you have an older version 'tensorflow' is used.
#'
#'@family Installation and Configuration
#'@keywords internal
#'
AifeducationConfiguration<-R6::R6Class(
  classname = "aifeducationConfiguration",
  private = list(
    TextEmbeddingFramework="not_specified",
    ClassifierFramework="not_specified"
  ),
  public = list(
    #'@description Method for requesting the used machine learning framework.
    #'@return Returns a \code{list} containing the used machine learning framework
    #'for \link{TextEmbeddingModel}s as well as for \link{TextEmbeddingClassifierNeuralNet}.
    get_framework=function(){
      return(
        list(private$TextEmbeddingFramework,
             private$ClassifierFramework))
    },
    #'@description Method for setting machine learning framework.
    #'@param backend \code{string} Framework to use for training and inference.
    #'\code{backend="tensorflow"} for 'tensorflow' and \code{backend="pytorch"}
    #'for 'PyTorch'.
    #'@return This method does nothing return. It is used for setting the global
    #'configuration of 'aifeducation'.
    set_global_ml_backend=function(backend){

      if((backend %in% c("tensorflow","pytorch"))==FALSE) {
        stop("backend must be 'tensorflow' or 'pytorch'.")
      }

      if(private$TextEmbeddingFramework=="not_specified"){
        if(keras["__version__"]>="2.4.0" & keras["__version__"]<"3.0.0"){
          private$TextEmbeddingFramework=backend
          private$ClassifierFramework="tensorflow"
          os$environ$setdefault("KERAS_BACKEND","tensorflow")

          cat("Backend for TextEmbeddingModels:",private$TextEmbeddingFramework,"\n")
          cat("Backend for Classifiers:",private$ClassifierFramework,"\n")

        } else if(keras["__version__"]>="3.0.0"){
          private$TextEmbeddingFramework=backend
          private$ClassifierFramework=backend
          os$environ$setdefault("KERAS_BACKEND",backend)

          cat("Backend for TextEmbeddingModels:",private$TextEmbeddingFramework,"\n")
          cat("Backend for Classifiers:",private$ClassifierFramework,"\n")

        } else if(keras["__version__"]<"2.4.0" & reticulate::py_module_available("keras-core")){
          private$TextEmbeddingFramework=backend
          private$ClassifierFramework=backend
          os$environ$setdefault("KERAS_BACKEND",backend)

          cat("Backend for TextEmbeddingModels:",private$TextEmbeddingFramework,"\n")
          cat("Backend for Classifiers:",private$ClassifierFramework,"\n")

        } else {
          stop("No compatible version of keras found.")
        }
      } else {
        warning("The global machine learning framework has already been set.
            If you would like to change the framework please restart the
            session and set framework to the desired backend.")
      }
    }
  )
)
