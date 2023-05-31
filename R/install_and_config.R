#'Installing necessary python modules to an environment
#'
#'Function for installing the necessary python modules
#'
#'@param envname \code{string} Name of the environment where the packages should
#'be installed.
#'@export
install_py_modules<-function(envname="aifeducation"){
  relevant_modules<-c("os",
                      "transformers",
                      "tokenizers",
                      "datasets",
                      "torch",
                      "keras",
                      "tensorflow")
  reticulate::py_install(
    envname=envname,
    method = "auto",
    pip=TRUE,
    packages=relevant_modules)
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
#'@export
check_aif_py_modules<-function(trace=TRUE){
  relevant_modules<-c("os",
                      "transformers",
                      "tokenizers",
                      "datasets",
                      "torch",
                      "keras",
                      "tensorflow")
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


#'Setting cpu only for tensorflow
#'
#'This functions configurates tensorflow to use only cpus.
#'@return This function does not return anything. It is used for its
#'side effects.
#'@note os$environ$setdefault("CUDA_VISIBLE_DEVICES","-1")
#'@export
set_config_cpu_only<-function(){
  os$environ$setdefault("CUDA_VISIBLE_DEVICES","-1")
}

#'Setting gpus' memory usage
#'
#'This function changes the memory usage of the gpus to allow computations
#'on machines with small memory. With this function, some computations of large
#'models may be possible but the speed of computation decreases.
#'#'@return This function does not return anything. It is used for its
#'side effects.
#'@note This function sets TF_GPU_ALLOCATOR to \code{"cuda_malloc_async"} and
#'sets memory growth to \code{TRUE}.
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
#'This function changes the level for logging information with tensorflow.
#'
#'@param level \code{string} Minimal level that should be printed to console. Five
#'levels are available: FATAL, ERROR, WARN, INFO, and DEBUG.
#'@return This function does not return anything. It is used for its
#'side effects.
#'@export
set_config_tf_logger<-function(level="ERROR"){
  logger<-tf$get_logger()
   logger$setLevel(level)
}

#'Sets the level for logging information in tensor flow.
#'
#'This function changes the level for logging information with tensorflow via
#'the os environment. This function must be called before importing tensorflow.
#'
#'@param level \code{string} Minimal level that should be printed to console. Five
#'levels are available: INFO, WARNING, ERROR and NONE.
#'@return This function does not return anything. It is used for its
#'side effects.
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
