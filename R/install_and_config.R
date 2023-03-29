#'Installing necessary python modules to an environment
#'
#'Function for installing the necessary python modules
#'
#'@param envname \code{string} Name of the environment where the packages should
#'be installed.
#'@export
install_py_modules<-function(envname=NULL){
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
#------------------------------------------------------------------------------
#'Check if all necessary python modules are available
#'
#'This function checks if all necessary python modules are available for
#'the package aifeducation to work
#'@return The function prints a table with all relevant packages and shows
#' which modules are available or not.
#'@return If all relevant modules are available the functions returns \code{TRUE}.
#'In all other cases \code{FALSE}
#'@export
check_aif_py_modules<-function(){
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
  print(matrix_overview)
  if(sum(matrix_overview[,2])==length(relevant_modules)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
