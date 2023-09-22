tf<-NULL
transformers<-NULL
datasets<-NULL
tok<-NULL
np<-NULL
codecarbon<-NULL
torch<-NULL
os<-NULL
keras<-NULL

.onLoad<-function(libname, pkgname){
  # use superassignment to update the global reference
  os<<-reticulate::import("os", delay_load = TRUE)
  transformers<<-reticulate::import("transformers", delay_load = TRUE)
  datasets<<-reticulate::import("datasets", delay_load = TRUE)
  tok<<-reticulate::import("tokenizers", delay_load = TRUE)
  np<<-reticulate::import("numpy", delay_load = TRUE)
  tf<<-reticulate::import("tensorflow", delay_load = TRUE)
  torch<<-reticulate::import("torch", delay_load = TRUE)
  codecarbon<<-reticulate::import("codecarbon", delay_load = TRUE)

  py_package_list<-reticulate::py_list_packages()
  keras_version<-py_package_list[which(py_package_list$package=="keras"),"version"]
  keras_core<-py_package_list[which(py_package_list$package=="keras-core"),"version"]

  if(keras_version<"3.0.0" & identical(x=keras_core,y=character(0))==FALSE &Sys.info()["sysname"]!="Windows"){
    keras<<-reticulate::import("keras-core", delay_load = TRUE)
  } else {
    keras<<-reticulate::import("keras", delay_load = TRUE)
  }

}


