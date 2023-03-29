tf<-NULL
os<-NULL
transformers<-NULL
datasets<-NULL
tok<-NULL
np<-NULL

.onLoad<-function(libname, pkgname){
  # use superassignment to update global reference
  transformers<<-reticulate::import("transformers", delay_load = TRUE)
  datasets<<-reticulate::import("datasets", delay_load = TRUE)
  tok<<-reticulate::import("tokenizers", delay_load = TRUE)
  np<<-reticulate::import("numpy", delay_load = TRUE)
  tf<<-reticulate::import("tensorflow", delay_load = TRUE)
  os<<-reticulate::import("os", delay_load = TRUE)
}


