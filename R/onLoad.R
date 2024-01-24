tf<-NULL
transformers<-NULL
datasets<-NULL
tok<-NULL
np<-NULL
codecarbon<-NULL
torch<-NULL
torcheval<-NULL
os<-NULL
keras<-NULL
accelerate<-NULL
safetensors<-NULL

aifeducation_config<-NULL

py_update_aifeducation_progress_bar_epochs<-NULL
py_update_aifeducation_progress_bar_steps<-NULL

.onLoad<-function(libname, pkgname){
  # use superassignment to update the global reference
  os<<-reticulate::import("os", delay_load = TRUE)
  transformers<<-reticulate::import("transformers", delay_load = TRUE)
  datasets<<-reticulate::import("datasets", delay_load = TRUE)
  tok<<-reticulate::import("tokenizers", delay_load = TRUE)
  np<<-reticulate::import("numpy", delay_load = TRUE)
  tf<<-reticulate::import("tensorflow", delay_load = TRUE)
  torch<<-reticulate::import("torch", delay_load = TRUE)
  torcheval<<-reticulate::import("torcheval", delay_load = TRUE)
  accelerate<<-reticulate::import("accelerate", delay_load = TRUE)
  safetensors<<-reticulate::import("safetensors", delay_load = TRUE)

  codecarbon<<-reticulate::import("codecarbon", delay_load = TRUE)
  keras<<-reticulate::import("keras", delay_load = TRUE)

  aifeducation_config<<-AifeducationConfiguration$new()

}


