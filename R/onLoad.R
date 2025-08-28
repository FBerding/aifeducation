tf <- NULL
transformers <- NULL
datasets <- NULL
tok <- NULL
np <- NULL
codecarbon <- NULL
torch <- NULL
torcheval <- NULL
os <- NULL
keras <- NULL
accelerate <- NULL
safetensors <- NULL
pandas <- NULL
pyarrow <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update the global reference
  os <<- reticulate::import("os", delay_load = TRUE)
  transformers <<- reticulate::import("transformers", delay_load = TRUE)
  datasets <<- reticulate::import("datasets", delay_load = TRUE)
  tok <<- reticulate::import("tokenizers", delay_load = TRUE)
  np <<- reticulate::import("numpy", delay_load = TRUE)
  torch <<- reticulate::import("torch", delay_load = TRUE)
  torcheval <<- reticulate::import("torcheval", delay_load = TRUE)
  accelerate <<- reticulate::import("accelerate", delay_load = TRUE)
  safetensors <<- reticulate::import("safetensors", delay_load = TRUE)
  pandas <<- reticulate::import("pandas", delay_load = TRUE)
  pyarrow <<- reticulate::import("pyarrow", delay_load = TRUE)
  codecarbon <<- reticulate::import("codecarbon", delay_load = TRUE)
  #calflops <<- reticulate::import("calflops", delay_load = TRUE)
}


# Message on load of the package
packageStartupMessage(
  "This is version 1.1.0 of aifeducation. Support for 'tensorflow' is removed.
  If you need to use these models please use an older version of this package which
  is available on CRAN or GitHub. Please refer to news for more details.",
  appendLF = TRUE
)

