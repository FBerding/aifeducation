## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = TRUE, eval=FALSE-----------------------------------------------
# install.packages("aifeducation")

## ----include = TRUE, eval=FALSE-----------------------------------------------
# aifeducation::install_aifeducation(
#   install_aifeducation_studio = TRUE,
#   python_version = "3.12",
#   cuda_version = "12.4",
#   use_conda = FALSE
# )

## ----include = TRUE, eval=FALSE-----------------------------------------------
# reticulate::py_available(initialize = TRUE)

## ----include = TRUE, eval=FALSE-----------------------------------------------
# aifeducation::check_aif_py_modules()

## ----include = TRUE, eval=FALSE-----------------------------------------------
# aifeducation::start_aifeducation_studio()

## ----include = TRUE, eval=FALSE-----------------------------------------------
# library(aifeducation)
# aifeducation::prepare_session()

## ----include = TRUE, eval=FALSE-----------------------------------------------
# aifeducation::update_aifeducation(
#   update_aifeducation_studio = TRUE,
#   env_type = "auto",
#   cuda_version = "12.4",
#   envname = "aifeducation"
# )

## ----include = TRUE, eval=TRUE------------------------------------------------
t(aifeducation::get_recommended_py_versions())

