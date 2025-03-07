# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Install aifeducation on a machine
#' @description Function for installing 'aifeducation' on a machine. This functions assumes that not 'python' and no
#'   'miniconda' is installed. Only'pytorch' is installed.
#'
#' @param install_aifeducation_studio `bool` If `TRUE` all necessary R packages are installed for using AI for Education
#'   Studio.
#'
#' @return Function does nothing return. It installs python, optional R packages, and necessary 'python' packages on a
#'   machine.
#'
#' @importFrom reticulate install_python
#' @importFrom reticulate install_miniconda
#' @importFrom utils install.packages
#'
#' @family Installation and Configuration
#'
#' @export
install_aifeducation <- function(install_aifeducation_studio = TRUE) {
  reticulate::install_python()
  reticulate::install_miniconda()
  install_py_modules(
    envname = "aifeducation",
    install = "pytorch",
    pytorch_cuda_version = "12.1",
    python_version = "3.11",
    remove_first = FALSE
  )

  if (install_aifeducation_studio == TRUE) {
    utils::install.packages(
      "ggplot2",
      "rlang",
      "shiny",
      "shinyFiles",
      "shinyWidgets",
      "sortable",
      "bslib",
      "future",
      "promises",
      "DT",
      "readtext",
      "readxl"
    )
  }
}

#' @title Installing necessary python modules to an environment
#' @description Function for installing the necessary python modules.
#'
#' @param envname `string` Name of the environment where the packages should be installed.
#' @param transformer_version `string` determining the desired version of the python library 'transformers'.
#' @param tokenizers_version `string` determining the desired version of the python library 'tokenizers'.
#' @param pandas_version `string` determining the desired version of the python library 'pandas'.
#' @param datasets_version `string` determining the desired version of the python library 'datasets'.
#' @param codecarbon_version `string` determining the desired version of the python library 'codecarbon'.
#' @param safetensors_version `string` determining the desired version of the python library 'safetensors'.
#' @param torcheval_version `string` determining the desired version of the python library 'torcheval'.
#' @param accelerate_version `string` determining the desired version of the python library 'accelerate'.
#' @param pytorch_cuda_version `string` determining the desired version of 'cuda' for ' PyTorch'.
#' @param python_version `string` Python version to use.
#' @param remove_first `bool` If `TRUE` removes the environment completely before recreating the environment and
#'   installing the packages. If `FALSE` the packages are installed in the existing environment without any prior
#'   changes.
#' @return Returns no values or objects. Function is used for installing the necessary python libraries in a conda
#'   environment.
#' @importFrom reticulate conda_create
#' @importFrom reticulate conda_remove
#' @importFrom reticulate condaenv_exists
#' @importFrom reticulate py_install
#' @importFrom utils compareVersion
#' @family Installation and Configuration
#' @export
install_py_modules <- function(envname = "aifeducation",
                               transformer_version = "<=4.46",
                               tokenizers_version = "<=0.20.4",
                               pandas_version = "<=2.2.3",
                               datasets_version = "<=3.1.0",
                               codecarbon_version = "<=2.8.2",
                               safetensors_version = "<=0.4.5",
                               torcheval_version = "<=0.0.7",
                               accelerate_version = "<=1.1.1",
                               pytorch_cuda_version = "12.1",
                               python_version = "3.9",
                               remove_first = FALSE) {
  relevant_modules <- c(
    paste0("transformers", transformer_version),
    paste0("tokenizers", tokenizers_version),
    paste0("pandas", pandas_version),
    paste0("datasets", datasets_version),
    paste0("codecarbon", codecarbon_version)
  )
  relevant_modules_pt <- c(
    paste0("safetensors", safetensors_version),
    paste0("torcheval", torcheval_version),
    paste0("accelerate", accelerate_version)
  )

  if (reticulate::condaenv_exists(envname = envname) == TRUE) {
    if (remove_first == TRUE) {
      reticulate::conda_remove(envname = envname)
      reticulate::conda_create(
        envname = envname,
        channel = c("conda-forge"),
        python_version = python_version
      )
    }
  } else {
    reticulate::conda_create(
      envname = envname,
      channel = c("conda-forge"),
      python_version = python_version
    )
  }

  # PyTorch Installation---------------------------------------------------
    reticulate::conda_install(
      packages = c(
        "pytorch",
        paste0("pytorch-cuda", "=", pytorch_cuda_version)
      ),
      envname = envname,
      channel = c("pytorch", "nvidia"),
      conda = "auto",
      pip = FALSE
    )

  # Necessary Packages----------------------------------------------------
    reticulate::conda_install(
      packages = c(relevant_modules, relevant_modules_pt),
      envname = envname,
      conda = "auto",
      pip = TRUE
    )
}

#' @title Check if all necessary python modules are available
#' @description This function checks if all  python modules necessary for the package aifeducation to work are
#'   available.
#'
#' @param trace `bool` `TRUE` if a list with all modules and their availability should be printed to the console.
#' @return The function prints a table with all relevant packages and shows which modules are available or unavailable.
#' @return If all relevant modules are available, the functions returns `TRUE`. In all other cases it returns `FALSE`
#' @family Installation and Configuration
#' @export
check_aif_py_modules <- function(trace = TRUE) {

  general_modules <- c(
    "os",
    "transformers",
    "tokenizers",
    "datasets",
    "codecarbon"
  )
  pytorch_modules <- c(
    "torch",
    "torcheval",
    "safetensors",
    "accelerate",
    "pandas"
  )
    relevant_modules <- c(
      general_modules,
      pytorch_modules
    )

  matrix_overview <- matrix(
    data = NA,
    nrow = length(relevant_modules),
    ncol = 2
  )
  colnames(matrix_overview) <- c("module", "available")
  matrix_overview <- as.data.frame(matrix_overview)
  for (i in 1:length(relevant_modules)) {
    matrix_overview[i, 1] <- relevant_modules[i]
    matrix_overview[i, 2] <- reticulate::py_module_available(relevant_modules[i])
  }

  if (trace == TRUE) {
    print(matrix_overview)
  }

  if (sum(matrix_overview[, 2]) == length(relevant_modules)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Sets the level for logging information of the 'transformers' library
#' @description This function changes the level for logging information of the 'transformers' library. It influences the
#'   output printed to console for creating and training transformer models as well as [TextEmbeddingModel]s.
#'
#' @param level `string` Minimal level that should be printed to console. Four levels are available: INFO, WARNING,
#'   ERROR and DEBUG
#' @return This function does not return anything. It is used for its side effects.
#' @family Installation and Configuration
#' @export
set_transformers_logger <- function(level = "ERROR") {
  if (level == "ERROR") {
    transformers$utils$logging$set_verbosity_error()
  } else if (level == "WARNING") {
    transformers$utils$logging$set_verbosity_warning()
  } else if (level == "INFO") {
    transformers$utils$logging$set_verbosity_info()
  } else if (level == "DEBUG") {
    transformers$utils$logging$set_verbosity_debug()
  }
}
