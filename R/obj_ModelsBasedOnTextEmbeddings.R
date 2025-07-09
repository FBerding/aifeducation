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



#' @title Base class for models using neural nets
#' @description Abstract class for all models that do not rely on the python library 'transformers'.
#' All models of this class require text embeddings as input. These are provided as
#' objects of class [EmbeddedText] or [LargeDataSetForTextEmbeddings].
#'
#' @return Objects of this class containing fields and methods used in several other classes in 'AI for education'. This class
#'   is **not** designed for a direct application and should only be used by developers.
#' @family R6 Classes for Developers
#' @export
ModelsBasedOnTextEmbeddings <- R6::R6Class(
  classname = "ModelsBasedOnTextEmbeddings",
  inherit = AIFEBaseModel,
  public = list(
    #--------------------------------------------------------------------------
    #' @description Method for requesting the text embedding model information.
    #' @return `list` of all relevant model information on the text embedding model underlying the model.
    get_text_embedding_model = function() {
      return(private$text_embedding_model)
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting the name (unique id) of the underlying text embedding model.
    #' @return Returns a `string` describing name of the text embedding model.
    get_text_embedding_model_name = function() {
      return(private$text_embedding_model$model$model_name)
    },
    #--------------------------------------------------------------------------
    # Check Embedding Model compatibility of the text embedding
    #' @description Method for checking if the provided text embeddings are created with the same [TextEmbeddingModel]
    #'   as the model.
    #' @param text_embeddings Object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings].
    #' @return `TRUE` if the underlying [TextEmbeddingModel] are the same. `FALSE` if the models differ.
    check_embedding_model = function(text_embeddings) {
      # Check object type
      private$check_embeddings_object_type(text_embeddings, strict = TRUE)

      # Check original text embedding model
      embedding_model_config <- text_embeddings$get_model_info()
      check <- c("model_name")

      if (
        !is.null_or_na(embedding_model_config[[check]]) &
          !is.null_or_na(private$text_embedding_model$model[[check]])
      ) {
        if (embedding_model_config[[check]] != private$text_embedding_model$model[[check]]) {
          stop("The TextEmbeddingModel that generated the data_embeddings is not
               the same as the TextEmbeddingModel when generating the classifier.")
        }
      }
    },
    #--------------------------------------------------------------------------
    #' @description loads an object from disk and updates the object to the current version of the package.
    #' @param dir_path Path where the object set is stored.
    #' @return Method does not return anything. It loads an object from disk.
    load_from_disk = function(dir_path) {
      # Set configuration state
      private$set_configuration_to_TRUE()

      # Load R file with configuration and other data
      config_file <- load_R_config_state(dir_path)

      # load information of the text embedding model
      private$load_config_and_docs_textembeddingmodel(
        config_public = config_file$public,
        config_private = config_file$private
      )

      # Call the core method which loads data common for all models.
      # These are documentations, licenses, model's name and label etc.
      private$load_base_config_and_docs_general(
        config_public = config_file$public,
        config_private = config_file$private
      )

      # Check and update model_config
      # Call this method to add parameters that where added in later version
      # which are missing in the old model
      private$update_model_config()

      #Check and update pad value if necessary
      private$update_pad_value()

      # Create and load AI model
      private$create_reset_model()
      self$load(dir_path = dir_path)

      # Set training status
      private$trained <- config_file$private$trained
    }
  ),
  private = list(
    text_embedding_model = list(
      model = list(),
      times = NA,
      features = NA
    ),
    #------------------------------------------------------------------------------
    load_config_and_docs_textembeddingmodel = function(config_public, config_private) {
      if(is.null(config_private$text_embedding_model$pad_value)){
        pad_value=0
      } else {
        pad_value=config_private$text_embedding_model$pad_value
      }

      private$set_text_embedding_model(
        model_info = config_private$text_embedding_model$model,
        feature_extractor_info = config_private$text_embedding_model$feature_extractor,
        times = config_private$text_embedding_model$times,
        features = config_private$text_embedding_model$features,
        pad_value=pad_value
      )
    },
    #---------------------------------------------------------------------------
    check_embeddings_object_type = function(embeddings, strict = TRUE) {
      if (strict == TRUE) {
        if (
          !("EmbeddedText" %in% class(embeddings)) &
            !("LargeDataSetForTextEmbeddings" %in% class(embeddings))
        ) {
          stop("text_embeddings must be of class EmbeddedText or LargeDataSetForTextEmbeddings.")
        }
      } else {
        if (
          !("EmbeddedText" %in% class(embeddings)) &
            !("LargeDataSetForTextEmbeddings" %in% class(embeddings)) &
            !("array" %in% class(embeddings)) &
            !("datasets.arrow_dataset.Dataset" %in% class(embeddings))
        ) {
          stop("text_embeddings must be of class EmbeddedText, LargeDataSetForTextEmbeddings,
               datasets.arrow_dataset.Dataset or array.")
        }
      }
    },
    #-------------------------------------------------------------------------
    check_single_prediction = function(embeddings) {
      if (
        "EmbeddedText" %in% class(embeddings) |
          "LargeDataSetForTextEmbeddings" %in% class(embeddings)
      ) {
        if (embeddings$n_rows() > 1) {
          single_prediction <- FALSE
        } else {
          single_prediction <- TRUE
        }
      } else if ("array" %in% class(embeddings)) {
        if (nrow(embeddings) > 1) {
          single_prediction <- FALSE
        } else {
          single_prediction <- TRUE
        }
      } else if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        single_prediction <- FALSE
      }
      return(single_prediction)
    },
    #--------------------------------------------------------------------------
    prepare_embeddings_as_dataset = function(embeddings) {
      if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        prepared_dataset <- embeddings
      } else if ("EmbeddedText" %in% class(embeddings)) {
        prepared_dataset <- datasets$Dataset$from_dict(
          reticulate::dict(
            list(
              id = rownames(embeddings$embeddings),
              input = np$squeeze(
                np$split(
                  reticulate::np_array(embeddings$embeddings),
                  as.integer(nrow(embeddings$embeddings)),
                  axis = 0L
                )
              )
            ),
            convert = FALSE
          )
        )
      } else if ("array" %in% class(embeddings)) {
        prepared_dataset <- datasets$Dataset$from_dict(
          reticulate::dict(
            list(
              id = rownames(embeddings),
              input = np$squeeze(np$split(reticulate::np_array(embeddings), as.integer(nrow(embeddings)), axis = 0L))
            ),
            convert = FALSE
          )
        )
      } else if ("LargeDataSetForTextEmbeddings" %in% class(embeddings)) {
        prepared_dataset <- embeddings$get_dataset()
      }
      return(prepared_dataset)
    },
    #-------------------------------------------------------------------------
    prepare_embeddings_as_np_array = function(embeddings) {
      if ("EmbeddedText" %in% class(embeddings)) {
        prepared_dataset <- embeddings$embeddings
        tmp_np_array <- np$array(prepared_dataset)
      } else if ("array" %in% class(embeddings)) {
        prepared_dataset <- embeddings
        tmp_np_array <- np$array(prepared_dataset)
      } else if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        prepared_dataset <- embeddings$set_format("np")
        tmp_np_array <- prepared_dataset["input"]
      } else if ("LargeDataSetForTextEmbeddings" %in% class(embeddings)) {
        prepared_dataset <- embeddings$get_dataset()
        prepared_dataset$set_format("np")
        tmp_np_array <- prepared_dataset["input"]
      }
      tmp_np_array <- reticulate::np_array(tmp_np_array)
      if (numpy_writeable(tmp_np_array) == FALSE) {
        warning("Numpy array is not writable")
      }
      return(tmp_np_array)
    },
    #--------------------------------------------------------------------------
    get_rownames_from_embeddings = function(embeddings) {
      if ("EmbeddedText" %in% class(embeddings)) {
        return(rownames(embeddings$embeddings))
      } else if ("array" %in% class(embeddings)) {
        return(rownames(embeddings))
      } else if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        return(embeddings["id"])
      } else if ("LargeDataSetForTextEmbeddings" %in% class(embeddings)) {
        embeddings$get_ids()
      }
    },
    #--------------------------------------------------------------------------
    set_text_embedding_model = function(model_info,
                                        feature_extractor_info,
                                        times,
                                        features,
                                        pad_value) {
      private$text_embedding_model["model"] <- list(model_info)
      private$text_embedding_model["feature_extractor"] <- feature_extractor_info
      private$text_embedding_model["times"] <- times
      private$text_embedding_model["features"] <- features
      private$text_embedding_model["pad_value"] <- pad_value
    },
    save_all_args = function(args, group = "training") {
      if (group %in% c("configure", "training")) {
        if (group == "training") {
          for (arg in names(args)) {
            if (!R6::is.R6(args[[arg]]) &
              !is.factor(args[[arg]]) &
              !arg %in% c("log_dir", "log_write_interval")) {
              self$last_training$config[arg] <- list(args[[arg]])
            }
          }
        } else if (group == "configure") {
          for (arg in names(args)) {
            if (!R6::is.R6(args[[arg]]) &
              !is.factor(args[[arg]]) &
              !arg %in% c("log_dir", "log_write_interval")) {
              self$model_config[arg] <- list(args[[arg]])
            }
          }
        }
      } else {
        stop("Argument 'group' must be 'configure' or 'training'.")
      }
    },
    set_up_logger = function(log_dir, log_write_interval) {
      private$log_config$log_dir <- log_dir
      private$log_config$log_state_file <- paste0(private$log_config$log_dir, "/aifeducation_state.log")
      private$log_config$log_write_interval <- log_write_interval
    },
    #-------------------------------------------------------------------------
    # This Method updates the model config in the case that new parameters have been
    # introduced
    update_model_config = function() {
      current_pkg_version <- self$get_package_versions()$r_package_versions$aifeducation
      if (is.null_or_na(current_pkg_version)) {
        update <- TRUE
      } else {
        if (check_versions(
          a = packageVersion("aifeducation"),
          operator = ">",
          b = self$get_package_versions()$r_package_versions$aifeducation
        )) {
          update <- TRUE
        } else {
          update <- FALSE
        }
      }

      if (update) {
        param_dict <- get_param_dict()
        if (is.function(self$configure)) {
          param_names_new <- rlang::fn_fmls_names(self$configure)
          for (param in param_names_new) {
            if (is_valid_and_exportable_param(arg_name = param, param_dict = param_dict)) {
              if (is.null(self$model_config[[param]])) {
                if (!is.null(param_dict[[param]]$default_historic)) {
                  self$model_config[param] <- list(param_dict[[param]]$default_historic)
                } else {
                  stop(paste("Historic default for", param, "is missing in parameter dictionary."))
                }
              }
            }
          }
          # Update Package version for the model
          private$r_package_versions$aifeducation <- packageVersion("aifeducation")
        } else {
          warning("Class does not have a method `configure`.")
        }
      }
    },
    #-------------------------------------------------------------------------
    update_pad_value=function(){
      current_pkg_version <- self$get_package_versions()$r_package_versions$aifeducation
      if (is.na(current_pkg_version)) {
        update <- TRUE
      } else {
        if (check_versions(
          a = packageVersion("aifeducation"),
          operator = ">",
          b = self$get_package_versions()$r_package_versions$aifeducation
        )) {
          update <- TRUE
        } else {
          update <- FALSE
        }
      }

      if (update) {
        if(is.null_or_na(private$text_embedding_model["pad_value"])){
          private$text_embedding_model["pad_value"] <- 0
        }
      }
    },
    #--------------------------------------------------------------------------
    generate_model_id = function(name) {
      if (is.null(name)) {
        return(paste0("mbote_",generate_id(16)))
      } else {
        return(name)
      }
    }
  )
)
