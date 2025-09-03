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
#'Objects of this class containing fields and methods used in several other classes in 'AI for Education'.
#'
#'This class is **not** designed for a direct application and should only be used by developers.
#'
#' @return A new object of this class.
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
    #-------------------------------------------------------------------------
    #' @description Method for saving a model.
    #' @param dir_path `string` Path of the directory where the model should be saved.
    #' @param folder_name `string` Name of the folder that should be created within the directory.
    #' @return Function does not return a value. It saves the model to disk.
    #' @importFrom utils write.csv
    save = function(dir_path, folder_name) {
      #Save pytorch model
      private$save_pytorch_model(dir_path=dir_path, folder_name=folder_name)

      # Saving Sustainability Data
      private$save_sustainability_data(dir_path=dir_path, folder_name=folder_name)
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
      private$load_pytorch_model(dir_path = dir_path)

      #Load sustainability data
      private$load_sustainability_data(model_dir=dir_path)

      # Set training status
      private$trained <- config_file$private$trained
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting a plot of the training history.
    #' This method requires the *R* package 'ggplot2' to work.
    #' @param final_training `bool` If `FALSE` the values of the performance estimation are used. If `TRUE` only
    #' the epochs of the final training are used.
    #' @param add_min_max `bool` If `TRUE` the minimal and maximal values during performance estimation are port of the plot.
    #' If `FALSE` only the mean values are shown. Parameter is ignored if `final_training=TRUE`.
    #' @param pl_step `int` Number of the step during pseudo labeling to plot. Only relevant if the model was trained
    #' with active pseudo labeling.
    #' @param measure Measure to plot.
    #' @param y_min Minimal value for the y-axis. Set to `NULL` for an automatic adjustment.
    #' @param y_max Maximal value for the y-axis. Set to `NULL` for an automatic adjustment.
    #' @param text_size Size of the text.
    #' @return Returns a plot of class `ggplot` visualizing the training process.
    plot_training_history=function(final_training=FALSE,pl_step=NULL,measure="loss",y_min=NULL,y_max=NULL,add_min_max=TRUE,text_size=10){
        requireNamespace("ggplot2")
        plot_data <- private$prepare_training_history(
          final = final_training,
          pl_step = pl_step
        )

      # Select the performance measure to display
      plot_data <- plot_data[[measure]]

      # Create Plot
      if (measure == "loss") {
        y_label <- "loss"
      } else if (measure == "accuracy") {
        y_label <- "Accuracy"
      } else if (measure == "balanced_accuracy") {
        y_label <- "Balanced Accuracy"
      } else if (measure == "avg_iota") {
        y_label <- "Average Iota"
      }

      plot <- ggplot2::ggplot(data = plot_data) +
        ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train_mean, color = "train")) +
        ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation_mean, color = "validation"))

      if (add_min_max == TRUE) {
        plot <- plot +
          ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train_min, color = "train")) +
          ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$train_max, color = "train")) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = .data$epoch,
              ymin = .data$train_min,
              ymax = .data$train_max
            ),
            alpha = 0.25,
            fill = "red"
          ) +
          ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation_min, color = "validation")) +
          ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$validation_max, color = "validation")) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = .data$epoch,
              ymin = .data$validation_min,
              ymax = .data$validation_max
            ),
            alpha = 0.25,
            fill = "blue"
          )
      }

      if ("test_mean" %in% colnames(plot_data)) {
        plot <- plot +
          ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test_mean, color = "test"))
        if (add_min_max == TRUE) {
          plot <- plot +
            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test_min, color = "test")) +

            ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$test_max, color = "test")) +
            ggplot2::geom_ribbon(
              ggplot2::aes(
                x = .data$epoch,
                ymin = .data$test_min,
                ymax = .data$test_max
              ),
              alpha = 0.25,
              fill = "darkgreen"
            )
        }
      }

      plot <- plot + ggplot2::theme_classic() +
        ggplot2::ylab(y_label) +
        ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
        ggplot2::xlab("epoch") +
        ggplot2::scale_color_manual(values = c(
          "train" = "red",
          "validation" = "blue",
          "test" = "darkgreen"
        )) +
        ggplot2::theme(
          text = ggplot2::element_text(size = text_size),
          legend.position = "bottom"
        )
      return(plot)
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
    set_up_logger = function(log_dir, log_write_interval) {
      private$log_config$log_dir <- log_dir
      private$log_config$log_state_file <- paste0(private$log_config$log_dir, "/aifeducation_state.log")
      private$log_config$log_write_interval <- log_write_interval
    },
    #-------------------------------------------------------------------------
    # This Method updates the model config in the case that new parameters have been
    # introduced
    update_model_config = function() {

      #Check if an update is necessary
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

      #check if an update of values is necessary. This is the case if the model
      #was created with an older version of aifeducation compared to 1.1.0
      #Update values to the new values introduced in version 1.1.0
      if (is.null_or_na(current_pkg_version)) {
        update_values <- TRUE
      } else {
        if (check_versions(
          a = "1.1.0",
          operator = ">",
          b = self$get_package_versions()$r_package_versions$aifeducation
        )) {
          update_values <- TRUE
        } else {
          update_values <- FALSE
        }
      }

      if (update) {
        param_dict <- get_param_dict()
        if (is.function(self$configure)) {
          param_names_new <- rlang::fn_fmls_names(self$configure)
          for (param in param_names_new) {
            if (is_valid_and_exportable_param(arg_name = param, param_dict = param_dict)) {
              if (is.null(private$model_config[[param]])) {
                if (!is.null(param_dict[[param]]$default_historic)) {
                  private$model_config[param] <- list(param_dict[[param]]$default_historic)
                } else {
                  warning(paste("Historic default for", param, "is missing in parameter dictionary."))
                }
              }
            }
            if(update_values){
              private$model_config[param]=list(update_values_to_new_1.1.0(private$model_config[[param]]))
            }
          }

          # Update Package version for the model
          private$r_package_versions$aifeducation <- packageVersion("aifeducation")
        } else {
          warning("Class does not have a method `configure`.")
        }
      }
      #print(private$model_config)


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
    },
    #--------------------------------------------------------------------------
    #'  Prepare history data of objects
    #'  Function for preparing the history data of a model in order to be plotted in AI for Education - Studio.
    #'
    #'  final `bool` If `TRUE` the history data of the final training is used for the data set.
    #'  pl_step `int` If `use_pl=TRUE` select the step within pseudo labeling for which the data should be prepared.
    #'  Returns a named `list` with the training history data of the model. The
    #' reported measures depend on the provided model.
    #'
    #'  Utils Studio Developers
    #'  internal
    prepare_training_history=function(final = FALSE,
                                      pl_step = NULL) {
      plot_data <- self$last_training$history

      if(length(plot_data)<=1){
        plot_data[[1]] <- list(loss = plot_data[[1]])
      }

      #if ("TEFeatureExtractor" %in% class(model)) {
      #  plot_data[[1]] <- list(loss = plot_data[[1]])
      #}

      if (is.null_or_na(final)) final <- FALSE

      if(is.null_or_na(self$last_training$config$use_pl)){
        use_pl=FALSE
      } else {
        use_pl=self$last_training$config$use_pl
      }

      if(use_pl==TRUE & is.null_or_na(pl_step)){
        stop("Model was trained with pseudo labeling. Please provide a pl_step.")
      }

      # Get standard statistics
      n_epochs <- self$last_training$config$epochs
      index_final <- length(self$last_training$history)

      # Get information about the existence of a training, validation, and test data set
      # Get Number of folds for the request
      if (final == FALSE) {
        n_folds <- length(self$last_training$history)
        if (n_folds > 1) {
          n_folds <- n_folds - 1
        }

        if (!use_pl) {
          measures <- names(plot_data[[1]])
          n_sample_type <- nrow(plot_data[[1]][[measures[1]]])
        } else {
          measures <- names(plot_data[[1]][[1]])
          n_sample_type <- nrow(plot_data[[1]][[as.numeric(pl_step)]][[measures[1]]])
        }
      } else {
        n_folds <- 1
        if (use_pl == FALSE) {
          measures <- names(plot_data[[index_final]])
          n_sample_type <- nrow(plot_data[[index_final]][[measures[1]]])
        } else {
          measures <- names(plot_data[[index_final]][[1]])
          n_sample_type <- nrow(plot_data[[index_final]][[as.numeric(pl_step)]][[measures[1]]])
        }
      }

      if (n_sample_type == 3) {
        sample_type_name <- c("train", "validation", "test")
      } else {
        sample_type_name <- c("train", "validation")
      }

      # Create array for saving the data-------------------------------------------
      result_list <- NULL
      for (j in seq_len(length(measures))) {
        measure <- measures[j]
        measure_array <- array(
          dim = c(
            n_folds,
            n_sample_type,
            n_epochs
          ),
          dimnames = list(fold = NULL, sample_type = sample_type_name, epoch = NULL)
        )

        final_data_measure <- matrix(
          data = NA,
          nrow = n_epochs,
          ncol = 3 * n_sample_type + 1
        )
        colnames(final_data_measure) <- c(
          "epoch",
          paste0(
            sample_type_name,
            c(
              rep("_min", times = n_sample_type),
              rep("_mean", times = n_sample_type),
              rep("_max", times = n_sample_type)
            )
          )
        )
        final_data_measure[, "epoch"] <- seq.int(from = 1, to = n_epochs)

        if (final == FALSE) {
          for (i in 1:n_folds) {
            if (use_pl == FALSE) {
              measure_array[i, , ] <- plot_data[[i]][[measure]]
            } else {
              measure_array[i, , ] <- plot_data[[i]][[as.numeric(pl_step)]][[measure]]
            }
          }
        } else {
          if (!use_pl) {
            measure_array[1, , ] <- plot_data[[index_final]][[measure]]
          } else {
            measure_array[1, , ] <- plot_data[[index_final]][[as.numeric(pl_step)]][[measure]]
          }
        }

        for (i in 1:n_epochs) {
          final_data_measure[i, "train_min"] <- min(measure_array[, "train", i])
          final_data_measure[i, "train_mean"] <- mean(measure_array[, "train", i])
          final_data_measure[i, "train_max"] <- max(measure_array[, "train", i])

          final_data_measure[i, "validation_min"] <- min(measure_array[, "validation", i])
          final_data_measure[i, "validation_mean"] <- mean(measure_array[, "validation", i])
          final_data_measure[i, "validation_max"] <- max(measure_array[, "validation", i])

          if (n_sample_type == 3) {
            final_data_measure[i, "test_min"] <- min(measure_array[, "test", i])
            final_data_measure[i, "test_mean"] <- mean(measure_array[, "test", i])
            final_data_measure[i, "test_max"] <- max(measure_array[, "test", i])
          }
        }
        result_list[j] <- list(final_data_measure)
      }

      # Finalize data---------------------------------------------------------------
      names(result_list) <- measures
      return(result_list)
    },
    #---------------------------------------------------------------------------
    save_pytorch_model=function(dir_path,folder_name){
      save_location <- paste0(dir_path, "/", folder_name)

      save_format <- "safetensors"

      if (save_format == "safetensors" & reticulate::py_module_available("safetensors") == FALSE) {
        warning("Python library 'safetensors' is not available. Using
                 standard save format for pytorch.")
        save_format <- "pt"
      }

      create_dir(save_location, FALSE)
      private$model$to("cpu", dtype = torch$float)
      if (save_format == "safetensors") {
        file_path <- paste0(save_location, "/", "model_data", ".safetensors")
        safetensors$torch$save_model(model = private$model, filename = file_path)
      } else if (save_format == "pt") {
        file_path <- paste0(save_location, "/", "model_data", ".pt")
        torch$save(private$model$state_dict(), file_path)
      }
    },
    #---------------------------------------------------------------------------
    load_pytorch_model = function(dir_path) {
      # Load python scripts
      private$load_reload_python_scripts()

      # Load the model---------------------------------------------------------
      path_pt <- paste0(dir_path, "/", "model_data", ".pt")
      path_safe_tensors <- paste0(dir_path, "/", "model_data", ".safetensors")
      private$create_reset_model()
      if (file.exists(path_safe_tensors)) {
        safetensors$torch$load_model(model = private$model, filename = path_safe_tensors)
      } else {
        if (file.exists(paths = path_pt) == TRUE) {
          private$model$load_state_dict(torch$load(path_pt))
        } else {
          stop("There is no compatible model file in the choosen directory.
                     Please check path. Please note that classifiers have to be loaded with
                     the same framework as during creation.")
        }
      }
    }
  )
)
