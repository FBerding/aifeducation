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

#' @title Base class for classifiers relying on numerical representations of texts instead of words that use
#' the architecture of Protonet and its corresponding training techniques.
#' @description Base class for classifiers relying on [EmbeddedText] or [LargeDataSetForTextEmbeddings] as input
#' which use the architecture of Protonet and its corresponding training techniques.
#'
#' @return Objects of this containing fields and methods used in several other classes in 'ai for education'. This class
#'   is **not** designed for a direct application and should only be used by developers.
#' @family Classifiers for developers
#' @export
TEClassifiersBasedOnProtoNet <- R6::R6Class(
  classname = "TEClassifiersBasedOnProtoNet",
  inherit = ClassifiersBasedOnTextEmbeddings,
  public = list(
    #-------------------------------------------------------------------------
    #' @description Method for training a neural net.
    #'
    #'   Training includes a routine for early stopping. In the case that loss<0.0001 and Accuracy=1.00 and Average
    #'   Iota=1.00 training stops. The history uses the values of the last trained epoch for the remaining epochs.
    #'
    #'   After training the model with the best values for Average Iota, Accuracy, and Loss on the validation data set
    #'   is used as the final model.
    #'
    #' @param data_embeddings `r get_param_doc_desc("data_embeddings")`
    #' @param data_targets `r get_param_doc_desc("data_targets")`.
    #' @param data_folds `r get_param_doc_desc("data_folds")`
    #' @param data_val_size `r get_param_doc_desc("data_val_size")`
    #' @param balance_class_weights `r get_param_doc_desc("balance_class_weights")`
    #' @param balance_sequence_length `r get_param_doc_desc("balance_sequence_length")`
    #' @param use_sc `r get_param_doc_desc("use_sc")`
    #' @param sc_method `r get_param_doc_desc("sc_method")`
    #' @param sc_min_k `r get_param_doc_desc("sc_min_k")`
    #' @param sc_max_k `r get_param_doc_desc("sc_max_k")`
    #' @param use_pl `r get_param_doc_desc("use_pl")`
    #' @param pl_max_steps `r get_param_doc_desc("pl_max_steps")`
    #' @param pl_anchor `r get_param_doc_desc("pl_anchor")`
    #' @param pl_max `r get_param_doc_desc("pl_max")`
    #' @param pl_min `r get_param_doc_desc("pl_min")`
    #' @param sustain_track `r get_param_doc_desc("sustain_track")`
    #' @param sustain_iso_code `r get_param_doc_desc("sustain_iso_code")`
    #' @param sustain_region `r get_param_doc_desc("sustain_region")`
    #' @param sustain_interval `r get_param_doc_desc("sustain_interval")`
    #' @param epochs `r get_param_doc_desc("epochs")`
    #' @param batch_size `r get_param_doc_desc("batch_size")`
    #' @param log_dir `r get_param_doc_desc("log_dir")`
    #' @param log_write_interval `r get_param_doc_desc("log_write_interval")`
    #' @param trace `r get_param_doc_desc("trace")`
    #' @param ml_trace `r get_param_doc_desc("ml_trace")`
    #' @param n_cores `r get_param_doc_desc("n_cores")`
    #' @param lr_rate `r get_param_doc_desc("lr_rate")`
    #' @param lr_warm_up_ratio `r get_param_doc_desc("lr_warm_up_ratio")`
    #' @param Ns `r get_param_doc_desc("Ns")`
    #' @param Nq `r get_param_doc_desc("Nq")`
    #' @param loss_alpha `r get_param_doc_desc("loss_alpha")`
    #' @param loss_margin `r get_param_doc_desc("loss_margin")`
    #' @param sampling_separate `r get_param_doc_desc("sampling_separate")`
    #' @param sampling_shuffle `r get_param_doc_desc("sampling_shuffle")`
    #' @return Function does not return a value. It changes the object into a trained classifier.
    #' @details
    #'
    #' * `sc_max_k`: All values from sc_min_k up to sc_max_k are successively used. If
    #' the number of `sc_max_k` is too high, the value is reduced to a number that allows the calculating of synthetic
    #' units.
    #' * `pl_anchor:` With the help of this value, the new cases are sorted. For
    #' this aim, the distance from the anchor is calculated and all cases are arranged into an ascending order.
    #'
    train = function(data_embeddings=NULL,
                     data_targets=NULL,
                     data_folds = 5,
                     data_val_size = 0.25,
                     use_sc = TRUE,
                     sc_method = "knnor",
                     sc_min_k = 1,
                     sc_max_k = 10,
                     use_pl = TRUE,
                     pl_max_steps = 3,
                     pl_max = 1.00,
                     pl_anchor = 1.00,
                     pl_min = 0.00,
                     sustain_track = TRUE,
                     sustain_iso_code = NULL,
                     sustain_region = NULL,
                     sustain_interval = 15,
                     epochs = 40,
                     batch_size = 35,
                     Ns = 5,
                     Nq = 3,
                     loss_alpha = 0.5,
                     loss_margin = 0.5,
                     sampling_separate = FALSE,
                     sampling_shuffle = TRUE,
                     trace = TRUE,
                     ml_trace = 1,
                     log_dir = NULL,
                     log_write_interval = 10,
                     n_cores = auto_n_cores(),
                     lr_rate=1e-3,
                     lr_warm_up_ratio=0.02) {
      private$do_training(args=get_called_args(n=1))

    },
    #---------------------------------------------------------------------------
    #' @description Method for embedding documents. Please do not confuse this type of embeddings with the embeddings of
    #'   texts created by an object of class [TextEmbeddingModel]. These embeddings embed documents according to their
    #'   similarity to specific classes.
    #' @param embeddings_q Object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings] containing the text
    #'   embeddings for all cases which should be embedded into the classification space.
    #' @param batch_size `int` batch size.
    #' @return Returns a `list` containing the following elements
    #'
    #' * `embeddings_q`: embeddings for the cases (query sample).
    #' * `embeddings_prototypes`: embeddings of the prototypes which were learned during training. They represents the
    #' center for the different classes.
    #'
    embed = function(embeddings_q = NULL, batch_size = 32) {
      check_class(object=embeddings_q, classes=c("EmbeddedText", "LargeDataSetForTextEmbeddings"), allow_NULL=FALSE)
      check_type(object=batch_size, type="int", FALSE)

      # Check input for compatible text embedding models and feature extractors
      if ("EmbeddedText" %in% class(embeddings_q)) {
        self$check_embedding_model(text_embeddings = embeddings_q)
        requires_compression <- self$requires_compression(embeddings_q)
      } else if ("array" %in% class(embeddings_q)) {
        requires_compression <- self$requires_compression(embeddings_q)
      } else {
        requires_compression <- FALSE
      }

      # Load Custom Model Scripts
      private$load_reload_python_scripts()

      # Check number of cases in the data
      single_prediction <- private$check_single_prediction(embeddings_q)

      # Get current row names/name of the cases
      current_row_names <- private$get_rownames_from_embeddings(embeddings_q)

      # Apply feature extractor if it is part of the model
      if (requires_compression == TRUE) {
        # Returns a data set
        embeddings_q <- self$feature_extractor$extract_features(
          data_embeddings = embeddings_q,
          batch_size = as.integer(batch_size)
        )
      }


      # If at least two cases are part of the data set---------------------------
      if (single_prediction == FALSE) {
        # Returns a data set object
        prediction_data_q_embeddings <- private$prepare_embeddings_as_dataset(embeddings_q)

        prediction_data_q_embeddings$set_format("torch")
        embeddings_and_distances <- py$TeProtoNetBatchEmbedDistance(
          model = self$model,
          dataset_q = prediction_data_q_embeddings,
          batch_size = as.integer(batch_size)
        )
        embeddings_tensors_q <- private$detach_tensors(embeddings_and_distances[[1]])
        distances_tensors_q <- private$detach_tensors(embeddings_and_distances[[2]])
      } else {
        prediction_data_q_embeddings <- private$prepare_embeddings_as_np_array(embeddings_q)

        # Apply feature extractor if it is part of the model
        if (requires_compression == TRUE) {
          # Returns a data set
          prediction_data_q <- np$array(self$feature_extractor$extract_features(
            data_embeddings = prediction_data_q_embeddings,
            batch_size = as.integer(batch_size)
          )$embeddings)
        }


        if (torch$cuda$is_available()) {
          device <- "cuda"
          dtype <- torch$double
          self$model$to(device, dtype = dtype)
          self$model$eval()
          input <- torch$from_numpy(prediction_data_q_embeddings)
          embeddings_tensors_q <- self$model$embed(input$to(device, dtype = dtype))
          embeddings_tensors_q <- private$detach_tensors(embeddings_tensors_q)
          distances_tensors_q <- self$model$get_distances(input$to(device, dtype = dtype))
          distances_tensors_q <- private$detach_tensors(distances_tensors_q)
        } else {
          device <- "cpu"
          dtype <- torch$float
          self$model$to(device, dtype = dtype)
          self$model$eval()
          input <- torch$from_numpy(prediction_data_q_embeddings)
          embeddings_tensors_q <- self$model$embed(input$to(device, dtype = dtype))
          embeddings_tensors_q <- private$detach_tensors(embeddings_tensors_q)
          distances_tensors_q <- self$model$get_distances(input$to(device, dtype = dtype))
          distances_tensors_q <- private$detach_tensors(distances_tensors_q)
        }
      }

      embeddings_prototypes <- private$detach_tensors(
        self$model$get_trained_prototypes()
      )

      # Post processing
      rownames(embeddings_tensors_q) <- current_row_names
      rownames(distances_tensors_q) <- current_row_names
      rownames(embeddings_prototypes) <- self$model_config$target_levels

      return(list(
        embeddings_q = embeddings_tensors_q,
        distances_q = distances_tensors_q,
        embeddings_prototypes = embeddings_prototypes
      ))
    },
    #---------------------------------------------------------------------------
    #' @description Method for creating a plot to visualize embeddings and their corresponding centers (prototypes).
    #' @param embeddings_q Object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings] containing the text
    #'   embeddings for all cases which should be embedded into the classification space.
    #' @param classes_q Named `factor` containg the true classes for every case. Please note that the names must match
    #'   the names/ids in `embeddings_q`.
    #' @param inc_unlabeled `bool` If `TRUE` plot includes unlabeled cases as data points.
    #' @param size_points `int` Size of the points excluding the points for prototypes.
    #' @param size_points_prototypes `int` Size of points representing prototypes.
    #' @param alpha `float` Value indicating how transparent the points should be (important
    #'   if many points overlap). Does not apply to points representing prototypes.
    #' @param batch_size `int` batch size.
    #' @return Returns a plot of class `ggplot`visualizing embeddings.
    plot_embeddings = function(embeddings_q,
                               classes_q = NULL,
                               batch_size = 12,
                               alpha = 0.5,
                               size_points = 3,
                               size_points_prototypes = 8,
                               inc_unlabeled = TRUE) {
      # Argument checking-------------------------------------------------------


      embeddings <- self$embed(
        embeddings_q = embeddings_q,
        batch_size = batch_size
      )

      prototypes <- as.data.frame(embeddings$embeddings_prototypes)
      prototypes$class <- rownames(embeddings$embeddings_prototypes)
      prototypes$type <- rep("prototype", nrow(embeddings$embeddings_prototypes))
      colnames(prototypes) <- c("x", "y", "class", "type")


      if (!is.null(classes_q)) {
        true_values_names <- intersect(
          x = names(na.omit(classes_q)),
          y = private$get_rownames_from_embeddings(embeddings_q)
        )
        true_values <- as.data.frame(embeddings$embeddings_q[true_values_names, , drop = FALSE])
        true_values$class <- classes_q[true_values_names]
        true_values$type <- rep("labeled", length(true_values_names))
        colnames(true_values) <- c("x", "y", "class", "type")
      } else {
        true_values_names <- NULL
        true_values <- NULL
      }


      if (inc_unlabeled == TRUE) {
        estimated_values_names <- setdiff(
          x = private$get_rownames_from_embeddings(embeddings_q),
          y = true_values_names
        )

        if (length(estimated_values_names) > 0) {
          estimated_values <- as.data.frame(embeddings$embeddings_q[estimated_values_names, , drop = FALSE])
          estimated_values$class <- private$calc_classes_on_distance(
            distance_matrix = embeddings$distances_q[estimated_values_names, , drop = FALSE],
            prototypes = embeddings$embeddings_prototypes
          )
          estimated_values$type <- rep("unlabeled", length(estimated_values_names))
          colnames(estimated_values) <- c("x", "y", "class", "type")
        }
      } else {
        estimated_values_names <- NULL
      }


      plot_data <- prototypes
      if (length(true_values) > 0) {
        plot_data <- rbind(plot_data, true_values)
      }
      if (length(estimated_values_names) > 0) {
        plot_data <- rbind(plot_data, estimated_values)
      }

      plot <- ggplot2::ggplot(data = plot_data) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(
            x = x,
            y = y,
            color = class,
            shape = type,
            size = type,
            alpha = type
          ) # ,
          # position = ggplot2::position_jitter(h = 0.1, w = 0.1)
        ) +
        ggplot2::scale_size_manual(values = c(
          "prototype" = size_points_prototypes,
          "labeled" = size_points,
          "unlabeled" = size_points
        )) +
        ggplot2::scale_alpha_manual(
          values = c(
            "prototype" = 1,
            "labeled" = alpha,
            "unlabeled" = alpha
          )
        ) +
        ggplot2::theme_classic()
      return(plot)
    }
  ),
  private = list(
    #-------------------------------------------------------------------------
    calc_classes_on_distance = function(distance_matrix, prototypes) {
      index_vector <- vector(length = nrow(distance_matrix))

      for (i in seq_len(length(index_vector))) {
        index_vector[i] <- which.min(distance_matrix[i, ])
      }

      classes <- factor(index_vector,
        levels = seq_len(nrow(prototypes)),
        labels = rownames(prototypes)
      )
      return(classes)
    },
    #--------------------------------------------------------------------------
    basic_train = function(train_data = NULL,
                           val_data = NULL,
                           test_data = NULL,
                           reset_model = FALSE,
                           use_callback = TRUE,
                           log_dir = NULL,
                           log_write_interval = 10,
                           log_top_value = NULL,
                           log_top_total = NULL,
                           log_top_message = NULL) {
      # Clear session to provide enough resources for computations
      if (torch$cuda$is_available()) {
        torch$cuda$empty_cache()
      }

      # Reset model if requested
      if (reset_model == TRUE) {
        private$create_reset_model()
      }

      # Set loss function
      loss_fct_name <- "ProtoNetworkMargin"

      # Check directory for checkpoints
      create_dir(
        dir_path = paste0(self$last_training$config$dir_checkpoint, "/checkpoints"),
        trace = self$last_training$config$trace,
        msg = "Creating Checkpoint Directory"
      )

      # Set target column
      if (self$model_config$require_one_hot == FALSE) {
        target_column <- "labels"
      } else {
        target_column <- "one_hot_encoding"
      }

      dataset_train <- train_data$select_columns(c("input", target_column))
      if (self$model_config$require_one_hot == TRUE) {
        dataset_train <- dataset_train$rename_column(target_column, "labels")
      }

      pytorch_train_data <- dataset_train$with_format("torch")

      pytorch_val_data <- val_data$select_columns(c("input", target_column))
      if (self$model_config$require_one_hot == TRUE) {
        pytorch_val_data <- pytorch_val_data$rename_column(target_column, "labels")
      }
      pytorch_val_data <- pytorch_val_data$with_format("torch")

      if (!is.null(test_data)) {
        pytorch_test_data <- test_data$select_columns(c("input", target_column))
        if (self$model_config$require_one_hot == TRUE) {
          pytorch_test_data <- pytorch_test_data$rename_column(target_column, "labels")
        }
        pytorch_test_data <- pytorch_test_data$with_format("torch")
      } else {
        pytorch_test_data <- NULL
      }

      history <- py$TeClassifierProtoNetTrain_PT_with_Datasets(
        model = self$model,
        loss_fct_name = loss_fct_name,
        optimizer_method = self$model_config$optimizer,
        lr_rate=self$last_training$config$lr_rate,
        lr_warm_up_ratio=self$last_training$config$lr_warm_up_ratio,
        Ns = as.integer(self$last_training$config$Ns),
        Nq = as.integer(self$last_training$config$Nq),
        loss_alpha = self$last_training$config$loss_alpha,
        loss_margin = self$last_training$config$loss_margin,
        trace = as.integer(self$last_training$config$ml_trace),
        use_callback = use_callback,
        train_data = pytorch_train_data,
        val_data = pytorch_val_data,
        test_data = pytorch_test_data,
        epochs = as.integer(self$last_training$config$epochs),
        sampling_separate = self$last_training$config$sampling_separate,
        sampling_shuffle = self$last_training$config$sampling_shuffle,
        filepath = paste0(self$last_training$config$dir_checkpoint, "/checkpoints/best_weights.pt"),
        n_classes = as.integer(length(self$model_config$target_levels)),
        log_dir = log_dir,
        log_write_interval = log_write_interval,
        log_top_value = log_top_value,
        log_top_total = log_top_total,
        log_top_message = log_top_message
      )

      # provide rownames and replace -100
      history <- private$prepare_history_data(history)

      return(history)
    }
  )
)
