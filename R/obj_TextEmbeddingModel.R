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

#' @title Text embedding model
#' @description This `R6` class stores a text embedding model which can be used to tokenize, encode, decode, and embed
#'   raw texts. The object provides a unique interface for different text processing methods.
#' @return Objects of class [TextEmbeddingModel] transform raw texts into numerical representations which can be used
#'   for downstream tasks. For this aim objects of this class allow to tokenize raw texts, to encode tokens to sequences
#'   of integers, and to decode sequences of integers back to tokens.
#' @family Text Embedding
#' @export
TextEmbeddingModel <- R6::R6Class(
  classname = "TextEmbeddingModel",
  inherit = AIFEBaseModel,
  private = list(

    publication_info = list(
      developed_by = list(
        authors = NULL,
        citation = NULL,
        url = NULL
      ),
      modified_by = list(
        authors = NULL,
        citation = NULL,
        url = NULL
      )
    ),

    #--------------------------------------------------------------------------
    # Method for generating a model id
    generate_model_id = function(name) {
      if (is.null(name)) {
        return(paste0("tem_", generate_id(16)))
      } else {
        return(name)
      }
    },
    #--------------------------------------------------------------------------
    # Method for setting the model info
    set_model_info = function(model_name, label, model_date, model_language) {
      private$model_info$model_name <- model_name
      private$model_info$model_label <- label
      private$model_info$model_date <- model_date
      private$model_info$model_language <- model_language
    },
    #-------------------------------------------------------------------------
    load_reload_python_scripts = function() {
      load_py_scripts(
        files = c(
          "pytorch_layers.py",
          "MPNetForMPLM_PT.py"
        )
      )
    },
    #-------------------------------------------------------------------------
    # Method for checking and setting the embedding configuration
    check_and_set_embedding_layers = function(emb_layer_min,
                                              emb_layer_max) {
      if (self$BaseModel$get_model_type() == "funnel") {
        max_layers_funnel <- sum(
          self$BaseModel$get_model()$config$block_repeats *
            self$BaseModel$get_model()$config$block_sizes
        )

        if (emb_layer_min == "First") {
          emb_layer_min <- 1
        } else if (emb_layer_min == "Middle") {
          emb_layer_min <- floor(0.5 * max_layers_funnel)
        } else if (emb_layer_min == "2_3_layer") {
          emb_layer_min <- floor(2 / 3 * max_layers_funnel)
        } else if (emb_layer_min == "Last") {
          emb_layer_min <- max_layers_funnel
        }

        if (emb_layer_max == "First") {
          emb_layer_max <- 1
        } else if (emb_layer_max == "Middle") {
          emb_layer_max <- floor(0.5 * max_layers_funnel)
        } else if (emb_layer_max == "2_3_layer") {
          emb_layer_max <- floor(2 / 3 * max_layers_funnel)
        } else if (emb_layer_max == "Last") {
          emb_layer_max <- max_layers_funnel
        }
      } else {
        if (emb_layer_min == "First") {
          emb_layer_min <- 1
        } else if (emb_layer_min == "Middle") {
          emb_layer_min <- floor(0.5 * self$BaseModel$get_model()$config$num_hidden_layers)
        } else if (emb_layer_min == "2_3_layer") {
          emb_layer_min <- floor(2 / 3 * self$BaseModel$get_model()$config$num_hidden_layers)
        } else if (emb_layer_min == "Last") {
          emb_layer_min <- self$BaseModel$get_model()$config$num_hidden_layers
        }

        if (emb_layer_max == "First") {
          emb_layer_max <- 1
        } else if (emb_layer_max == "Middle") {
          emb_layer_max <- floor(0.5 * self$BaseModel$get_model()$config$num_hidden_layers)
        } else if (emb_layer_max == "2_3_layer") {
          emb_layer_max <- floor(2 / 3 * self$BaseModel$get_model()$config$num_hidden_layers)
        } else if (emb_layer_max == "Last") {
          emb_layer_max <- self$BaseModel$get_model()$config$num_hidden_layers
        }
      }

      # Check requested configuration
      if (emb_layer_min > emb_layer_max) {
        stop("emb_layer_min layer must be smaller or equal emb_layer_max.")
      }
      if (emb_layer_min < 1) {
        stop("emb_laser_min must be at least 1.")
      }
      if (self$BaseModel$get_model_type() == "funnel") {
        if (emb_layer_max > self$BaseModel$get_model()$config$num_hidden_layers) {
          stop(paste0(
            "emb_layer_max can not exceed the number of layers. The transformer has",
            max_layers_funnel, "layers."
          ))
        }
      } else {
        if (emb_layer_max > self$BaseModel$get_model()$config$num_hidden_layers) {
          stop(paste0(
            "emb_layer_max can not exceed the number of layers. The transformer has",
            self$BaseModel$get_model()$config$num_hidden_layers, "layers."
          ))
        }
      }

      if (is.integer(as.integer(emb_layer_min)) == FALSE | is.integer(as.integer(emb_layer_max)) == FALSE) {
        stop("emb_layer_min and emb_layer_max must be integers or the following string:
               'first','last','middle','2_3_layer'")
      }

      private$model_config$emb_layer_min <- emb_layer_min
      private$model_config$emb_layer_max <- emb_layer_max
    },
    #-------------------------------------------------------------------------
    # Method for checking and setting pooling type
    check_and_set_pooling_type = function(emb_pool_type) {
      if (emb_pool_type %in% c("CLS", "Average") == FALSE) {
        stop("emb_pool_type must be 'cls' or 'average'.")
      }
      if (self$BaseModel$get_model_type() == "funnel" & emb_pool_type != "CLS") {
        stop("Funnel currently supports only cls as pooling type.")
      }
      private$model_config$emb_pool_type <- emb_pool_type
    },
    #-------------------------------------------------------------------------
    # Method for checking and setting max_length
    check_and_set_max_length = function(max_length) {
      if (max_length > (self$BaseModel$get_model()$config$max_position_embeddings)) {
        stop(paste(
          "max_length is", max_length, ". This value is not allowed to exceed",
          self$BaseModel$get_model()$config$max_position_embeddings
        ))
      } else {
        private$model_config$max_length <- as.integer(max_length)
      }
    },
    #-------------------------------------------------------------------------
    update_model_config = function() {
      # check if an update of values is necessary. This is the case if the model
      # was created with an older version of aifeducation compared to 1.1.0
      # Update values to the new values introduced in version 1.1.0

      current_pkg_version <- self$get_package_versions()$r_package_versions$aifeducation
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

      tmp_model_config_params <- names(private$transformer_components)
      if (update_values) {
        for (param in tmp_model_config_params) {
          private$transformer_components[param] <- list(update_values_to_new_1.1.0(private$transformer_components[[param]]))
        }
      }
      private$r_package_versions$aifeducation <- packageVersion("aifeducation")
    }
  ),
  public = list(
    BaseModel = NULL,

    #--------------------------------------------------------------------------
    #' @description Method for creating a new text embedding model
    #' @param model_name `string` containing the name of the new model.
    #' @param model_label `string` containing the label/title of the new model.
    #' @param model_language `string` containing the language which the model
    #' represents (e.g., English).
    #' @param max_length `int` determining the maximum length of token
    #' sequences used in transformer models. Not relevant for the other methods.
    #' @param chunks `int` Maximum number of chunks. Must be at least 2.
    #' @param overlap `int` determining the number of tokens which should be added
    #' at the beginning of the next chunk. Only relevant for transformer models.
    #' @param emb_layer_min `int` or `string` determining the first layer to be included
    #' in the creation of embeddings. An integer correspondents to the layer number. The first
    #' layer has the number 1. Instead of an integer the following strings are possible:
    #' `"start"` for the first layer, `"Middle"` for the middle layer,
    #' `"2_3_layer"` for the layer two-third layer, and `"Last"` for the last layer.
    #' @param emb_layer_max `int` or `string` determining the last layer to be included
    #' in the creation of embeddings. An integer correspondents to the layer number. The first
    #' layer has the number 1. Instead of an integer the following strings are possible:
    #' `"start"` for the first layer, `"Middle"` for the middle layer,
    #' `"2_3_layer"` for the layer two-third layer, and `"Last"` for the last layer.
    #' @param emb_pool_type `string` determining the method for pooling the token embeddings
    #' within each layer. If `"CLS"` only the embedding of the CLS token is used. If
    #' `"Average"` the token embedding of all tokens are averaged (excluding padding tokens).
    #' `"cls` is not supported for `method="funnel"`.
    #' @param pad_value `r get_param_doc_desc("pad_value")`
    #' @param model_dir `string` path to the directory where the
    #' BERT model is stored.
    #' @param trace `bool` `TRUE` prints information about the progress.
    #' `FALSE` does not.
    #' @return Returns an object of class [TextEmbeddingModel].
    #'
    #' @import reticulate
    #' @import stats
    #' @import reshape2
    configure = function(model_name = NULL,
                         model_label = NULL,
                         model_language = NULL,
                         max_length = 0,
                         chunks = 2,
                         overlap = 0,
                         emb_layer_min = "Middle",
                         emb_layer_max = "2_3_layer",
                         emb_pool_type = "Average",
                         pad_value = -100,
                         base_model = NULL) {
      # Load or reload python scripts
      private$load_reload_python_scripts()

      # Check if the object is not configured
      private$check_config_for_FALSE()

      # Load BaseModel
      self$BaseModel <- base_model$clone(deep = TRUE)

      # Save Embedding Config
      private$save_all_args(args = get_called_args(n = 1), group = "configure")

      # Set Model info
      private$set_model_info(
        model_name = private$generate_model_id(model_name),
        label = model_label,
        model_date = date(),
        model_language = model_language
      )

      # Check and Set Embedding Configuration
      private$check_and_set_embedding_layers(
        emb_layer_min = emb_layer_min,
        emb_layer_max = emb_layer_max
      )

      #Check and set max length
      private$check_and_set_max_length(max_length)

      # Check and set pooling type
      private$check_and_set_pooling_type(emb_pool_type)

      # Close config
      private$set_configuration_to_TRUE()
    },
    #--------------------------------------------------------------------------
    #' @description loads an object from disk
    #' and updates the object to the current version of the package.
    #' @param dir_path Path where the object set is stored.
    #' @return Method does not return anything. It loads an object from disk.
    load_from_disk = function(dir_path) {
      # Load private and public config files
      private$load_config_file(dir_path)

      # Load or reload python scripts
      private$load_reload_python_scripts()

      # Load Base model
      self$BaseModel <- load_from_disk(dir_path = paste0(dir_path, "/", "base_model"))

      # Update config if necessary
      private$update_model_config()

      # Set model info
      # private$set_model_info(
      #  model_name = config_file$private$model_info$model_name,
      #  label = config_file$private$model_info$model_label,
      #  model_date = config_file$private$model_info$model_date,
      #  model_language = config_file$private$model_info$model_language
      # )

      # Finalize config
      private$set_configuration_to_TRUE()
    },
    #--------------------------------------------------------------------------
    #' @description Method for saving a transformer model on disk.Relevant
    #' only for transformer models.
    #' @param dir_path `string` containing the path to the relevant
    #' model directory.
    #' @param folder_name `string` Name for the folder created within the directory.
    #' This folder contains all model files.
    #' @return Function does not return a value. It is used for saving a transformer model
    #' to disk.
    #'
    #' @importFrom utils write.csv
    save = function(dir_path, folder_name) {
      save_location <- paste0(dir_path, "/", folder_name)
      create_dir(dir_path = save_location, trace = FALSE)

      # Save BaseModel
      save_to_disk(
        object = self$BaseModel,
        dir_path = save_location,
        folder_name = "base_model"
      )
    },
    #-------------------------------------------------------------------------
    #' @description Method for encoding words of raw texts into integers.
    #' @param raw_text `vector`containing the raw texts.
    #' @param token_encodings_only `bool` If `TRUE`, only the token
    #' encodings are returned. If `FALSE`, the complete encoding is returned
    #' which is important for some transformer models.
    #' @param to_int `bool` If `TRUE` the integer ids of the tokens are
    #' returned. If `FALSE` the tokens are returned. Argument only applies
    #' for transformer models and if `token_encodings_only=TRUE`.
    #' @param trace `bool` If `TRUE`, information of the progress
    #' is printed. `FALSE` if not requested.
    #' @return `list` containing the integer or token sequences of the raw texts with
    #' special tokens.
    encode = function(raw_text,
                      token_encodings_only = FALSE,
                      to_int = TRUE,
                      trace = FALSE) {
      return(
        self$BaseModel$Tokenizer$encode(
          raw_text = raw_text,
          token_overlap = private$model_config$overlap,
          max_token_sequence_length = private$model_config$max_length,
          n_chunks = private$model_config$chunks,
          token_encodings_only = token_encodings_only,
          to_int = to_int,
          trace = trace
        )
      )
    },
    decode = function(int_seqence, to_token = FALSE){
      return(
        self$BaseModel$Tokenizer$decode(
          int_seqence=int_seqence,
          to_token = to_token
        )
      )
    },
    # Embedding------------------------------------------------------------------
    #' @description Method for creating text embeddings from raw texts.
    #' This method should only be used if a small number of texts should be transformed
    #' into text embeddings. For a large number of texts please use the method `embed_large`.
    #' @param raw_text `vector` containing the raw texts.
    #' @param doc_id `vector` containing the corresponding IDs for every text.
    #' @param batch_size `int` determining the maximal size of every batch.
    #' @param trace `bool` `TRUE`, if information about the progression
    #' should be printed on console.
    #' @param return_large_dataset 'bool' If `TRUE` the retuned object is of class
    #' [LargeDataSetForTextEmbeddings]. If `FALSE` it is of class [EmbeddedText]
    #' @return Method returns an object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings]. This object
    #' contains the embeddings as a [data.frame] and information about the
    #' model creating the embeddings.
    embed = function(raw_text = NULL, doc_id = NULL, batch_size = 8, trace = FALSE, return_large_dataset = FALSE) {
      # check arguments
      check_type(object = raw_text, type = "vector", FALSE)
      check_type(object = doc_id, type = "vector", FALSE)
      check_type(object = batch_size, type = "int", FALSE)
      check_type(object = trace, type = "bool", FALSE)
      check_type(object = return_large_dataset, type = "bool", FALSE)

      # transformer---------------------------------------------------------------------
      n_units <- length(raw_text)
      n_layer <- self$BaseModel$get_model()$config$num_hidden_layers
      n_layer_size <- self$BaseModel$get_model()$config$hidden_size

      # Batch refers to the number of cases
      n_batches <- ceiling(n_units / batch_size)
      batch_results <- NULL

      if (private$model_config$emb_pool_type == "Average") {
        reticulate::py_run_file(system.file("python/pytorch_old_scripts.py",
          package = "aifeducation"
        ))
        pooling <- py$layer_global_average_pooling_1d(mask_type = "attention")
        pooling$eval()
      }

      for (b in 1:n_batches) {
        # Set model to evaluation mode
        self$BaseModel$get_model()$eval()
        if (torch$cuda$is_available()) {
          pytorch_device <- "cuda"
          pytorch_dtype <- torch$float
        } else {
          pytorch_device <- "cpu"
          pytorch_dtype <- torch$double
        }
        self$BaseModel$get_model()$to(pytorch_device, dtype = pytorch_dtype)
        if (private$model_config$emb_pool_type == "Average") {
          pooling$to(pytorch_device)
        }


        index_min <- 1 + (b - 1) * batch_size
        index_max <- min(b * batch_size, n_units)
        batch <- index_min:index_max

        tokens <- self$BaseModel$Tokenizer$encode(
          raw_text = raw_text[batch],
          trace = trace,
          token_encodings_only = FALSE,
          token_overlap = private$model_config$overlap,
          max_token_sequence_length = private$model_config$max_length,
          n_chunks = private$model_config$chunks,
          to_int = TRUE,
          return_token_type_ids <- (self$BaseModel$get_model_type() != AIFETrType$mpnet)
        )

        text_embedding <- array(
          data = private$model_config$pad_value,
          dim = c(
            length(batch),
            private$model_config$chunks,
            n_layer_size
          )
        )

        # Selecting the relevant layers
        selected_layer <- private$model_config$emb_layer_min:private$model_config$emb_layer_max
        tmp_selected_layer <- 1 + selected_layer

        # Clear memory
        if (torch$cuda$is_available()) {
          torch$cuda$empty_cache()
        }

        # Calculate tensors
        tokens$encodings$set_format(type = "torch")

        with(
          data = torch$no_grad(),
          {
            if (self$BaseModel$get_model_type() == AIFETrType$mpnet) {
              tensor_embeddings <- self$BaseModel$get_model()(
                input_ids = tokens$encodings["input_ids"]$to(pytorch_device),
                attention_mask = tokens$encodings["attention_mask"]$to(pytorch_device),
                output_hidden_states = TRUE
              )$hidden_states
            } else if (self$BaseModel$get_model_type() == AIFETrType$modernbert) {
              tensor_embeddings <- self$BaseModel$get_model()(
                input_ids = tokens$encodings["input_ids"]$to(pytorch_device),
                attention_mask = tokens$encodings["attention_mask"]$to(pytorch_device),
                output_hidden_states = TRUE
              )$hidden_states
            } else {
              tensor_embeddings <- self$BaseModel$get_model()(
                input_ids = tokens$encodings["input_ids"]$to(pytorch_device),
                attention_mask = tokens$encodings["attention_mask"]$to(pytorch_device),
                token_type_ids = tokens$encodings["token_type_ids"]$to(pytorch_device),
                output_hidden_states = TRUE
              )$hidden_states
            }
          }
        )

        if (private$model_config$emb_pool_type == "Average") {
          # Average Pooling over all tokens of a layer
          for (i in tmp_selected_layer) {
            # abc=pooling(
            #  x = tensor_embeddings[[as.integer(i)]]$to(pytorch_device),
            #  mask = tokens$encodings["attention_mask"]$to(pytorch_device)
            # )
            # print(abc)
            tensor_embeddings[i] <- list(pooling(
              x = tensor_embeddings[[as.integer(i)]]$to(pytorch_device),
              mask = tokens$encodings["attention_mask"]$to(pytorch_device)
            ))
          }
        }

        # Sorting the hidden states to the corresponding cases and times
        # If more than one layer is selected the mean is calculated
        index <- 0
        for (i in seq_len(length(batch))) {
          for (j in 1:tokens$chunks[i]) {
            for (layer in tmp_selected_layer) {
              layer_int <- as.integer(layer)
              index_int <- as.integer(index)

              # Set values to zero to remove padding value
              text_embedding[i, j, ] <- 0

              if (torch$cuda$is_available() == FALSE) {
                if (private$model_config$emb_pool_type == "CLS") {
                  # CLS Token is always the first token
                  text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                    tensor_embeddings[[layer_int]][[index_int]][[as.integer(0)]]$detach()$numpy()
                  )
                } else if (private$model_config$emb_pool_type == "Average") {
                  text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                    tensor_embeddings[[layer_int]][[index_int]]$detach()$numpy()
                  )
                }
              } else {
                if (private$model_config$emb_pool_type == "CLS") {
                  # CLS Token is always the first token
                  text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                    tensor_embeddings[[layer_int]][[index_int]][[as.integer(0)]]$detach()$cpu()$numpy()
                  )
                } else if (private$model_config$emb_pool_type == "Average") {
                  text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                    tensor_embeddings[[layer_int]][[index_int]]$detach()$cpu()$numpy()
                  )
                }
              }
            }
            text_embedding[i, j, ] <- text_embedding[i, j, ] / length(tmp_selected_layer)
            index <- index + 1
          }
        }
        dimnames(text_embedding)[[3]] <- paste0(
          self$BaseModel$get_model_type(), "_",
          seq(from = 1, to = n_layer_size, by = 1)
        )

        # Add ID of every case
        dimnames(text_embedding)[[1]] <- doc_id[batch]
        batch_results[b] <- list(text_embedding)
        if (trace == TRUE) {
          cat(paste(
            date(),
            "Batch", b, "/", n_batches, "Done", "\n"
          ))
        }
        base::gc(verbose = FALSE, full = TRUE)
      }

      # Summarizing the results over all batches
      text_embedding <- array_form_bind(batch_results)

      embeddings <- EmbeddedText$new()
      embeddings$configure(
        model_name = private$model_info$model_name,
        model_label = private$model_info$model_label,
        model_date = private$model_info$model_date,
        model_method = self$BaseModel$get_model_type(),
        model_language = private$model_info$model_language,
        param_seq_length = private$model_config$max_length,
        param_features = dim(text_embedding)[3],
        param_chunks = private$model_config$chunks,
        param_overlap = private$model_config$overlap,
        param_emb_layer_min = private$model_config$emb_layer_min,
        param_emb_layer_max = private$model_config$emb_layer_max,
        param_emb_pool_type = private$model_config$emb_pool_type,
        param_pad_value = private$model_config$pad_value,
        param_aggregation = NA,
        embeddings = text_embedding
      )

      if (return_large_dataset == FALSE) {
        return(embeddings)
      } else {
        embedded_texts_large <- LargeDataSetForTextEmbeddings$new()
        embedded_texts_large$configure(
          model_name = private$model_info$model_name,
          model_label = private$model_info$model_label,
          model_date = private$model_info$model_date,
          model_method = self$BaseModel$get_model_type(),
          model_language = private$model_info$model_language,
          param_seq_length = private$model_config$max_length,
          param_features = dim(embeddings$embeddings)[length(dim(embeddings$embeddings))],
          param_chunks = private$model_config$chunks,
          param_overlap = private$model_config$overlap,
          param_emb_layer_min = private$model_config$emb_layer_min,
          param_emb_layer_max = private$model_config$emb_layer_max,
          param_emb_pool_type = private$model_config$emb_pool_type,
          param_pad_value = private$model_config$pad_value,
          param_aggregation = NA
        )
        # Add new data
        embedded_texts_large$add_embeddings_from_EmbeddedText(embeddings)
        return(embedded_texts_large)
      }
    },
    #--------------------------------------------------------------------------
    #' @description Method for creating text embeddings from raw texts.
    #' @param large_datas_set Object of class [LargeDataSetForText] containing the
    #' raw texts.
    #' @param batch_size `int` determining the maximal size of every batch.
    #' @param trace `bool` `TRUE`, if information about the progression
    #' should be printed on console.
    #' @param log_file `string` Path to the file where the log should be saved.
    #' If no logging is desired set this argument to `NULL`.
    #' @param log_write_interval `int` Time in seconds determining the interval in which
    #' the logger should try to update the log files. Only relevant if `log_file` is not `NULL`.
    #' @return Method returns an object of class [LargeDataSetForTextEmbeddings].
    embed_large = function(large_datas_set, batch_size = 32, trace = FALSE,
                           log_file = NULL,
                           log_write_interval = 2) {
      # Check arguments
      check_class(object = large_datas_set, classes = c("LargeDataSetForText", allow_NULL = FALSE))
      check_type(object = batch_size, type = "int", FALSE)
      check_type(object = trace, type = "bool", FALSE)

      # Get total number of batches for the loop
      total_number_of_bachtes <- ceiling(large_datas_set$n_rows() / batch_size)

      # Get indices for every batch
      batches_index <- get_batches_index(
        number_rows = large_datas_set$n_rows(),
        batch_size = batch_size,
        zero_based = TRUE
      )
      # Set up log
      last_log <- NULL

      # Process every batch
      for (i in 1:total_number_of_bachtes) {
        subset <- large_datas_set$select(as.integer(batches_index[[i]]))
        embeddings <- self$embed(
          raw_text = c(subset["text"]),
          doc_id = c(subset["id"]),
          batch_size = batch_size,
          trace = FALSE
        )
        if (i == 1) {
          # Create Large Dataset
          embedded_texts_large <- LargeDataSetForTextEmbeddings$new()
          embedded_texts_large$configure(
            model_name = private$model_info$model_name,
            model_label = private$model_info$model_label,
            model_date = private$model_info$model_date,
            model_method = self$BaseModel$get_model_type(),
            model_language = private$model_info$model_language,
            param_seq_length = private$model_config$max_length,
            param_features = dim(embeddings$embeddings)[3],
            param_chunks = private$model_config$chunks,
            param_overlap = private$model_config$overlap,
            param_emb_layer_min = private$model_config$emb_layer_min,
            param_emb_layer_max = private$model_config$emb_layer_max,
            param_emb_pool_type = private$model_config$emb_pool_type,
            param_aggregation = NA,
            param_pad_value = private$model_config$pad_value
          )
          # Add new data
          embedded_texts_large$add_embeddings_from_EmbeddedText(embeddings)
        } else {
          # Add new data
          embedded_texts_large$add_embeddings_from_EmbeddedText(embeddings)
        }
        if (trace == TRUE) {
          cat(paste(
            date(),
            "Batch", i, "/", total_number_of_bachtes, "done", "\n"
          ))
        }

        # Update log
        last_log <- write_log(
          log_file = log_file,
          last_log = last_log,
          write_interval = log_write_interval,
          value_top = i,
          value_middle = 0,
          value_bottom = 0,
          total_top = total_number_of_bachtes,
          total_middle = 1,
          total_bottom = 1,
          message_top = "Batches",
          message_middle = NA,
          message_bottom = NA
        )
        gc()
      }
      return(embedded_texts_large)
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting the number of features.
    #' @return Returns a `double` which represents the number of features. This number represents the
    #' hidden size of the embeddings for every chunk or time.
    get_n_features = function() {
      return(self$BaseModel$get_final_size())
    },
    #' @description Value for indicating padding.
    #' @return Returns an `int` describing the value used for padding.
    get_pad_value = function() {
      return(private$model_config$pad_value)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting the bibliographic information of the model.
    #' @param type `string` Type of information which should be changed/added.
    #' `developer`, and `modifier` are possible.
    #' @param authors List of people.
    #' @param citation `string` Citation in free text.
    #' @param url `string` Corresponding URL if applicable.
    #' @return Function does not return a value. It is used to set the private
    #' members for publication information of the model.
    set_publication_info = function(type,
                                    authors,
                                    citation,
                                    url = NULL) {
      if (type == "developer") {
        private$publication_info$developed_by$authors <- authors
        private$publication_info$developed_by$citation <- citation
        private$publication_info$developed_by$url <- url
      } else if (type == "modifier") {
        private$publication_info$modified_by$authors <- authors
        private$publication_info$modified_by$citation <- citation
        private$publication_info$modified_by$url <- url
      }
    }
  )
)

#Add Object to index
TextEmbeddingObjectsIndex$TextEmbeddingModel="TextEmbeddingModel"
