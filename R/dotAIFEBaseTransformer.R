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

#' @title Base `R6` class for creation and definition of `.AIFE*Transformer-like` classes
#'
#' @description This base class is used to create and define `.AIFE*Transformer-like` classes. It serves as a skeleton
#'   for a future concrete transformer and cannot be used to create an object of itself (an attempt to call `new`-method
#'   will produce an error).
#'
#'   See p.1 Base Transformer Class in
#'   [Transformers for Developers](https://fberding.github.io/aifeducation/articles/transformers.html) for details.
#'
#' @section Create: The `create`-method is a basic algorithm that is used to create a new transformer, but cannot be
#'   called directly.
#'
#' @section Train: The `train`-method is a basic algorithm that is used to train and tune the transformer but cannot be
#'   called directly.
#'
#' @section Concrete transformer implementation: There are already implemented concrete (child) transformers (e.g.
#'   `BERT`, `DeBERTa-V2`, etc.), to implement a new one see p.4 Implement A Custom Transformer in
#'   [Transformers for Developers](https://fberding.github.io/aifeducation/articles/transformers.html)
#'
#' @param text_dataset `r get_param_doc_desc("text_dataset")`
#' @param sustain_track `r get_param_doc_desc("sustain_track")`
#' @param sustain_iso_code `r get_param_doc_desc("sustain_iso_code")`
#' @param sustain_region `r get_param_doc_desc("sustain_region")`
#' @param sustain_interval `r get_param_doc_desc("sustain_interval")`
#' @param trace `r get_param_doc_desc("trace")`
#' @param pytorch_safetensors `r get_param_doc_desc("pytorch_safetensors")`
#' @param log_dir `r get_param_doc_desc("log_dir")`
#' @param log_write_interval `r get_param_doc_desclog_write_interval
#'
#' @references Hugging Face transformers documantation:
#'   * [BERT](https://huggingface.co/docs/transformers/model_doc/bert)
#'   * [DeBERTa](https://huggingface.co/docs/transformers/model_doc/deberta-v2)
#'   * [Funnel](https://huggingface.co/docs/transformers/model_doc/funnel)
#'   * [Longformer](https://huggingface.co/docs/transformers/model_doc/longformer)
#'   * [RoBERTa](https://huggingface.co/docs/transformers/model_doc/roberta)
#'   * [MPNet](https://huggingface.co/docs/transformers/model_doc/mpnet)
#'
#' @family R6 classes for transformers
#' @export
.AIFEBaseTransformer <- R6::R6Class( # nolint
  classname = ".AIFEBaseTransformer",
  private = list(
    # == Attributes ====================================================================================================

    # Transformer's title
    title = "Transformer Model",

    # This object is used to track sustainability on demand
    # It can be created with the private `create_sustain_tracker()` method.
    sustainability_tracker = NULL,

    # Required and optional steps to create a new transformer
    steps_for_creation = list(
      # required in each transformer
      create_tokenizer_draft = NULL,
      calculate_vocab = NULL,
      save_tokenizer_draft = NULL,
      create_final_tokenizer = NULL,
      create_transformer_model = NULL,
      # optional
      check_max_pos_emb = NULL,
      # required and already defined for all transformers
      save_transformer_model = NULL
    ),

    # Required steps to train the transformer
    steps_for_training = list(
      # required
      load_existing_model = NULL,
      # optional
      cuda_empty_cache = NULL,
      # required and already defined steps
      check_chunk_size = NULL,
      create_chunks_for_training = NULL,
      prepare_train_tune = NULL,
      start_training = NULL,
      save_model = NULL,
      # required, already defined steps. Can be overwritten with a custom version
      create_data_collator = NULL
    ),

    # == Methods =======================================================================================================

    # Clear methods ----

    # Clears the variables of the class: `sustainability_tracker`, `params` and `temp`
    clear_variables = function() {
      private$sustainability_tracker <- NULL
      self$params <- list()
      self$temp <- list()
    },


    # Check methods ----

    # Checks whether a required creation step is missing
    check_required_SFC = function() {
      if (!is.function(private$steps_for_creation$create_tokenizer_draft)) {
        stop("'steps_for_creation$create_tokenizer_draft' is not a function")
      }
      if (!is.function(private$steps_for_creation$calculate_vocab)) {
        stop("'steps_for_creation$calculate_vocab' is not a function")
      }
      if (!is.function(private$steps_for_creation$save_tokenizer_draft)) {
        stop("'steps_for_creation$save_tokenizer_draft' is not a function")
      }
      if (!is.function(private$steps_for_creation$create_final_tokenizer)) {
        stop("'steps_for_creation$create_final_tokenizer' is not a function")
      }
      if (!is.function(private$steps_for_creation$create_transformer_model)) {
        stop("'steps_for_creation$create_transformer_model' is not a function")
      }
    },


    # Checks whether a required training step is missing
    check_required_SFT = function() {
      if (!is.function(private$steps_for_training$load_existing_model)) {
        stop("'steps_for_training$load_existing_model' is not a function")
      }
    },


    # Other ----

    # Initializes the 'static' parameters of the transformer in the `param` list
    init_common_model_params = function(sustain_track,
                                        sustain_iso_code,
                                        sustain_region,
                                        sustain_interval,
                                        trace,
                                        pytorch_safetensors,
                                        log_dir,
                                        log_write_interval,
                                        text_dataset) {
      self$set_model_param("sustain_track", sustain_track)
      self$set_model_param("sustain_iso_code", sustain_iso_code)
      self$set_model_param("sustain_region", sustain_region)
      self$set_model_param("sustain_interval", sustain_interval)
      self$set_model_param("trace", trace)
      self$set_model_param("pytorch_safetensors", pytorch_safetensors)
      self$set_model_param("log_dir", log_dir)
      self$set_model_param("log_write_interval", log_write_interval)
      self$set_model_param("text_dataset", text_dataset)
    },


    # -------------------------------------------------------------------------------
    define_required_SFC_functions = function() {
      if (!is.function(private$steps_for_creation$save_transformer_model)) {
        private$steps_for_creation$save_transformer_model <- function(self) {
          self$temp$model$save_pretrained(
            save_directory = self$params$model_dir,
            safe_serilization = self$temp$pt_safe_save
          )
        }
      }
    },


    # -------------------------------------------------------------------------------
    define_required_SFT_functions = function() {
      # SFT: check_chunk_size -------------------------------------------------------
      if (!is.function(private$steps_for_training$check_chunk_size)) {
        private$steps_for_training$check_chunk_size <- function(self) {
          if (self$params$chunk_size > (self$temp$model$config$max_position_embeddings)) {
            stop(
              paste(
                "Chunk size is", self$params$chunk_size, ". This value is not allowed to exceed",
                self$temp$model$config$max_position_embeddings
              )
            )
          }
          if (self$params$chunk_size < 3) {
            stop("Chunk size must be at least 3.")
          }
          # adjust chunk size. To elements are needed for begin and end of sequence
          self$params$chunk_size <- self$params$chunk_size - 2
        }
      }


      # SFT: create_chunks_for_training ---------------------------------------------
      if (!is.function(private$steps_for_training$create_chunks_for_training)) {
        private$steps_for_training$create_chunks_for_training <- function(self) {
          tokenized_texts_raw <- tokenize_dataset(
            dataset = self$temp$raw_text_dataset,
            tokenizer = self$temp$tokenizer,
            max_length = self$params$chunk_size,
            log_file = self$temp$log_file,
            write_interval = self$temp$write_interval,
            value_top = self$temp$value_top,
            total_top = self$temp$total_top,
            message_top = self$temp$message_top
          )


          length_vector <- tokenized_texts_raw["length"]
          if (self$params$full_sequences_only) {
            relevant_indices <- which(length_vector == self$params$chunk_size)
          } else {
            relevant_indices <- which(
              length_vector <= self$params$chunk_size &
                length_vector >= self$params$min_seq_len
            )
          }

          if (length(relevant_indices) == 0) {
            self$temp$tokenized_dataset <- tokenized_texts_raw
          } else {
            self$temp$tokenized_dataset <- tokenized_texts_raw$select(as.integer(relevant_indices - 1))
          }
          private$update_tokenizer_statistics(self$temp$tokenized_dataset, "training")
        }
      }


      # SFT: create_data_collator ---------------------------------------------------
      if (!is.function(private$steps_for_training$create_data_collator)) {
        # Create default data collator
        # For mpnet transformer see AIFEMpnetTransformer -> SFT -> create_data_collator
        private$steps_for_training$create_data_collator <- function(self) {
          if (self$params$whole_word) {
            self$temp$data_collator <- transformers$DataCollatorForWholeWordMask(
              tokenizer = self$temp$tokenizer,
              mlm = TRUE,
              mlm_probability = self$params$p_mask,
              return_tensors = self$temp$return_tensors
            )
          } else {
            self$temp$data_collator <- transformers$DataCollatorForLanguageModeling(
              tokenizer = self$temp$tokenizer,
              mlm = TRUE,
              mlm_probability = self$params$p_mask,
              return_tensors = self$temp$return_tensors
            )
          }
        }
      }


      # SFT: prepare_train_tune -----------------------------------------------------
      if (!is.function(private$steps_for_training$prepare_train_tune)) {
        private$steps_for_training$prepare_train_tune <- function(self) {
          msg <- ifelse(self$params$whole_word, "Using Whole Word Masking", "Using Token Masking")
          print_message(msg, self$params$trace)

          self$temp$return_tensors <- "pt"

          # Create data collator
          private$steps_for_training$create_data_collator(self)

          # ----
          self$temp$tokenized_dataset$set_format(type = "torch")
          self$temp$tokenized_dataset <- self$temp$tokenized_dataset$train_test_split(
            test_size = self$params$val_size
          )

          print_message("Preparing Training of the Model", self$params$trace)
          # Create Custom Callbacks ----

          run_py_file("pytorch_transformer_callbacks.py")
          create_logger <- py$create_AIFETransformerCSVLogger_PT

          logger_args <- list(
            loss_file = self$temp$loss_file,
            log_file = self$temp$log_file,
            value_top = 6,
            total_top = 10,
            message_top = "Overall: Training...",
            min_step = 1
          )
          logger <- do.call(create_logger, logger_args)

          if (check_versions(a = get_py_package_version("transformers"), operator = ">=", b = "4.46.0")) {
            training_args <- transformers$TrainingArguments(
              output_dir = private$dir_checkpoint,
              overwrite_output_dir = TRUE,
              eval_strategy = "epoch",
              num_train_epochs = as.integer(self$params$n_epoch),
              logging_strategy = "epoch",
              save_strategy = "epoch",
              save_total_limit = as.integer(1),
              load_best_model_at_end = TRUE,
              optim = "adamw_torch",
              learning_rate = self$params$learning_rate,
              per_device_train_batch_size = as.integer(self$params$batch_size),
              per_device_eval_batch_size = as.integer(self$params$batch_size),
              save_safetensors = TRUE,
              auto_find_batch_size = FALSE,
              report_to = "none",
              log_level = "error",
              disable_tqdm = !self$params$pytorch_trace,
              dataloader_pin_memory = torch$cuda$is_available()
            )
          } else {
            training_args <- transformers$TrainingArguments(
              output_dir = private$dir_checkpoint,
              overwrite_output_dir = TRUE,
              evaluation_strategy = "epoch",
              num_train_epochs = as.integer(self$params$n_epoch),
              logging_strategy = "epoch",
              save_strategy = "epoch",
              save_total_limit = as.integer(1),
              load_best_model_at_end = TRUE,
              optim = "adamw_torch",
              learning_rate = self$params$learning_rate,
              per_device_train_batch_size = as.integer(self$params$batch_size),
              per_device_eval_batch_size = as.integer(self$params$batch_size),
              save_safetensors = TRUE,
              auto_find_batch_size = FALSE,
              report_to = "none",
              log_level = "error",
              disable_tqdm = !self$params$pytorch_trace
            )
          }

          if (check_versions(a = get_py_package_version("transformers"), operator = ">=", b = "4.46.0")) {
            self$temp$trainer <- transformers$Trainer(
              model = self$temp$model,
              train_dataset = self$temp$tokenized_dataset$train,
              eval_dataset = self$temp$tokenized_dataset$test,
              args = training_args,
              data_collator = self$temp$data_collator,
              processing_class = self$temp$tokenizer
            )
          } else {
            self$temp$trainer <- transformers$Trainer(
              model = self$temp$model,
              train_dataset = self$temp$tokenized_dataset$train,
              eval_dataset = self$temp$tokenized_dataset$test,
              args = training_args,
              data_collator = self$temp$data_collator,
              tokenizer = self$temp$tokenizer
            )
          }

          self$temp$trainer$remove_callback(transformers$integrations$CodeCarbonCallback)
          if (!as.logical(self$params$pytorch_trace)) {
            self$temp$trainer$remove_callback(transformers$PrinterCallback)
            self$temp$trainer$remove_callback(transformers$ProgressCallback)
          }

          # Add Callbacks
          self$temp$trainer$add_callback(logger)
        }
      }


      # SFT: start_training ---------------------------------------------------------
      if (!is.function(private$steps_for_training$start_training)) {
        private$steps_for_training$start_training <- function(self) {
          if (is.function(private$steps_for_training$cuda_empty_cache)) {
            private$steps_for_training$cuda_empty_cache()
          }
          private$create_checkpoint_directory()
          self$temp$trainer$train()
          private$clean_checkpoint_directory()
        }
      }

      # SFT: save_model -------------------------------------------------------------
      if (!is.function(private$steps_for_training$save_model)) {
        private$steps_for_training$save_model <- function(self) {
          self$temp$model$save_pretrained(
            save_directory = self$params$output_dir,
            safe_serilization = self$temp$pt_safe_save
          )
          history_log <- pandas$DataFrame(self$temp$trainer$state$log_history)
          history_log <- clean_pytorch_log_transformers(history_log)

          # Write history log
          write.csv2(
            history_log,
            file = paste0(self$params$output_dir, "/history.log"),
            row.names = FALSE,
            quote = FALSE
          )
        }
      }
    },


    # Creates a sustainability tracker and stores it in the private `sustainability_tracker` attribute
    create_sustain_tracker = function() {
      if (check_versions(a = get_py_package_version("codecarbon"), operator = ">=", b = "2.8.0")) {
        path_look_file <- codecarbon$lock$LOCKFILE
        if (file.exists(path_look_file)) {
          unlink(path_look_file)
        }
      }
      private$sustainability_tracker <- codecarbon$OfflineEmissionsTracker(
        country_iso_code = self$params$sustain_iso_code,
        region = self$params$sustain_region,
        tracking_mode = "machine",
        log_level = "warning",
        measure_power_secs = self$params$sustain_interval,
        save_to_file = FALSE,
        save_to_api = FALSE,
        allow_multiple_runs = FALSE
      )
    },


    # Save data csv (for sustainability data and tokenizer statistics)
    write_data_csv = function(data, csv_file_name, quote = TRUE, mode = "create") {
      matrix <- t(as.matrix(unlist(data)))
      x_value <- matrix
      if (mode == "create") {
        data_output <- paste0(self$params$model_dir, "/", csv_file_name)
      } else if (mode == "train") {
        data_output <- paste0(self$params$output_dir, "/", csv_file_name)
        data_input <- paste0(self$params$model_dir_path, "/", csv_file_name)

        if (file.exists(data_input)) {
          data_chronic <- as.matrix(read.csv(data_input))
          data_chronic <- rbind(data_chronic, matrix)

          x_value <- data_chronic
        }
      } else {
        stop(paste("Mode", mode, "is invalid. Allowed: create or train"))
      }

      write.csv(
        x = x_value,
        file = data_output,
        row.names = FALSE,
        quote = quote
      )
    },

    # Save Sustainability tracker
    save_sustainability_data = function(mode = "create") {
      sustainability_data <- summarize_tracked_sustainability(private$sustainability_tracker)

      private$write_data_csv(
        data = sustainability_data,
        csv_file_name = "sustainability.csv",
        mode = mode
      )
    },

    # Save Tokenizer Statistics on disk
    save_tokenizer_statistics = function(mode = "create") {
      private$write_data_csv(
        data = self$temp$tokenizer_statistics,
        csv_file_name = "tokenizer_statistics.csv",
        quote = FALSE,
        mode = mode
      )
    },

    # Update Tokenizer Statistics (calculate)
    update_tokenizer_statistics = function(dataset, step = "creation") {
      self$temp$tokenizer_statistics[length(self$temp$tokenizer_statistics) + 1] <- list(
        calc_tokenizer_statistics(
          dataset = dataset,
          step = step
        )
      )
    },

    # Field for saving the path to the folder string temp files
    dir_checkpoint = NULL,

    # Create directory in temp files of saving checkpoints
    create_checkpoint_directory = function() {
      # Create a directory for the package
      tmp_dir <- create_and_get_tmp_dir()

      # Create a folder for the current task
      private$dir_checkpoint <- paste0(
        tmp_dir, "/",
        generate_id(16)
      )
      create_dir(dir = private$dir_checkpoint, trace = FALSE)
    },

    # Clean directory for temp files
    clean_checkpoint_directory = function() {
      unlink(
        x = private$dir_checkpoint,
        recursive = TRUE,
        force = FALSE
      )
    }
  ),
  public = list(
    # == Attributes ====================================================================================================

    #' @field params A list containing transformer's parameters ('static', 'dynamic' and 'dependent' parameters)
    #'
    #'   `list()` containing all the transformer parameters. Can be set with `set_model_param()`.
    #'
    #'   ### **'Static' parameters**
    #'
    #'   Regardless of the transformer, the following parameters are always included:
    #'   * `text_dataset`
    #'   * `sustain_track`
    #'   * `sustain_iso_code`
    #'   * `sustain_region`
    #'   * `sustain_interval`
    #'   * `trace`
    #'   * `pytorch_safetensors`
    #'   * `log_dir`
    #'   * `log_write_interval`
    #'
    #'   ### **'Dynamic' parameters**
    #'
    #'   In the case of **create** it also contains (see `create`-method for details):
    #'   * `model_dir`
    #'   * `vocab_size`
    #'   * `max_position_embeddings`
    #'   * `hidden_size`
    #'   * `hidden_act`
    #'   * `hidden_dropout_prob`
    #'   * `attention_probs_dropout_prob`
    #'   * `intermediate_size`
    #'   * `num_attention_heads`
    #'
    #'   In the case of **train** it also contains (see `train`-method for details):
    #'   * `output_dir`
    #'   * `model_dir_path`
    #'   * `p_mask`
    #'   * `whole_word`
    #'   * `val_size`
    #'   * `n_epoch`
    #'   * `batch_size`
    #'   * `chunk_size`
    #'   * `min_seq_len`
    #'   * `full_sequences_only`
    #'   * `learning_rate`
    #'   * `n_workers`
    #'   * `multi_process`
    #'   * `keras_trace`
    #'   * `pytorch_trace`
    #'
    #'   ### **'Dependent' parameters**
    #'
    #'   Depending on the transformer and the method used class may contain different parameters:
    #'   * `vocab_do_lower_case`
    #'   * `num_hidden_layer`
    #'   * `add_prefix_space`
    #'   * etc.
    #'
    params = list(),

    #' @field temp A list containing temporary transformer's parameters
    #'
    #'   `list()` containing all the temporary local variables that need to be accessed between the step functions. Can
    #'   be set with `set_model_temp()`.
    #'
    #'   For example, it can be a variable `tok_new` that stores the tokenizer from
    #'   `steps_for_creation$create_tokenizer_draft`. To train the tokenizer, access the variable `tok_new` in
    #'   `steps_for_creation$calculate_vocab` through the `temp` list of this class.
    #
    temp = list(),

    # == Methods =======================================================================================================

    # New ----

    #' @description An object of this class cannot be created. Thus, method's call will produce an error.
    #' @return This method returns an error.
    initialize = function() {
      stop("Cannot create .AIFEBaseTransformer objects.")
    },

    #' @description Method to execute while initializing a new transformer.
    #' @param title `string` A new title.
    #' @param init_trace `bool` option to show prints. If `TRUE` (by default) - messages will be shown, otherwise
    #'   (`FALSE`) - hidden.
    #' @return This method returns nothing.s
    init_transformer = function(title, init_trace) {
      self$set_title(title)
      if (init_trace) print(paste(title, "has been initialized."))
    },

    # Setters ----

    #' @description Setter for the title. Sets a new value for the `title` private attribute.
    #' @param title `string` A new title.
    #' @return This method returns nothing.
    set_title = function(title) private$title <- title,

    #' @description Setter for the parameters. Adds a new parameter and its value to the `params` list.
    #' @param param_name `string` Parameter's name.
    #' @param param_value `any` Parameter's value.
    #' @return This method returns nothing.
    set_model_param = function(param_name, param_value) self$params[[param_name]] <- param_value,

    #' @description Setter for the temporary model's parameters. Adds a new temporary parameter and its value to the
    #'    `temp` list.
    #' @param temp_name `string` Parameter's name.
    #' @param temp_value `any` Parameter's value.
    #' @return This method returns nothing.
    set_model_temp = function(temp_name, temp_value) self$temp[[temp_name]] <- temp_value,


    # Setters for SFC ----

    #' @description Setter for the `check_max_pos_emb` element of the private `steps_for_creation` list. Sets a new
    #'   `fun` function as the `check_max_pos_emb` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFC_check_max_pos_emb = function(fun) private$steps_for_creation$check_max_pos_emb <- fun,

    #' @description Setter for the `create_tokenizer_draft` element of the  private `steps_for_creation` list. Sets a
    #'   new `fun` function as the `create_tokenizer_draft` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFC_create_tokenizer_draft = function(fun) private$steps_for_creation$create_tokenizer_draft <- fun,

    #' @description Setter for the `calculate_vocab` element of the private `steps_for_creation` list. Sets a new `fun`
    #'   function as the `calculate_vocab` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFC_calculate_vocab = function(fun) private$steps_for_creation$calculate_vocab <- fun,

    #' @description Setter for the `save_tokenizer_draft` element of the private `steps_for_creation` list. Sets a new
    #'   `fun` function as the `save_tokenizer_draft` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFC_save_tokenizer_draft = function(fun) private$steps_for_creation$save_tokenizer_draft <- fun,

    #' @description Setter for the `create_final_tokenizer` element of the private `steps_for_creation` list. Sets a new
    #'   `fun` function as the `create_final_tokenizer` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFC_create_final_tokenizer = function(fun) private$steps_for_creation$create_final_tokenizer <- fun,

    #' @description Setter for the `create_transformer_model` element of the private `steps_for_creation` list. Sets a
    #'   new `fun` function as the `create_transformer_model` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFC_create_transformer_model = function(fun) private$steps_for_creation$create_transformer_model <- fun,


    #' @description Setter for all required elements of the private `steps_for_creation` list. Executes setters for all
    #'   required creation steps.
    #' @param required_SFC `list()` A list of all new required steps.
    #' @return This method returns nothing.
    set_required_SFC = function(required_SFC) { # nolint
      self$set_SFC_create_tokenizer_draft(required_SFC$create_tokenizer_draft)
      self$set_SFC_calculate_vocab(required_SFC$calculate_vocab)
      self$set_SFC_save_tokenizer_draft(required_SFC$save_tokenizer_draft)
      self$set_SFC_create_final_tokenizer(required_SFC$create_final_tokenizer)
      self$set_SFC_create_transformer_model(required_SFC$create_transformer_model)
    },


    # Setters for SFT ----

    #' @description Setter for the `load_existing_model` element of the private `steps_for_training` list. Sets a new
    #'   `fun` function as the `load_existing_model` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFT_load_existing_model = function(fun) private$steps_for_training$load_existing_model <- fun,

    #' @description Setter for the `cuda_empty_cache` element of the private `steps_for_training` list. Sets a new
    #'   `fun` function as the `cuda_empty_cache` step.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFT_cuda_empty_cache = function(fun) private$steps_for_training$cuda_empty_cache <- fun,

    #' @description Setter for the `create_data_collator` element of the private `steps_for_training` list. Sets a new
    #'   `fun` function as the `create_data_collator` step. Use this method to make a custom data collator for a
    #'   transformer.
    #' @param fun `function()` A new function.
    #' @return This method returns nothing.
    set_SFT_create_data_collator = function(fun) private$steps_for_training$create_data_collator <- fun,


    # Main methods ----

    # Create ----

    #' @description This method creates a transformer configuration based on the child-transformer architecture and a
    #'   vocabulary using the python libraries `transformers` and `tokenizers`.
    #'
    #'   This method **adds** the following parameters to the `temp` list:
    #'   * `log_file`
    #'   * `raw_text_dataset`
    #'   * `pt_safe_save`
    #'   * `value_top`
    #'   * `total_top`
    #'   * `message_top`
    #'
    #'   This method **uses** the following parameters from the `temp` list:
    #'   * `log_file`
    #'   * `raw_text_dataset`
    #'   * `tokenizer`
    #'
    #' @param model_dir `r get_param_doc_desc("model_dir")`
    #' @param vocab_size `r get_param_doc_desc("vocab_size")`
    #' @param max_position_embeddings `r get_param_doc_desc("max_position_embeddings")`
    #' @param hidden_size `r get_param_doc_desc("hidden_size")`
    #' @param num_attention_heads `r get_param_doc_desc("num_attention_heads")`
    #' @param intermediate_size `r get_param_doc_desc("intermediate_size")`
    #' @param hidden_act `r get_param_doc_desc("hidden_act")`
    #' @param hidden_dropout_prob `r get_param_doc_desc("hidden_dropout_prob")`
    #' @param attention_probs_dropout_prob `r get_param_doc_desc("attention_probs_dropout_prob")`
    #'
    #' @return This method does not return an object. Instead, it saves the configuration and vocabulary of the new
    #'   model to disk.
    create = function(model_dir,
                      text_dataset,
                      vocab_size,
                      max_position_embeddings,
                      hidden_size,
                      num_attention_heads,
                      intermediate_size,
                      hidden_act,
                      hidden_dropout_prob,
                      attention_probs_dropout_prob,
                      sustain_track,
                      sustain_iso_code,
                      sustain_region,
                      sustain_interval,
                      trace,
                      pytorch_safetensors,
                      log_dir,
                      log_write_interval) {
      tryCatch({
        private$define_required_SFC_functions()
        # Init model parameters -----------------------------------------------------
        # Each transformer has these parameters ----
        private$init_common_model_params(
          sustain_track, sustain_iso_code, sustain_region, sustain_interval,
          trace, pytorch_safetensors,
          log_dir, log_write_interval,
          text_dataset
        )
        # Each transformer has these parameters in the case of creation ----
        self$set_model_param("model_dir", model_dir)
        self$set_model_param("vocab_size", vocab_size)
        self$set_model_param("max_position_embeddings", max_position_embeddings)
        self$set_model_param("hidden_size", hidden_size)
        self$set_model_param("hidden_act", hidden_act)
        self$set_model_param("hidden_dropout_prob", hidden_dropout_prob)
        self$set_model_param("attention_probs_dropout_prob", attention_probs_dropout_prob)
        self$set_model_param("intermediate_size", intermediate_size)
        self$set_model_param("num_attention_heads", num_attention_heads)

        # Check definition of required functions ----
        private$check_required_SFC()

        # Logging file
        run_py_file("py_log.py")

        self$temp$log_file <- NULL
        if (!is.null(self$params$log_dir) && dir.exists(self$params$log_dir)) {
          self$temp$log_file <- paste0(self$params$log_dir, "/aifeducation_state.log")
        }

        total <- 10
        write_interval <- self$params$log_write_interval
        last_log <- NULL

        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 0, total_top = total, message_top = paste(private$title, "Overall: Checking"),
          last_log = last_log, write_interval = write_interval
        )

        # argument checking ---------------------------------------------------------
        # optional function
        if (is.function(private$steps_for_creation$check_max_pos_emb)) {
          private$steps_for_creation$check_max_pos_emb(self)
        }
        check_class(object = text_dataset, object_name = "text_dataset", classes = "LargeDataSetForText", allow_NULL = FALSE)
        self$temp$raw_text_dataset <- text_dataset$get_dataset()
        if (is.null(self$temp$raw_text_dataset$features$text)) {
          stop("Dataset does not contain a column 'text' storing the raw texts.")
        }

        check.hidden_act(hidden_act)
        check.sustain_iso_code(sustain_iso_code, sustain_track)

        # Check possible save formats
        self$temp$pt_safe_save <- check.possible_save_formats(pytorch_safetensors)

        # Start Sustainability Tracking ---------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 1, total_top = total, message_top = paste(private$title, "Overall: Starting"),
          last_log = last_log, write_interval = write_interval
        )

        track_msg <- ifelse(
          sustain_track,
          "Start Sustainability Tracking",
          "Start without Sustainability Tracking"
        )
        print_message(track_msg, trace)

        if (sustain_track) {
          private$create_sustain_tracker()
          private$sustainability_tracker$start()
        }

        # Creating a new Tokenizer for Computing Vocabulary -------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 2, total_top = total, message_top = paste(private$title, "Overall: Tokenizer Draft"),
          last_log = last_log, write_interval = write_interval
        )

        print_message("Creating Tokenizer Draft", trace)
        private$steps_for_creation$create_tokenizer_draft(self)

        # Calculating Vocabulary ----------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 3, total_top = total, message_top = paste(private$title, "Overall: Computing Vocabulary"),
          last_log = last_log, write_interval = write_interval
        )

        self$temp$value_top <- 3
        self$temp$total_top <- total
        self$temp$message_top <- paste(private$title, "Overall: Computing Vocabulary")

        print_message("Start Computing Vocabulary", trace)
        private$steps_for_creation$calculate_vocab(self)
        print_message("Start Computing Vocabulary - Done", trace)

        # Saving Tokenizer Draft ----------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 4, total_top = total, message_top = paste(private$title, "Overall: Saving Tokenizer Draft"),
          last_log = last_log, write_interval = write_interval
        )

        print_message("Saving Draft", trace)
        create_dir(model_dir, trace, "Creating Model Directory")
        private$steps_for_creation$save_tokenizer_draft(self)

        # Final Tokenizer -----------------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 5, total_top = total,
          message_top = paste(private$title, "Overall: Creating Final Tokenizer & Calculating Statistics"),
          last_log = last_log, write_interval = write_interval
        )

        print_message("Creating Tokenizer", trace)
        private$steps_for_creation$create_final_tokenizer(self)
        if (!("tokenizer" %in% names(self$temp))) {
          stop("The final tokenizer must be stored in the 'tokenizer' parameter of the 'temp' list.")
        }
        print_message("Creating Tokenizer - Done", trace)

        # Calculate tokenizer statistics -------------------------------------------


        tokenized_texts_raw <- tokenize_dataset(
          dataset = self$temp$raw_text_dataset,
          tokenizer = self$temp$tokenizer,
          max_length = 2048,
          log_file = self$temp$log_file,
          write_interval = write_interval,
          value_top = 5, total_top = total,
          message_top = paste(private$title, "Overall: Creating Final Tokenizer & Calculating Statistics")
        )


        private$update_tokenizer_statistics(tokenized_texts_raw, "creation")

        # Creating Transformer Model ------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 6, total_top = total, message_top = paste(private$title, "Overall: Creating Transformer Model"),
          last_log = last_log, write_interval = write_interval
        )

        print_message("Creating Transformer Model", trace)
        private$steps_for_creation$create_transformer_model(self)
        if (!("model" %in% names(self$temp))) {
          stop("The transformer model must be stored in the 'model' parameter of the 'temp' list.")
        }

        # Saving Model --------------------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 7, total_top = total, message_top = paste(private$title, "Overall: Saving Model"),
          last_log = last_log, write_interval = write_interval
        )

        print_message(paste("Saving", private$title), trace)
        private$steps_for_creation$save_transformer_model(self)
        private$save_tokenizer_statistics()

        # Saving Tokenizer ----------------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 8, total_top = total, message_top = paste(private$title, "Overall: Saving Tokenizer"),
          last_log = last_log, write_interval = write_interval
        )
        print_message("Saving Tokenizer Model", trace)
        self$temp$tokenizer$save_pretrained(model_dir)

        # Stop Sustainability Tracking if requested ----------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 9, total_top = total, message_top = paste(private$title, "Overall: Stopping"),
          last_log = last_log, write_interval = write_interval
        )

        if (sustain_track) {
          private$sustainability_tracker$stop()
          print_message("Saving Sustainability Data", trace)
          private$save_sustainability_data()
        }

        # Finish --------------------------------------------------------------------
        py$write_log_py(
          self$temp$log_file,
          value_top = 10, total_top = total, message_top = paste(private$title, "Overall: Done"),
          last_log = NULL, write_interval = write_interval
        )

        print_message("Done", trace)
        # Clear variables -----------------------------------------------------------
        private$clear_variables()
      }, finally = {
        if (!is.null(private$sustainability_tracker)) {
          private$sustainability_tracker$stop()
        }
      })
    },


    # Train ----

    #' @description This method can be used to train or fine-tune a transformer based on `BERT` architecture with the
    #'   help of the python libraries `transformers`, `datasets`, and `tokenizers`.
    #'
    #'   This method **adds** the following parameters to the `temp` list:
    #'   * `log_file`
    #'   * `loss_file`
    #'   * `from_pt`
    #'   * `from_tf`
    #'   * `load_safe`
    #'   * `raw_text_dataset`
    #'   * `pt_safe_save`
    #'   * `value_top`
    #'   * `total_top`
    #'   * `message_top`
    #'
    #'   This method **uses** the following parameters from the `temp` list:
    #'   * `log_file`
    #'   * `raw_text_dataset`
    #'   * `tokenized_dataset`
    #'   * `tokenizer`
    #'
    #' @param output_dir `r get_param_doc_desc("output_dir")`
    #' @param model_dir_path `r get_param_doc_desc("model_dir_path")`
    #' @param p_mask `r get_param_doc_desc("p_mask")`
    #' @param whole_word `r get_param_doc_desc("whole_word")`
    #' @param val_size `r get_param_doc_desc("val_size")`
    #' @param n_epoch `r get_param_doc_desc("n_epoch")`
    #' @param batch_size `r get_param_doc_desc("batch_size")`
    #' @param chunk_size `r get_param_doc_desc("chunk_size")`
    #' @param full_sequences_only `r get_param_doc_desc("full_sequences_only")`
    #' @param min_seq_len `r get_param_doc_desc("min_seq_len")`
    #' @param learning_rate `r get_param_doc_desc("learning_rate")`
    #' @param n_workers `r get_param_doc_desc("n_workers")`
    #' @param multi_process `r get_param_doc_desc("multi_process")`
    #' @param keras_trace `r get_param_doc_desc("keras_trace")`
    #' @param pytorch_trace ``r get_param_doc_desc("pytorch_trace")`
    #'
    #' @return This method does not return an object. Instead, it saves the configuration and vocabulary of the new
    #'   model to disk.
    #'
    #' @importFrom utils write.csv
    #' @importFrom utils read.csv
    train = function(output_dir,
                     model_dir_path,
                     text_dataset,
                     p_mask,
                     whole_word,
                     val_size,
                     n_epoch,
                     batch_size,
                     chunk_size,
                     full_sequences_only,
                     min_seq_len,
                     learning_rate,
                     n_workers,
                     multi_process,
                     sustain_track,
                     sustain_iso_code,
                     sustain_region,
                     sustain_interval,
                     trace,
                     keras_trace,
                     pytorch_trace,
                     pytorch_safetensors,
                     log_dir,
                     log_write_interval) {
      tryCatch({
        private$define_required_SFT_functions()
        # Init model parameters -----------------------------------------------------
        # Each transformer has these parameters ----
        private$init_common_model_params(
          sustain_track, sustain_iso_code, sustain_region, sustain_interval,
          trace, pytorch_safetensors,
          log_dir, log_write_interval,
          text_dataset
        )
        # Each transformer has these parameters in the case of training ----
        self$set_model_param("output_dir", output_dir)
        self$set_model_param("model_dir_path", model_dir_path)
        self$set_model_param("p_mask", p_mask)
        self$set_model_param("whole_word", whole_word)
        self$set_model_param("val_size", val_size)
        self$set_model_param("n_epoch", n_epoch)
        self$set_model_param("batch_size", batch_size)
        self$set_model_param("chunk_size", chunk_size)
        self$set_model_param("full_sequences_only", full_sequences_only)
        self$set_model_param("min_seq_len", min_seq_len)
        self$set_model_param("learning_rate", learning_rate)
        self$set_model_param("n_workers", n_workers)
        self$set_model_param("multi_process", multi_process)
        self$set_model_param("keras_trace", keras_trace)
        self$set_model_param("pytorch_trace", pytorch_trace)

        # Check defining of required functions ----
        private$check_required_SFT()

        # Logging file
        run_py_file("py_log.py")

        self$temp$log_file <- NULL
        self$temp$loss_file <- NULL
        if (!is.null(self$params$log_dir) && dir.exists(self$params$log_dir)) {
          self$temp$log_file <- paste0(self$params$log_dir, "/aifeducation_state.log")
          self$temp$loss_file <- paste0(self$params$log_dir, "/aifeducation_loss.log")
        } else {
          self$temp$loss_file <- paste0(self$params$output_dir, "/aifeducation_loss.log")
        }

        total <- 10
        write_interval <- self$params$log_write_interval
        last_log <- NULL

        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 0, total_top = total, message_top = paste(private$title, "Overall: Checking"),
          last_log = last_log, write_interval = write_interval
        )

        # argument checking ---------------------------------------------------------

        model_files_check <- check.model_files(model_dir_path)
        self$temp$from_pt <- model_files_check$from_pt
        self$temp$from_tf <- model_files_check$from_tf
        self$temp$load_safe <- model_files_check$load_safe

        check_class(object = text_dataset, object_name = "text_dataset", classes = "LargeDataSetForText", allow_NULL = FALSE)
        self$temp$raw_text_dataset <- text_dataset$get_dataset()
        if (is.null(self$temp$raw_text_dataset$features$text)) {
          stop("Dataset does not contain a column 'text' storing the texts for training.")
        }

        check.sustain_iso_code(sustain_iso_code, sustain_track)

        # Check possible save formats
        self$temp$pt_safe_save <- check.possible_save_formats(pytorch_safetensors)

        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 1, total_top = total, message_top = paste(private$title, "Overall: Starting"),
          last_log = last_log, write_interval = write_interval
        )

        # Start Sustainability Tracking ---------------------------------------------
        track_msg <- ifelse(
          sustain_track,
          "Start Sustainability Tracking",
          "Start without Sustainability Tracking"
        )
        print_message(track_msg, trace)

        if (sustain_track) {
          private$create_sustain_tracker()
          private$sustainability_tracker$start()
        }

        # Loading existing model ----------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 2, total_top = total, message_top = paste(private$title, "Overall: Loading Existing Model"),
          last_log = last_log, write_interval = write_interval
        )

        print_message("Loading Existing Model", trace)
        private$steps_for_training$load_existing_model(self)
        if (!("tokenizer" %in% names(self$temp))) {
          stop("The tokenizer must be stored in the 'tokenizer' parameter of the 'temp' list.")
        }
        if (!("model" %in% names(self$temp))) {
          stop("The transformer model must be stored in the 'model' parameter of the 'temp' list.")
        }

        # argument checking------------------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 3, total_top = total, message_top = paste(private$title, "Overall: Checking"),
          last_log = last_log, write_interval = write_interval
        )

        private$steps_for_training$check_chunk_size(self)

        # creating chunks of sequences ----------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 4, total_top = total,
          message_top = paste(
            private$title, "Overall: Creating Chunks of Sequences & Calculating Tokenizer Statistics"
          ),
          last_log = last_log, write_interval = write_interval
        )

        self$temp$write_interval <- write_interval
        self$temp$value_top <- 4
        self$temp$total_top <- total
        self$temp$message_top <- paste(
          private$title, "Overall: Creating Chunks of Sequences & Calculating Tokenizer Statistics"
        )

        print_message("Creating Chunks of Sequences for Training", trace)
        private$steps_for_training$create_chunks_for_training(self)

        n_chunks <- self$temp$tokenized_dataset$num_rows
        print_message(paste(n_chunks, "Chunks Created"), trace)

        # Seeting up DataCollator and Dataset ----------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 5, total_top = total,
          message_top = paste(private$title, "Overall: Seeting up DataCollator and Dataset"),
          last_log = last_log, write_interval = write_interval
        )

        create_dir(output_dir, trace, "Creating Output Directory")
        # create_dir(paste0(output_dir, "/checkpoints"), trace, "Creating Checkpoint Directory")

        private$steps_for_training$prepare_train_tune(self)

        # Start Training -------------------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 6, total_top = total, message_top = paste(private$title, "Overall: Start Training"),
          last_log = last_log, write_interval = write_interval
        )

        print_message("Start Fine Tuning", trace)
        private$steps_for_training$start_training(self)

        # Saving Model --------------------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 7, total_top = total, message_top = paste(private$title, "Overall: Saving Model"),
          last_log = last_log, write_interval = write_interval
        )

        print_message(paste("Saving", private$title), trace)
        private$steps_for_training$save_model(self)
        private$save_tokenizer_statistics("train")

        # Saving Tokenizer -----------------------------------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 8, total_top = total, message_top = paste(private$title, "Overall: Saving Tokenizer"),
          last_log = last_log, write_interval = write_interval
        )

        print_message("Saving Tokenizer", trace)
        self$temp$tokenizer$save_pretrained(output_dir)

        # Stop Sustainability Tracking if requested ----------------------------------
        last_log <- py$write_log_py(
          self$temp$log_file,
          value_top = 9, total_top = total, message_top = paste(private$title, "Overall: Stopping"),
          last_log = last_log, write_interval = write_interval
        )

        if (sustain_track) {
          private$sustainability_tracker$stop()
          print_message("Saving Sustainability Data", trace)
          private$save_sustainability_data("train")
        }

        # Finish --------------------------------------------------------------------
        py$write_log_py(
          self$temp$log_file,
          value_top = 10, total_top = total, message_top = paste(private$title, "Overall: Done"),
          last_log = NULL, write_interval = write_interval
        )

        print_message("Done", trace)
        # Clear variables -----------------------------------------------------------
        private$clear_variables()
      }, finally = {
        if (!is.null(private$sustainability_tracker)) {
          private$sustainability_tracker$stop()
        }
      })
    }
  )
)
