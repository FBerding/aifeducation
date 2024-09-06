#' @title Base `R6` class for creation and definition of `.AIFE*Transformer-like` classes
#'
#' @description This base class is used to create and define `.AIFE*Transformer-like` classes. It serves as a skeleton
#'   for a future concrete transformer and cannot be used to create an object of itself (an attempt to call `new`-method
#'   will produce an error).
#'
#' @section Private attributes: Developers should know the purpose of all the private attributes.
#'
#'   ## **Attribute `title`**
#'
#'   `string` The title for a transformer. This title is displayed in the progress bar. Can be set with `set_title()`.
#'
#'   ## **Attribute `steps_for_creation`**
#'
#'   `list()` that stores required and optional steps (functions) for creating a new transformer.
#'
#'   To access (input) parameters of the transformer, use the `params` list (e.g. `params$ml_framework`). To access a
#'   local variable outside of a function, put it in the `temp` list.
#'
#'   ### Required
#'
#'   The **required steps** to be defined in each child transformer are:
#'   * `create_tokenizer_draft`: `function()` that creates a tokenizer draft. In this function a new tokenizer must
#'   be created and stored as an element of a list `temp` (e.g. `temp$tok_new`). This function can include the
#'   definition of special tokens and/or trainers (`tokenizers.trainers.WordPieceTrainer`). See the
#'   `create_WordPiece_tokenizer()` and `create_ByteLevelBPE_tokenizer()` functions to create a new tokenizer object
#'   (`tokenizers.Tokenizer`) based on the `tokenizers.models.WordPiece` and `tokenizers.models.ByteLevel` models
#'   respectively.
#'
#'   * `calculate_vocab`: `function()` for calculating a vocabulary. The tokenizer created in the
#'   `create_tokenizer_draft()` function is trained. See `tokenizers.Tokenizer.train_from_iterator()` for details.
#'
#'   * `save_tokenizer_draft`: `function()` that saves a tokenizer draft to a model directory (e.g. to a `vocab.txt` file).
#'   See `tokenizers.Tokenizer.save_model()` for details.
#'
#'   * `create_final_tokenizer`: `function()` that creates a new transformer tokenizer object. The tokenizer must be stored
#'   in the `tokenizer` parameter of the `temp` list. See `transformers.PreTrainedTokenizerFast`,
#'   `transformers.LongformerTokenizerFast` and `transformers.RobertaTokenizerFast` for details.
#'
#'   * `create_transformer_model`: `function()` that creates a transformer model. The model must be passed to the `model`
#'   parameter of the `temp` list. See `transformers.(TF)BertModel`, `transformers.(TF)DebertaV2ForMaskedLM`,
#'   `transformers.(TF)FunnelModel`, `transformers.(TF)LongformerModel`, `transformers.(TF)RobertaModel`, etc. for
#'   details.
#'
#'   ### Optional
#'
#'   **Optional step** is:
#'   * `check_max_pos_emb`: `function()` that checks transformer parameter `max_position_embeddings`.
#'   Leave `NULL` to skip the check.
#'
#'   ### Other
#'
#'   **Required and already defined step** is:
#'   * `save_transformer_model`: `function()` that saves a newly created transformer. Uses the temporary `model`
#'   and `pt_safe_save` parameters of the `temp` list. See `transformers.(TF)PreTrainedModel.save_pretrained()` for
#'   details.
#'
#'   Use the `set_SFC_*()` methods to set required/optional steps for creation, where * is the name of the step.
#'
#'   Use the `set_required_SFC()` method to set all required steps at once.
#'
#'   ## **Attribute `steps_for_training`**
#'
#'   `list()` that stores required and optional steps (functions) for training the transformer.
#'
#'   To access (input) parameters of the transformer, use the `params` list (e.g. `params$ml_framework`). To access a
#'   local variable outside of a function, put it in the `temp` list.
#'
#'   ### Required
#'
#'   The **required step** in each child transformer is:
#'   * `load_existing_model`: `function()` that loads the model and its tokenizer. The model and the transformer must be
#'   stored to the `model` and `tokenizer` parameters respectively of the `temp` list. See
#'   `transformers.(TF)PreTrainedModel` for details.
#'
#'   ### Optional
#'
#'   **Optional step** is:
#'   * `cuda_empty_cache`: `function()` to empty the cache if `torch.cuda` is available.
#'
#'   ### Other
#'
#'   **Required and already defined steps** are:
#'   * `check_chunk_size`: `function()` that checks transformer's parameter `chunk_size` and adjusts it. Uses
#'   the `model` parameter of the `temp` list and modifies the `chunk_size` parameter of the `params`
#'   list.
#'
#'   * `create_chunks_for_training`: `function()` that creates chunks of the sequenses for the trainining. Uses the
#'   `tokenizer` parameter and adds `tokenized_dataset` parameter to the `temp` list.
#'
#'   * `prepare_train_tune`: `function()` that prepares the data for the training. For `tensorflow`: uses the `model`
#'   and `tokenizer` parameters, adds the `tf_train_dataset`, `tf_test_dataset`, `callbacks` parameters to the
#'   `temp` list. For `pytorch`: uses the `model`, `tokenizer` parameters, adds the `trainer` parameter to the
#'   `temp` list.
#'
#'   * `start_training`: `function()` that starts the training. For `tensorflow`: uses the `model`, `tf_train_dataset`,
#'   `tf_test_dataset`, `callbacks` parameters of the `temp` list. For `pytorch`: uses the `trainer` parameter
#'   of the `temp` list.
#'
#'   * `save_model`: `function()` that saves the model. For `tensorflow`: uses the `model` parameter of the `temp` list.
#'   For `pytorch`: uses the `model`, `pt_safe_save` and `trainer` parameters of the `temp` list.
#'
#'   Use the `set_SFT_*()` methods to set required/optional steps for creation, where * is the name of the step.
#'
#' @section Create: The `create`-method is a basic algorithm that is used to create a new transformer, but cannot be
#'   called directly. It has some required and optional steps stored in a private `steps_for_creation` list.
#'
#' @section Train: The `train`-method is a basic algorithm that is used to train and tune the transformer but cannot be
#'   called directly. It has some required and optional steps stored in a private `steps_for_training` list.
#'
#' @section Concrete transformer implementation: There are already implemented concrete (child) transformers (e.g.
#'   `BERT`, `DeBERTa-V2`, etc.).
#'
#'   To implement a new one, do the following steps:
#'
#'   1 Create a new `R`-file with a name like `dotAIFECustomTransformer`.
#'
#'   2 Open the file and write the creation of a new `R6::R6Class()` inside of it (see the code below). The name of the
#'   class must be defined here **(1)**. Remember to inherit the base transformer class **(2)**. Use the `private` list
#'   for the private attributes **(3)** like `title`, `steps_for_creation`, etc. (which will be explained later) and the
#'   `public` list **(4)** for the `initialize`, `create`, `train` methods.
#'
#'   ```r
#'   .AIFECustomTransformer <- R6::R6Class(
#'      classname = ".AIFECustomTransformer", # (1)
#'      inherit = .AIFEBaseTransformer,       # (2)
#'      private = list(),                     # (3)
#'      public = list()                       # (4)
#'   )
#'   ```
#'
#'   3 Define the private `title` attribute **(1)** and set it in the `initialize` method **(2)** using the inherited
#'   `super$set_title()` base method **(3)** in the base class.
#'
#'   ```r
#'   .AIFECustomTransformer <- R6::R6Class(
#'      classname = ".AIFECustomTransformer",
#'      inherit = .AIFEBaseTransformer,
#'      private = list(
#'        title = "Custom Model"           # (1)
#'      ),
#'      public = list(
#'        initialize = function() {        # (2)
#'          super$set_title(private$title) # (3)
#'        }
#'      )
#'   )
#'   ```
#'
#'   4 Define the private `steps_for_creation` list **(1)** to implement the required steps (functions) **(2)**-**(6)**,
#'   and **(7)** if needed. Do not forget to pass `self` as input parameter of the functions.
#'
#'   **Note** that local variables created inside of functions can be used through the inherited `temp` list. Put the
#'   local `tok_new` variable **(8)** in the `temp` list in the `create_tokenizer_draft` step **(2)** and use it in the
#'   `calculate_vocab` step **(3)** like **(9)**.
#'
#'   Similarly, use the input parameters of the transformer such as `ml_framework` using the inherited `params` list like
#'   **(10)**.
#'
#'   **Important!**
#'
#'   In the `create_final_tokenizer` step **(5)** store the tokenizer in the `self$temp$tokenizer` variable **(11)**.
#'
#'   In the `create_transformer_model` step **(6)** store the transformer model in the `self$temp$model` variable
#'   **(12)**.
#'
#'   ```r
#'   .AIFECustomTransformer <- R6::R6Class(
#'      classname = ".AIFECustomTransformer",
#'      inherit = .AIFEBaseTransformer,
#'      private = list(
#'        title = "Custom Model",
#'        steps_for_creation = list(                   # (1)
#'          # required
#'          create_tokenizer_draft = function(self) {      # (2)
#'            # The implementation must be here
#'            # self$temp$tok_new <- ...         # (8)
#'          },
#'          calculate_vocab = function(self) {             # (3)
#'            # The implementation must be here
#'            # ... self$temp$tok_new ...        # (9)
#'          },
#'          save_tokenizer_draft = function(self) {        # (4)
#'            # The implementation must be here
#'          },
#'          create_final_tokenizer = function(self) {      # (5)
#'            # The implementation must be here
#'            # self$temp$tokenizer <- ... # (!!!) (11)
#'          },
#'          create_transformer_model = function(self) {    # (6)
#'            # The implementation must be here
#'            # ... self$params$ml_framework ... # (10)
#'            # self$temp$model <- ...     # (!!!) (12)
#'          },
#'          # optional: omit this element if do not needed
#'          check_max_pos_emb = function(self) {           # (7)
#'            # The implementation must be here
#'          }
#'        )
#'      ),
#'      public = list(
#'        initialize = function() {
#'          super$set_title(private$title)
#'        }
#'      )
#'   )
#'   ```
#'
#'   5 Define the `create` method **(1)** with all the input parameters **(2)** of the `create` method of the base
#'   class. Add all the dependent parameters of the custom transformer to the input parameters **(3)**. Dependent
#'   parameters are parameters that depend on the transformer and are not present in the base class. Set these dependent
#'   parameters to the base class using the `super$set_model_param()` method **(4)**. Set required and optional steps to
#'   the base class using the `super$set_required_SFC()` and `super$set_SFC_check_max_pos_emb()` methods respectively
#'   **(5)**. Finally run the basic `create` algorithm using `super$create()` **(6)** with all the input parameters
#'   **(2)**.
#'
#'   ```r
#'   .AIFECustomTransformer <- R6::R6Class(
#'      classname = ".AIFECustomTransformer",
#'      inherit = .AIFEBaseTransformer,
#'      private = list(
#'        title = "Custom Model",
#'        steps_for_creation = list(
#'          # required
#'          create_tokenizer_draft = function(self) { },
#'          calculate_vocab = function(self) { },
#'          save_tokenizer_draft = function(self) { },
#'          create_final_tokenizer = function(self) { },
#'          create_transformer_model = function(self) { },
#'          # optional: omit this element if do not needed
#'          check_max_pos_emb = function(self) { }
#'        )
#'      ),
#'      public = list(
#'        initialize = function() { },
#'        # (1)
#'        create = function(# (2) --------------------------
#'                          ml_framework,
#'                          model_dir,
#'                          vocab_raw_texts,
#'                          vocab_size,
#'                          # ...
#'                          trace,
#'                          pytorch_safetensors,
#'
#'                          # (3) --------------------------
#'                          dep_param1,
#'                          dep_param2,
#'                          # ...
#'                          dep_paramN) {
#'          # (4) -----------------------------------------
#'          super$set_model_param("dep_param1", dep_param1)
#'          super$set_model_param("dep_param2", dep_param2)
#'          # ...
#'          super$set_model_param("dep_paramN", dep_paramN)
#'
#'          # (5) -----------------------------------------
#'          super$set_required_SFC(private$steps_for_creation)
#'
#'          # optional, can be omitted if do not needed
#'          super$set_SFC_check_max_pos_emb(private$steps_for_creation$check_max_pos_emb)
#'
#'          # (6) -----------------------------------------
#'          super$create(
#'            ml_framework = ml_framework,
#'            model_dir = model_dir,
#'            vocab_raw_texts = vocab_raw_texts,
#'            vocab_size = vocab_size,
#'            # ...
#'            trace = trace,
#'            pytorch_safetensors = pytorch_safetensors)
#'        }
#'      )
#'   )
#'   ```
#'
#'   6 Define `train` method **(1)** similarly to the step 5. Implement steps (functions) in the private
#'   `steps_for_training` list **(2)**. Do not forget to pass `self` as input parameter of the required function. Set the dependent parameters **(4)** in the base class using
#'   `super$set_model_param()` method **(5)**. Set the implemented steps for training in the base class using
#'   `super$set_SFT_*()` methods **(6)**. Finally run the basic `train` algorithm **(7)** of the base class with all the
#'   (input) parameters **(3)**.
#'
#'   ```r
#'   .AIFECustomTransformer <- R6::R6Class(
#'      classname = ".AIFECustomTransformer",
#'      inherit = .AIFEBaseTransformer,
#'      private = list(
#'        title = "Custom Model",
#'        steps_for_creation = list(
#'          # required
#'          create_tokenizer_draft = function(self) { },
#'          calculate_vocab = function(self) { },
#'          save_tokenizer_draft = function(self) { },
#'          create_final_tokenizer = function(self) { },
#'          create_transformer_model = function(self) { },
#'          # optional: omit this element if do not needed
#'          check_max_pos_emb = function(self) { }
#'        ),
#'        # (2)
#'        steps_for_training = list(
#'          # required
#'          load_existing_model = function(self) { },
#'          # optional
#'          cuda_empty_cache = function() { }
#'        )
#'      ),
#'      public = list(
#'        initialize = function() { },
#'        create = function( ) {
#'          # ---------------------------
#'          # super$set_model_param(...)
#'          # ...
#'          # ---------------------------
#'          # super$set_required_SFC(...)
#'          # super$set_SFC_*(...)
#'          # ...
#'          # ---------------------------
#'          # super$create(...)
#'        },
#'
#'        # (1)
#'        train = function(# (3) --------
#'                         ml_framework,
#'                         # ...
#'
#'                         # (4) --------
#'                         dep_param1,
#'                         # ...
#'                         dep_paramN) {
#'          # (5) -----------------------------------------
#'          super$set_model_param("dep_param1", dep_param1)
#'          # ...
#'          super$set_model_param("dep_paramN", dep_paramN)
#'
#'          # (6) -----------------------------------------
#'          super$set_SFT_load_existing_model(private$steps_for_creation$load_existing_model)
#'          # optional
#'          super$set_SFT_cuda_empty_cache(private$steps_for_creation$cuda_empty_cache)
#'
#'          # (7) -----------------------------------------
#'          super$train(
#'            ml_framework = ml_framework,
#'            # ...
#'          )
#'        }
#'      )
#'   )
#'   ```
#'
#' @param ml_framework `r paramDesc.ml_framework()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_region `r paramDesc.sustain_region()`
#' @param sustain_interval `r paramDesc.sustain_interval()`
#' @param trace `r paramDesc.trace()`
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
#'
#' @references Hugging Face transformers documantation:
#'   * [BERT](https://huggingface.co/docs/transformers/model_doc/bert)
#'   * [DeBERTa](https://huggingface.co/docs/transformers/model_doc/deberta-v2)
#'   * [Funnel](https://huggingface.co/docs/transformers/model_doc/funnel)
#'   * [Longformer](https://huggingface.co/docs/transformers/model_doc/longformer)
#'   * [RoBERTa](https://huggingface.co/docs/transformers/model_doc/roberta)
#'
#' @family Transformers for developers
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
      # required, already defined steps. Can be overwritten for custom version
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
    init_common_model_params = function(ml_framework,
                                        sustain_track,
                                        sustain_iso_code,
                                        sustain_region,
                                        sustain_interval,
                                        trace,
                                        pytorch_safetensors) {
      self$set_model_param("ml_framework", ml_framework)
      self$set_model_param("sustain_track", sustain_track)
      self$set_model_param("sustain_iso_code", sustain_iso_code)
      self$set_model_param("sustain_region", sustain_region)
      self$set_model_param("sustain_interval", sustain_interval)
      self$set_model_param("trace", trace)
      self$set_model_param("pytorch_safetensors", pytorch_safetensors)
    },


    # -------------------------------------------------------------------------------
    define_required_SFC_functions = function() {
      if (!is.function(private$steps_for_creation$save_transformer_model)) {
        private$steps_for_creation$save_transformer_model <- function(self) {
          if (self$params$ml_framework == "tensorflow") {
            self$temp$model$build()
            self$temp$model$save_pretrained(save_directory = self$params$model_dir)
          } else {
            self$temp$model$save_pretrained(
              save_directory = self$params$model_dir,
              safe_serilization = self$temp$pt_safe_save
            )
          }
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
          run_py_file("datasets_transformer_prepare_data.py")

          if (class(self$params$raw_texts) %in% c("datasets.arrow_dataset.Dataset") == FALSE) {
            # Create Dataset
            raw_text_dataset <- datasets$Dataset$from_dict(
              reticulate::dict(list(text = self$params$raw_texts))
            )
          } else {
            raw_text_dataset <- self$params$raw_texts
          }
          # Preparing Data
          tokenized_texts_raw <- raw_text_dataset$map(
            py$tokenize_raw_text,
            batched = TRUE,
            batch_size = 2L,
            fn_kwargs = reticulate::dict(
              list(
                tokenizer = self$temp$tokenizer,
                truncation = TRUE,
                padding = FALSE,
                max_length = as.integer(self$params$chunk_size),
                return_overflowing_tokens = TRUE,
                return_length = TRUE,
                return_special_tokens_mask = TRUE,
                return_offsets_mapping = FALSE,
                return_attention_mask = TRUE,
                return_tensors = "np",
                request_word_ids = self$params$whole_word,
                report_to_aifeducation_studio = is_shinyapp_active()
              )
            ),
            remove_columns = raw_text_dataset$column_names
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
          self$temp$tokenized_dataset <- tokenized_texts_raw$select(as.integer(relevant_indices - 1))
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

          self$temp$return_tensors <- ifelse(self$params$ml_framework == "tensorflow", "tf", "pt")

          # Create data collator
          private$steps_for_training$create_data_collator(self)

          # ----
          format_type <- ifelse(self$params$ml_framework == "tensorflow", "tensorflow", "torch")
          self$temp$tokenized_dataset$set_format(type = format_type)
          self$temp$tokenized_dataset <- self$temp$tokenized_dataset$train_test_split(
            test_size = self$params$val_size
          )

          print_message("Preparing Training of the Model", self$params$trace)

          if (self$params$ml_framework == "tensorflow") { # TENSORFLOW --------------
            self$temp$tf_train_dataset <- self$temp$model$prepare_tf_dataset(
              dataset = self$temp$tokenized_dataset$train,
              batch_size = as.integer(self$params$batch_size),
              collate_fn = self$temp$data_collator,
              shuffle = TRUE
            )
            self$temp$tf_test_dataset <- self$temp$model$prepare_tf_dataset(
              dataset = self$temp$tokenized_dataset$test,
              batch_size = as.integer(self$params$batch_size),
              collate_fn = self$temp$data_collator,
              shuffle = TRUE
            )

            adam <- tf$keras$optimizers$Adam

            # Create Callbacks ---------------------------------------------------------
            callback_checkpoint <- tf$keras$callbacks$ModelCheckpoint(
              filepath = paste0(self$params$output_dir, "/checkpoints/best_weights.h5"),
              monitor = "val_loss",
              verbose = as.integer(min(self$params$keras_trace, 1)),
              mode = "auto",
              save_best_only = TRUE,
              save_freq = "epoch",
              save_weights_only = TRUE
            )

            callback_history <- tf$keras$callbacks$CSVLogger(
              filename = paste0(self$params$output_dir, "/checkpoints/history.log"),
              separator = ",",
              append = FALSE
            )

            self$temp$callbacks <- list(callback_checkpoint, callback_history)

            # Add Callback if Shiny App is running -------------------------------------
            if (is_shinyapp_active()) {
              run_py_file("keras_callbacks.py")
              self$temp$callbacks <- list(callback_checkpoint, callback_history, py$ReportAiforeducationShiny())
            }

            print_message("Compile Model", self$params$trace)
            self$temp$model$compile(optimizer = adam(self$params$learning_rate), loss = "auto")

            # Clear session to provide enough resources for computations ---------------
            tf$keras$backend$clear_session()
          } else { # PYTORCH ------------------
            training_args <- transformers$TrainingArguments(
              output_dir = paste0(self$params$output_dir, "/checkpoints"),
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

            self$temp$trainer <- transformers$Trainer(
              model = self$temp$model,
              train_dataset = self$temp$tokenized_dataset$train,
              eval_dataset = self$temp$tokenized_dataset$test,
              args = training_args,
              data_collator = self$temp$data_collator,
              tokenizer = self$temp$tokenizer
            )

            self$temp$trainer$remove_callback(transformers$integrations$CodeCarbonCallback)
            if (!as.logical(self$params$pytorch_trace)) {
              self$temp$trainer$remove_callback(transformers$PrinterCallback)
              self$temp$trainer$remove_callback(transformers$ProgressCallback)
            }

            # Add Callback if Shiny App is running
            if (is_shinyapp_active()) {
              run_py_file("pytorch_transformer_callbacks.py")
              self$temp$trainer$add_callback(py$ReportAiforeducationShiny_PT())
            }
          }
        }
      }


      # SFT: start_training ---------------------------------------------------------
      if (!is.function(private$steps_for_training$start_training)) {
        private$steps_for_training$start_training <- function(self) {
          if (self$params$ml_framework == "tensorflow") { # TENSORFLOW --------------
            self$temp$model$fit(
              x = self$temp$tf_train_dataset,
              validation_data = self$temp$tf_test_dataset,
              epochs = as.integer(self$params$n_epoch),
              workers = as.integer(self$params$n_workers),
              use_multiprocessing = self$params$multi_process,
              callbacks = list(self$temp$callbacks),
              verbose = as.integer(self$params$keras_trace)
            )

            print_message("Load Weights From Best Checkpoint", self$params$trace)
            self$temp$model$load_weights(paste0(self$params$output_dir, "/checkpoints/best_weights.h5"))
          } else { # PYTORCH -----------------
            if (is.function(private$steps_for_training$cuda_empty_cache)) {
              private$steps_for_training$cuda_empty_cache()
            }
            self$temp$trainer$train()
          }
        }
      }


      # SFT: save_model -------------------------------------------------------------
      if (!is.function(private$steps_for_training$save_model)) {
        private$steps_for_training$save_model <- function(self) {
          if (self$params$ml_framework == "tensorflow") { # TENSORFLOW --------------
            self$temp$model$save_pretrained(
              save_directory = self$params$output_dir
            )
            history_log <- read.csv(file = paste0(self$params$output_dir, "/checkpoints/history.log"))
          } else { # PYTORCH -----------------
            self$temp$model$save_pretrained(
              save_directory = self$params$output_dir,
              safe_serilization = self$temp$pt_safe_save
            )
            history_log <- pandas$DataFrame(self$temp$trainer$state$log_history)
            history_log <- clean_pytorch_log_transformers(history_log)
          }

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
      private$sustainability_tracker <- codecarbon$OfflineEmissionsTracker(
        country_iso_code = self$params$sustain_iso_code,
        region = self$params$sustain_region,
        tracking_mode = "machine",
        log_level = "warning",
        measure_power_secs = self$params$sustain_interval,
        save_to_file = FALSE,
        save_to_api = FALSE
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
    #'   * `ml_framework`
    #'   * `sustain_track`
    #'   * `sustain_iso_code`
    #'   * `sustain_region`
    #'   * `sustain_interval`
    #'   * `trace`
    #'   * `pytorch_safetensors`
    #'
    #'   ### **'Dynamic' parameters**
    #'
    #'   In the case of **create** it also contains (see `create`-method for details):
    #'   * `model_dir`
    #'   * `vocab_raw_texts`
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
    #'   * `raw_texts`
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
    #'   ![](transformer_params_list.png)
    #'
    #'   Figure 1: Possible parameters in the params list
    params = list(),

    #' @field temp A list containing temporary transformer's parameters
    #'
    #'   `list()` containing all the temporary local variables that need to be accessed between the step functions. Can be
    #'   set with `set_model_temp()`.
    #'
    #'   For example, it can be a variable `tok_new` that stores the tokenizer from
    #'   `steps_for_creation$create_tokenizer_draft`. To train the tokenizer, access the variable `tok_new` in
    #'   `steps_for_creation$calculate_vocab` through the `temp` list of this class.
    #'
    #'   ![](transformer_temp_list.png)
    #'
    #'   Figure 2: Possible parameters in the temp list
    temp = list(),

    # == Methods =======================================================================================================

    # New ----

    #' @description An object of this class cannot be created. Thus, method's call will produce an error.
    #' @return This method returns an error.
    initialize = function() {
      stop("Cannot create .AIFEBaseTransformer objects.")
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
    #'   This method **adds** `raw_text_dataset` and `pt_safe_save`, **uses** `tokenizer` temporary parameters of the
    #'   `temp` list.
    #'
    #' @param model_dir `r paramDesc.model_dir()`
    #' @param vocab_raw_texts `r paramDesc.vocab_raw_texts()`
    #' @param vocab_size `r paramDesc.vocab_size()`
    #' @param max_position_embeddings `r paramDesc.max_position_embeddings()`
    #' @param hidden_size `r paramDesc.hidden_size()`
    #' @param num_attention_heads `r paramDesc.num_attention_heads()`
    #' @param intermediate_size `r paramDesc.intermediate_size()`
    #' @param hidden_act `r paramDesc.hidden_act()`
    #' @param hidden_dropout_prob `r paramDesc.hidden_dropout_prob()`
    #' @param attention_probs_dropout_prob `r paramDesc.attention_probs_dropout_prob()`
    #'
    #' @return This method does not return an object. Instead, it saves the configuration and vocabulary of the new
    #'   model to disk.
    create = function(ml_framework,
                      model_dir,
                      vocab_raw_texts,
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
                      pytorch_safetensors) {
      tryCatch({
        private$define_required_SFC_functions()
        # Init model parameters -----------------------------------------------------
        # Each transformer has these parameters ----
        private$init_common_model_params(
          ml_framework,
          sustain_track, sustain_iso_code, sustain_region, sustain_interval,
          trace, pytorch_safetensors
        )
        # Each transformer has these parameters in the case of creation ----
        self$set_model_param("model_dir", model_dir)
        self$set_model_param("vocab_raw_texts", vocab_raw_texts)
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

        # Set Shiny Progress Tracking -----------------------------------------------
        pgr_value <- -1
        pgr_max <- 10
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 0

        # argument checking ---------------------------------------------------------
        # optional function
        if (is.function(private$steps_for_creation$check_max_pos_emb)) {
          private$steps_for_creation$check_max_pos_emb(self)
        }
        self$temp$raw_text_dataset <- check.vocab_raw_texts(vocab_raw_texts)
        check.ml_framework(ml_framework)
        check.hidden_act(hidden_act)
        check.sustain_iso_code(sustain_iso_code, sustain_track)

        # Check possible save formats
        self$temp$pt_safe_save <- check.possible_save_formats(ml_framework, pytorch_safetensors)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 1

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
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 2

        # Creating a new Tokenizer for Computing Vocabulary -------------------------
        print_message("Creating Tokenizer Draft", trace)
        private$steps_for_creation$create_tokenizer_draft(self)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 3

        # Calculating Vocabulary ----------------------------------------------------
        print_message("Start Computing Vocabulary", trace)
        run_py_file("datasets_transformer_compute_vocabulary.py")
        private$steps_for_creation$calculate_vocab(self)
        print_message("Start Computing Vocabulary - Done", trace)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 4

        # Saving Tokenizer Draft ----------------------------------------------------
        print_message("Saving Draft", trace)
        create_dir(model_dir, trace, "Creating Model Directory")
        private$steps_for_creation$save_tokenizer_draft(self)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 5

        # Final Tokenizer -----------------------------------------------------------
        print_message("Creating Tokenizer", trace)
        private$steps_for_creation$create_final_tokenizer(self)
        if (!("tokenizer" %in% names(self$temp))) {
          stop("The final tokenizer must be stored in the 'tokenizer' parameter of the 'temp' list.")
        }
        print_message("Creating Tokenizer - Done", trace)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 6

        # Creating Transformer Model ------------------------------------------------
        print_message("Creating Transformer Model", trace)
        private$steps_for_creation$create_transformer_model(self)
        if (!("model" %in% names(self$temp))) {
          stop("The transformer model must be stored in the 'model' parameter of the 'temp' list.")
        }
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 7
        # Saving Model --------------------------------------------------------------
        print_message(paste("Saving", private$title), trace)
        private$steps_for_creation$save_transformer_model(self)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 8
        # Saving Tokenizer ----------------------------------------------------------
        print_message("Saving Tokenizer Model", trace)
        self$temp$tokenizer$save_pretrained(model_dir)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 9

        # Stop Sustainability Tracking if requested ----------------------------------
        if (sustain_track) {
          private$sustainability_tracker$stop()
          sustainability_data <- summarize_tracked_sustainability(private$sustainability_tracker)
          sustain_matrix <- t(as.matrix(unlist(sustainability_data)))

          print_message("Saving Sustainability Data", trace)

          write.csv(
            x = sustain_matrix,
            file = paste0(model_dir, "/sustainability.csv"),
            row.names = FALSE
          )
        }

        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 10

        # Finish --------------------------------------------------------------------
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
    #'   This method **adds** `from_pt`, `from_tf`, `load_safe` and `pt_safe_save`, **uses** `tokenized_dataset` and
    #'   `tokenizer` temporary parameters of the `temp` list.
    #'
    #' @param output_dir `r paramDesc.output_dir()`
    #' @param model_dir_path `r paramDesc.model_dir_path()`
    #' @param raw_texts `r paramDesc.raw_texts()`
    #' @param p_mask `r paramDesc.p_mask()`
    #' @param whole_word `r paramDesc.whole_word()`
    #' @param val_size `r paramDesc.val_size()`
    #' @param n_epoch `r paramDesc.n_epoch()`
    #' @param batch_size `r paramDesc.batch_size()`
    #' @param chunk_size `r paramDesc.chunk_size()`
    #' @param full_sequences_only `r paramDesc.full_sequences_only()`
    #' @param min_seq_len `r paramDesc.min_seq_len()`
    #' @param learning_rate `r paramDesc.learning_rate()`
    #' @param n_workers `r paramDesc.n_workers()`
    #' @param multi_process `r paramDesc.multi_process()`
    #' @param keras_trace `r paramDesc.keras_trace()`
    #' @param pytorch_trace `r paramDesc.pytorch_trace()`
    #'
    #' @return This method does not return an object. Instead, it saves the configuration and vocabulary of the new
    #'   model to disk.
    #'
    #' @importFrom utils write.csv
    #' @importFrom utils read.csv
    train = function(ml_framework,
                     output_dir,
                     model_dir_path,
                     raw_texts,
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
                     pytorch_safetensors) {
      tryCatch({
        private$define_required_SFT_functions()
        # Init model parameters -----------------------------------------------------
        # Each transformer has these parameters ----
        private$init_common_model_params(
          ml_framework,
          sustain_track, sustain_iso_code, sustain_region, sustain_interval,
          trace, pytorch_safetensors
        )
        # Each transformer has these parameters in the case of training ----
        self$set_model_param("output_dir", output_dir)
        self$set_model_param("model_dir_path", model_dir_path)
        self$set_model_param("raw_texts", raw_texts)
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

        # Set Shiny Progress Tracking -----------------------------------------------
        pgr_value <- -1
        pgr_max <- 10
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 0

        # argument checking ---------------------------------------------------------
        check.ml_framework(ml_framework)

        model_files_check <- check.model_files(ml_framework, model_dir_path)
        self$temp$from_pt <- model_files_check$from_pt
        self$temp$from_tf <- model_files_check$from_tf
        self$temp$load_safe <- model_files_check$load_safe

        check.sustain_iso_code(sustain_iso_code, sustain_track)

        # Check possible save formats
        self$temp$pt_safe_save <- check.possible_save_formats(ml_framework, pytorch_safetensors)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 1

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

        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 2

        # Loading existing model ----------------------------------------------------
        print_message("Loading Existing Model", trace)
        private$steps_for_training$load_existing_model(self)
        if (!("tokenizer" %in% names(self$temp))) {
          stop("The tokenizer must be stored in the 'tokenizer' parameter of the 'temp' list.")
        }
        if (!("model" %in% names(self$temp))) {
          stop("The transformer model must be stored in the 'model' parameter of the 'temp' list.")
        }
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 3

        # argument checking------------------------------------------------------------
        private$steps_for_training$check_chunk_size(self)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 4

        # creating chunks of sequences ----------------------------------------------
        print_message("Creating Chunks of Sequences for Training", trace)
        private$steps_for_training$create_chunks_for_training(self)

        n_chunks <- self$temp$tokenized_dataset$num_rows
        print_message(paste(n_chunks, "Chunks Created"), trace)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 5

        # Seeting up DataCollator and Dataset ----------------------------------------
        create_dir(output_dir, trace, "Creating Output Directory")
        create_dir(paste0(output_dir, "/checkpoints"), trace, "Creating Checkpoint Directory")

        private$steps_for_training$prepare_train_tune(self)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 6

        # Start Training -------------------------------------------------------------
        print_message("Start Fine Tuning", trace)
        private$steps_for_training$start_training(self)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 7

        # Saving Model --------------------------------------------------------------
        print_message(paste("Saving", private$title), trace)
        private$steps_for_training$save_model(self)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 8

        # Saving Tokenizer -----------------------------------------------------------
        print_message("Saving Tokenizer", trace)
        self$temp$tokenizer$save_pretrained(output_dir)
        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 9

        # Stop Sustainability Tracking if requested ----------------------------------
        if (sustain_track) {
          private$sustainability_tracker$stop()
          sustainability_data <- summarize_tracked_sustainability(private$sustainability_tracker)
          sustain_matrix <- t(as.matrix(unlist(sustainability_data)))

          print_message("Saving Sustainability Data", trace)

          sustainability_data_file_path_input <- paste0(model_dir_path, "/sustainability.csv")
          sustainability_data_file_path <- paste0(output_dir, "/sustainability.csv")

          x_value <- sustain_matrix
          if (file.exists(sustainability_data_file_path_input)) {
            sustainability_data_chronic <- as.matrix(read.csv(sustainability_data_file_path_input))
            sustainability_data_chronic <- rbind(
              sustainability_data_chronic,
              sustain_matrix
            )
            x_value <- sustainability_data_chronic
          }

          write.csv(
            x = x_value,
            file = sustainability_data_file_path,
            row.names = FALSE
          )
        }

        pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 10

        # Finish --------------------------------------------------------------------
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
