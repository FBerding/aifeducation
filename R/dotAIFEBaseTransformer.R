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
#'   ## **Attribute `params`**
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
#'
#'   ## **Attribute `temp`**
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
#'   stored to the `model` and `tokenizer` parameters respectively of the private `temp` list. See
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
#'   the `model` parameter of the private `temp` list and modifies the `chunk_size` parameter of the private `params`
#'   list.
#'
#'   * `create_chunks_for_training`: `function()` that creates chunks of the sequenses for the trainining. Uses the
#'   `tokenizer` parameter and adds `tokenized_dataset` parameter to the private `temp` list.
#'
#'   * `prepare_train_tune`: `function()` that prepares the data for the training. For `tensorflow`: uses the `model`
#'   and `tokenizer` parameters, adds the `tf_train_dataset`, `tf_test_dataset`, `callbacks` parameters to the private
#'   `temp` list. For `pytorch`: uses the `model`, `tokenizer` parameters, adds the `trainer` parameter to the private
#'   `temp` list.
#'
#'   * `start_training`: `function()` that starts the training. For `tensorflow`: uses the `model`, `tf_train_dataset`,
#'   `tf_test_dataset`, `callbacks` parameters of the private `temp` list. For `pytorch`: uses the `trainer` parameter
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
#'   and **(7)** if needed.
#'
#'   **Note** that local variables created inside of functions can be used through the private `temp` list. Put the
#'   local `tok_new` variable **(8)** in the `temp` list in the `create_tokenizer_draft` step **(2)** and use it in the
#'   `calculate_vocab` step **(3)** like **(9)**.
#'
#'   Similarly, use the input parameters of the transformer such as `ml_framework` using the private `params` list like
#'   **(10)**.
#'
#'   **Important!**
#'
#'   In the `create_final_tokenizer` step **(5)** store the tokenizer in the `private$temp$tokenizer` variable **(11)**.
#'
#'   In the `create_transformer_model` step **(6)** store the transformer model in the `private$temp$model` variable
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
#'          create_tokenizer_draft = function() {      # (2)
#'            # The implementation must be here
#'            # private$temp$tok_new <- ...         # (8)
#'          },
#'          calculate_vocab = function() {             # (3)
#'            # The implementation must be here
#'            # ... private$temp$tok_new ...        # (9)
#'          },
#'          save_tokenizer_draft = function() {        # (4)
#'            # The implementation must be here
#'          },
#'          create_final_tokenizer = function() {      # (5)
#'            # The implementation must be here
#'            # private$temp$tokenizer <- ... # (!!!) (11)
#'          },
#'          create_transformer_model = function() {    # (6)
#'            # The implementation must be here
#'            # ... private$params$ml_framework ... # (10)
#'            # private$temp$model <- ...     # (!!!) (12)
#'          },
#'          # optional: omit this element if do not needed
#'          check_max_pos_emb = function() {           # (7)
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
#'          create_tokenizer_draft = function() { },
#'          calculate_vocab = function() { },
#'          save_tokenizer_draft = function() { },
#'          create_final_tokenizer = function() { },
#'          create_transformer_model = function() { },
#'          # optional: omit this element if do not needed
#'          check_max_pos_emb = function() { }
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
#'   `steps_for_training` list **(2)**. Set the dependent parameters **(4)** in the base class using
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
#'          create_tokenizer_draft = function() { },
#'          calculate_vocab = function() { },
#'          save_tokenizer_draft = function() { },
#'          create_final_tokenizer = function() { },
#'          create_transformer_model = function() { },
#'          # optional: omit this element if do not needed
#'          check_max_pos_emb = function() { }
#'        ),
#'        # (2)
#'        steps_for_training = list(
#'          # required
#'          load_existing_model = function() { },
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
    # Variables ----

    # Transformer's title
    title = "Transformer Model",

    # This object is used to track sustainability on demand
    # It can be created with the private `create_sustain_tracker()` method.
    sustainability_tracker = NULL,

    # A list containing transformer's parameters ('static', 'dynamic' and 'dependent' parameters)
    params = list(),

    # A list containing temporary transformer's parameters
    temp = list(),

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
      save_transformer_model = function() {
        if (private$params$ml_framework == "tensorflow") {
          private$temp$model$build()
          private$temp$model$save_pretrained(save_directory = private$params$model_dir)
        } else {
          private$temp$model$save_pretrained(
            save_directory = private$params$model_dir,
            safe_serilization = private$temp$pt_safe_save
          )
        }
      }
    ),

    # Required steps to train the transformer
    steps_for_training = list(
      # required
      load_existing_model = NULL,
      # optional
      cuda_empty_cache = NULL,
      # required and already defined steps
      check_chunk_size = function() {
        if (private$params$chunk_size > (private$temp$model$config$max_position_embeddings)) {
          stop(
            paste(
              "Chunk size is", private$params$chunk_size, ". This value is not allowed to exceed",
              private$temp$model$config$max_position_embeddings
            )
          )
        }
        if (private$params$chunk_size < 3) {
          stop("Chunk size must be at least 3.")
        }
        # adjust chunk size. To elements are needed for begin and end of sequence
        private$params$chunk_size <- private$params$chunk_size - 2
      },
      create_chunks_for_training = function() {
        run_py_file("datasets_transformer_prepare_data.py")

        if (class(private$params$raw_texts) %in% c("datasets.arrow_dataset.Dataset") == FALSE) {
          # Create Dataset
          raw_text_dataset <- datasets$Dataset$from_dict(
            reticulate::dict(list(text = raw_texts))
          )
        }

        # Preparing Data
        tokenized_texts_raw <- raw_text_dataset$map(
          py$tokenize_raw_text,
          batched = TRUE,
          batch_size = 2L,
          fn_kwargs = reticulate::dict(
            list(
              tokenizer = private$temp$tokenizer,
              truncation = TRUE,
              padding = FALSE,
              max_length = as.integer(private$params$chunk_size),
              return_overflowing_tokens = TRUE,
              return_length = TRUE,
              return_special_tokens_mask = TRUE,
              return_offsets_mapping = FALSE,
              return_attention_mask = TRUE,
              return_tensors = "np",
              request_word_ids = private$params$whole_word,
              report_to_aifeducation_studio = is_shinyapp_active()
            )
          ),
          remove_columns = raw_text_dataset$column_names
        )

        length_vector <- tokenized_texts_raw["length"]
        relevant_indices <- ifelse(
          private$params$full_sequences_only,
          which(length_vector == private$params$chunk_size),
          which(length_vector <= private$params$chunk_size & length_vector >= private$params$min_seq_len)
        )
        private$temp$tokenized_dataset <- tokenized_texts_raw$select(as.integer(relevant_indices - 1))
      },
      prepare_train_tune = function() {
        if (private$params$ml_framework == "tensorflow") {
          data_collator <- create_data_collator(
            private$params$whole_word, private$temp$tokenizer, private$params$p_mask, "tf"
          )

          tokenized_dataset$set_format(type = "tensorflow")
          tokenized_dataset <- tokenized_dataset$train_test_split(test_size = private$params$val_size)
          private$temp$tf_train_dataset <- private$temp$model$prepare_tf_dataset(
            dataset = tokenized_dataset$train,
            batch_size = as.integer(private$params$batch_size),
            collate_fn = data_collator,
            shuffle = TRUE
          )
          private$temp$tf_test_dataset <- private$temp$model$prepare_tf_dataset(
            dataset = tokenized_dataset$test,
            batch_size = as.integer(private$params$batch_size),
            collate_fn = data_collator,
            shuffle = TRUE
          )

          print_message("Preparing Training of the Model", private$params$trace)
          adam <- tf$keras$optimizers$Adam

          # Create Callbacks ---------------------------------------------------------
          callback_checkpoint <- tf$keras$callbacks$ModelCheckpoint(
            filepath = paste0(private$params$output_dir, "/checkpoints/best_weights.h5"),
            monitor = "val_loss",
            verbose = as.integer(min(private$params$keras_trace, 1)),
            mode = "auto",
            save_best_only = TRUE,
            save_freq = "epoch",
            save_weights_only = TRUE
          )

          callback_history <- tf$keras$callbacks$CSVLogger(
            filename = paste0(private$params$output_dir, "/checkpoints/history.log"),
            separator = ",",
            append = FALSE
          )

          private$temp$callbacks <- list(callback_checkpoint, callback_history)

          # Add Callback if Shiny App is running -------------------------------------
          if (is_shinyapp_active()) {
            run_py_file("keras_callbacks.py")
            private$temp$callbacks <- list(callback_checkpoint, callback_history, py$ReportAiforeducationShiny())
          }

          print_message("Compile Model", private$params$trace)
          private$temp$model$compile(optimizer = adam(private$params$learning_rate), loss = "auto")

          # Clear session to provide enough resources for computations ---------------
          tf$keras$backend$clear_session()
        } else {
          data_collator <- create_data_collator(
            private$params$whole_word, private$temp$tokenizer, private$params$p_mask, "pt"
          )

          tokenized_dataset$set_format(type = "torch")
          tokenized_dataset <- tokenized_dataset$train_test_split(test_size = private$params$val_size)

          print_message("Preparing Training of the Model", private$params$trace)

          training_args <- transformers$TrainingArguments(
            output_dir = paste0(private$params$output_dir, "/checkpoints"),
            overwrite_output_dir = TRUE,
            evaluation_strategy = "epoch",
            num_train_epochs = as.integer(private$params$n_epoch),
            logging_strategy = "epoch",
            save_strategy = "epoch",
            save_total_limit = as.integer(1),
            load_best_model_at_end = TRUE,
            optim = "adamw_torch",
            learning_rate = private$params$learning_rate,
            per_device_train_batch_size = as.integer(private$params$batch_size),
            per_device_eval_batch_size = as.integer(private$params$batch_size),
            save_safetensors = TRUE,
            auto_find_batch_size = FALSE,
            report_to = "none",
            log_level = "error",
            disable_tqdm = !private$params$pytorch_trace
          )

          private$temp$trainer <- transformers$Trainer(
            model = private$temp$model,
            train_dataset = tokenized_dataset$train,
            eval_dataset = tokenized_dataset$test,
            args = training_args,
            data_collator = data_collator,
            tokenizer = private$temp$tokenizer
          )
          private$temp$trainer$remove_callback(transformers$integrations$CodeCarbonCallback)
          if (!as.logical(private$params$pytorch_trace)) {
            private$temp$trainer$remove_callback(transformers$PrinterCallback)
            private$temp$trainer$remove_callback(transformers$ProgressCallback)
          }

          # Add Callback if Shiny App is running
          if (is_shinyapp_active()) {
            run_py_file("pytorch_transformer_callbacks.py")
            private$temp$trainer$add_callback(py$ReportAiforeducationShiny_PT())
          }
        }
      },
      start_training = function() {
        if (private$params$ml_framework == "tensorflow") {
          private$temp$model$fit(
            x = private$temp$tf_train_dataset,
            validation_data = private$temp$tf_test_dataset,
            epochs = as.integer(private$params$n_epoch),
            workers = as.integer(private$params$n_workers),
            use_multiprocessing = private$params$multi_process,
            callbacks = list(private$temp$callbacks),
            verbose = as.integer(private$params$keras_trace)
          )

          print_message("Load Weights From Best Checkpoint", private$params$trace)
          private$temp$model$load_weights(paste0(private$params$output_dir, "/checkpoints/best_weights.h5"))
        } else {
          private$temp$trainer$train()
        }
      },
      save_model = function() {
        if (private$params$ml_framework == "tensorflow") {
          private$temp$model$save_pretrained(
            save_directory = private$params$output_dir
          )
          history_log <- read.csv(file = paste0(private$params$output_dir, "/checkpoints/history.log"))
        } else {
          private$temp$model$save_pretrained(
            save_directory = private$params$output_dir,
            safe_serilization = private$temp$pt_safe_save
          )
          history_log <- pandas$DataFrame(private$temp$trainer$state$log_history)
          history_log <- clean_pytorch_log_transformers(history_log)
        }

        write.csv2(
          history_log,
          file = paste0(private$params$output_dir, "/history.log"),
          row.names = FALSE,
          quote = FALSE
        )
      }
    ),

    # Methods ----
    # Clear methods ----
    # Clears the private variables of the class: `sustainability_tracker`, `params` and `temp`
    clear_variables = function() {
      private$sustainability_tracker <- NULL
      private$params <- list()
      private$temp <- list()
    },

    # Clears required steps for creation (`steps_for_creation`) the transformer
    clear_required_SFC = function() {
      private$steps_for_creation$create_tokenizer_draft <- NULL
      private$steps_for_creation$calculate_vocab <- NULL
      private$steps_for_creation$save_tokenizer_draft <- NULL
      private$steps_for_creation$create_final_tokenizer <- NULL
      private$steps_for_creation$create_transformer_model <- NULL
    },

    # Clears required steps for training (`steps_for_training`) the transformer
    clear_required_SFT = function() {
      private$steps_for_training$load_existing_model <- NULL
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
    # Initializes the 'static' parameters of the transformer in the private `param` list
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

    # Creates a sustainability tracker and stores it in the private `sustainability_tracker` attribute
    create_sustain_tracker = function() {
      private$sustainability_tracker <- codecarbon$OfflineEmissionsTracker(
        country_iso_code = private$params$sustain_iso_code,
        region = private$params$sustain_region,
        tracking_mode = "machine",
        log_level = "warning",
        measure_power_secs = private$params$sustain_interval,
        save_to_file = FALSE,
        save_to_api = FALSE
      )
    }
  ),
  public = list(
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

    #' @description Setter for the parameters. Adds a new parameter and its value to the private `params` list.
    #' @param param_name `string` Parameter's name.
    #' @param param_value `any` Parameter's value.
    #' @return This method returns nothing.
    set_model_param = function(param_name, param_value) private$params[[param_name]] <- param_value,

    #' @description Setter for the temporary model's parameters. Adds a new temporary parameter and its value to the
    #'   private `temp` list.
    #' @param temp_name `string` Parameter's name.
    #' @param temp_value `any` Parameter's value.
    #' @return This method returns nothing.
    set_model_temp = function(temp_name, temp_value) private$temp[[temp_name]] <- temp_value,

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

    # Main methods ----

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
      # Clear variables -----------------------------------------------------------
      private$clear_variables()
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
        private$steps_for_creation$check_max_pos_emb()
      }
      private$temp$raw_text_dataset <- check.vocab_raw_texts(vocab_raw_texts)
      check.ml_framework(ml_framework)
      check.hidden_act(hidden_act)
      check.sustain_iso_code(sustain_iso_code, sustain_track)

      # Check possible save formats
      private$temp$pt_safe_save <- check.possible_save_formats(ml_framework, pytorch_safetensors)
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
      private$steps_for_creation$create_tokenizer_draft()
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 3

      # Calculating Vocabulary ----------------------------------------------------
      print_message("Start Computing Vocabulary", trace)
      run_py_file("datasets_transformer_compute_vocabulary.py")
      private$steps_for_creation$calculate_vocab()
      print_message("Start Computing Vocabulary - Done", trace)
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 4

      # Saving Tokenizer Draft ----------------------------------------------------
      print_message("Saving Draft", trace)
      create_dir(model_dir, trace, "Creating Model Directory")
      private$steps_for_creation$save_tokenizer_draft()
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 5

      # Final Tokenizer -----------------------------------------------------------
      print_message("Creating Tokenizer", trace)
      private$steps_for_creation$create_final_tokenizer()
      if ("tokenizer" %in% names(private$temp)) {
        stop("The final tokenizer must be stored in the 'tokenizer' parameter of the private 'temp' list.")
      }
      print_message("Creating Tokenizer - Done", trace)
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 6

      # Creating Transformer Model ------------------------------------------------
      print_message("Creating Transformer Model", trace)
      private$steps_for_creation$create_transformer_model()
      if ("model" %in% names(private$temp)) {
        stop("The transformer model must be stored in the 'model' parameter of the private 'temp' list.")
      }
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 7

      # Saving Model --------------------------------------------------------------
      print_message(paste("Saving", private$title), trace)
      private$steps_for_creation$save_transformer_model()
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 8

      # Saving Tokenizer ----------------------------------------------------------
      print_message("Saving Tokenizer Model", trace)
      private$temp$tokenizer$save_pretrained(model_dir)
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
      # Clear required steps ------------------------------------------------------
      private$clear_required_SFC()
    },

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
      # Clear variables -----------------------------------------------------------
      private$clear_variables()
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

      model_files_check <- check.model_files(model_dir_path)
      private$temp$from_pt <- model_files_check$from_pt
      private$temp$from_tf <- model_files_check$from_tf
      private$temp$load_safe <- model_files_check$load_safe

      check.sustain_iso_code(sustain_iso_code, sustain_track)

      # Check possible save formats
      private$temp$pt_safe_save <- check.possible_save_formats(ml_framework, pytorch_safetensors)
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
      private$steps_for_training$load_existing_model()
      if ("tokenizer" %in% names(private$temp)) {
        stop("The tokenizer must be stored in the 'tokenizer' parameter of the private 'temp' list.")
      }
      if ("model" %in% names(private$temp)) {
        stop("The transformer model must be stored in the 'model' parameter of the private 'temp' list.")
      }
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 3

      # argument checking------------------------------------------------------------
      private$steps_for_training$check_chunk_size()
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 4

      # creating chunks of sequences ----------------------------------------------
      print_message("Creating Chunks of Sequences for Training", trace)
      private$steps_for_training$create_chunks_for_training()

      n_chunks <- private$temp$tokenized_dataset$num_rows
      print_message(paste(n_chunks, "Chunks Created"), trace)
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 5

      # Seeting up DataCollator and Dataset ----------------------------------------
      create_dir(output_dir, trace, "Creating Output Directory")
      create_dir(paste0(output_dir, "/checkpoints"), trace, "Creating Checkpoint Directory")

      private$steps_for_training$prepare_train_tune()
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 6

      # Start Training -------------------------------------------------------------
      print_message("Start Fine Tuning", trace)
      private$steps_for_training$start_training()
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 7

      # Saving Model --------------------------------------------------------------
      print_message(paste("Saving", private$title), trace)
      private$steps_for_training$save_model()
      pgr_value <- increment_aife_progress_bar(pgr_value, pgr_max, private$title) # 8

      # Saving Tokenizer -----------------------------------------------------------
      print_message("Saving Tokenizer", trace)
      private$temp$tokenizer$save_pretrained(output_dir)
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
      # Clear required steps ------------------------------------------------------
      private$clear_required_SFT()
    }
  )
)
