#' @title Child `R6` class for creation and training of `BERT` transformers
#'
#' @description This class has the following methods:
#'   * `create`: creates a new transformer based on `BERT`.
#'   * `train`: trains and fine-tunes a `BERT` model.
#'
#' @section Create: New models can be created using the `.AIFEBertTransformer$create` method.
#'
#' @section Train: To train the model, pass the directory of the model to the method `.AIFEBertTransformer$train`.
#'
#'   Pre-Trained models that can be fine-tuned using this method are available at <https://huggingface.co/>.
#'
#'   The model is trained using dynamic masking, as opposed to the original paper, which used static masking.
#'
#' @param ml_framework `r paramDesc.ml_framework()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_region `r paramDesc.sustain_region()`
#' @param sustain_interval `r paramDesc.sustain_interval()`
#' @param trace `r paramDesc.trace()`
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
#'
#' @note This model uses a `WordPiece` tokenizer like `BERT` and can be trained with whole word masking. The transformer
#'   library may display a warning, which can be ignored.
#'
#' @references Devlin, J., Chang, M.‑W., Lee, K., & Toutanova, K. (2019). BERT: Pre-training of Deep Bidirectional
#'   Transformers for Language Understanding. In J. Burstein, C. Doran, & T. Solorio (Eds.), Proceedings of the 2019
#'   Conference of the North (pp. 4171--4186). Association for Computational Linguistics. \doi{10.18653/v1/N19-1423}
#'
#' @references Hugging Face documentation
#'   * <https://huggingface.co/docs/transformers/model_doc/bert>
#'   * <https://huggingface.co/docs/transformers/model_doc/bert#transformers.BertForMaskedLM>
#'   * <https://huggingface.co/docs/transformers/model_doc/bert#transformers.TFBertForMaskedLM>
#'
#' @family Transformers for developers
#'
#' @export
.AIFEBertTransformer <- R6::R6Class(
  classname = ".AIFEBertTransformer",
  inherit = .AIFEBaseTransformer,
  private = list(
    # == Attributes ====================================================================================================

    # Transformer's title
    title = "BERT Model",

    # steps_for_creation `list()` that stores required and optional steps (functions) for creation a new transformer.
    #
    # `create_transformer_model()` **uses** `tokenizer` and **adds** `model` temporary parameters to the inherited `temp`
    # list.
    #
    # Use the `super$set_SFC_*()` methods to set required/optional steps for creation in the base class, where `*` is
    # the name of the step.
    #
    # Use the `super$set_required_SFC()` method to set all required steps in the base class at once.
    #
    # See the private `steps_for_creation` list in the base class `.AIFEBaseTransformer`, `Bert_like.SFC.*` functions
    # for details.
    steps_for_creation = list(

      # SFC: check_max_pos_emb ----
      check_max_pos_emb = function(self) check.max_position_embeddings(self$params$max_position_embeddings),

      # SFC: create_tokenizer_draft ----
      create_tokenizer_draft = function(self) Bert_like.SFC.create_tokenizer_draft(self),

      # SFC: calculate_vocab ----
      calculate_vocab = function(self) Bert_like.SFC.calculate_vocab(self),

      # SFC: save_tokenizer_draft ----
      save_tokenizer_draft = function(self) Bert_like.SFC.save_tokenizer_draft(self),

      # SFC: create_final_tokenizer ----
      create_final_tokenizer = function(self) Bert_like.SFC.create_final_tokenizer(self),

      # SFC: create_transformer_model ----
      create_transformer_model = function(self) {
        configuration <- transformers$BertConfig(
          vocab_size = as.integer(length(self$temp$tokenizer$get_vocab())),
          max_position_embeddings = as.integer(self$params$max_position_embeddings),
          hidden_size = as.integer(self$params$hidden_size),
          num_hidden_layers = as.integer(self$params$num_hidden_layer),
          num_attention_heads = as.integer(self$params$num_attention_heads),
          intermediate_size = as.integer(self$params$intermediate_size),
          hidden_act = self$params$hidden_act,
          hidden_dropout_prob = self$params$hidden_dropout_prob,
          attention_probs_dropout_prob = self$params$attention_probs_dropout_prob,
          pad_token_id = self$temp$tokenizer$pad_token_id,
          type_vocab_size = as.integer(2),
          initializer_range = 0.02,
          layer_norm_eps = 1e-12,
          position_embedding_type = "absolute",
          is_decoder = FALSE,
          use_cache = TRUE
        )
        if (self$params$ml_framework == "tensorflow") {
          self$temp$model <- transformers$TFBertModel(configuration)
        } else {
          self$temp$model <- transformers$BertModel(configuration)
        }
      }
    ),

    # steps_for_training `list()` that stores required and optional steps (functions) for training a new transformer.
    #
    # `load_existing_model()` **adds** `tokenizer` and `model` temporary parameters to the inherited `temp` list.
    #
    # Use the `super$set_SFT_*()` methods to set required/optional steps for training in the base class, where `*` is
    # the name of the step.
    #
    # See the private `steps_for_training` list in the base class `.AIFEBaseTransformer` for details.
    steps_for_training = list(

      # SFT: load_existing_model ----
      load_existing_model = function(self) {
        if (self$params$ml_framework == "tensorflow") {
          self$temp$model <- transformers$TFBertForMaskedLM$from_pretrained(
            self$params$model_dir_path,
            from_pt = self$temp$from_pt
          )
        } else {
          self$temp$model <- transformers$BertForMaskedLM$from_pretrained(
            self$params$model_dir_path,
            from_tf = self$temp$from_tf,
            use_safetensors = self$temp$load_safe
          )
        }

        self$temp$tokenizer <- transformers$AutoTokenizer$from_pretrained(self$params$model_dir_path)
      }
    )
  ),
  public = list(
    # == Methods =======================================================================================================

    # New ----

    #' @description Creates a new transformer based on `BERT` and sets the title.
    #' @return This method returns nothing.
    initialize = function() {
      super$set_title(private$title)
      print(paste(private$title, "has been inizialized."))
    },


    # Create ----

    #' @description This method creates a transformer configuration based on the `BERT` base architecture and a
    #'   vocabulary based on `WordPiece` by using the python libraries `transformers` and `tokenizers`.
    #'
    #'   This method adds the following *'dependent' parameters* to the base class's inherited `params` list:
    #'   * `vocab_do_lower_case`
    #'   * `num_hidden_layer`
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
    #' @param vocab_do_lower_case `r paramDesc.vocab_do_lower_case()`
    #' @param num_hidden_layer `r paramDesc.num_hidden_layer()`
    #'
    #' @return This method does not return an object. Instead, it saves the configuration and vocabulary of the new
    #'   model to disk.
    create = function(ml_framework = "pytorch",
                      model_dir,
                      vocab_raw_texts = NULL,
                      vocab_size = 30522,
                      vocab_do_lower_case = FALSE,
                      max_position_embeddings = 512,
                      hidden_size = 768,
                      num_hidden_layer = 12,
                      num_attention_heads = 12,
                      intermediate_size = 3072,
                      hidden_act = "gelu",
                      hidden_dropout_prob = 0.1,
                      attention_probs_dropout_prob = 0.1,
                      sustain_track = FALSE,
                      sustain_iso_code = NULL,
                      sustain_region = NULL,
                      sustain_interval = 15,
                      trace = TRUE,
                      pytorch_safetensors = TRUE) {
      # Init dependent parameters ----
      super$set_model_param("vocab_do_lower_case", vocab_do_lower_case)
      super$set_model_param("num_hidden_layer", num_hidden_layer)

      # Define steps for creation (SFC) ----
      # Optional steps
      super$set_SFC_check_max_pos_emb(private$steps_for_creation$check_max_pos_emb)
      # Required steps
      super$set_required_SFC(private$steps_for_creation)

      # Create method of super ----
      super$create(
        ml_framework = ml_framework,
        model_dir = model_dir,
        vocab_raw_texts = vocab_raw_texts,
        vocab_size = vocab_size,
        max_position_embeddings = max_position_embeddings,
        hidden_size = hidden_size,
        num_attention_heads = num_attention_heads,
        intermediate_size = intermediate_size,
        hidden_act = hidden_act,
        hidden_dropout_prob = hidden_dropout_prob,
        attention_probs_dropout_prob = attention_probs_dropout_prob,
        sustain_track = sustain_track,
        sustain_iso_code = sustain_iso_code,
        sustain_region = sustain_region,
        sustain_interval = sustain_interval,
        trace = trace,
        pytorch_safetensors = pytorch_safetensors
      )
    },


    # Train ----

    #' @description This method can be used to train or fine-tune a transformer based on `BERT` architecture with the
    #'   help of the python libraries `transformers`, `datasets`, and `tokenizers`.
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
    #' @return This method does not return an object. Instead the trained or fine-tuned model is saved to disk.
    train = function(ml_framework = "pytorch",
                     output_dir,
                     model_dir_path,
                     raw_texts,
                     p_mask = 0.15,
                     whole_word = TRUE,
                     val_size = 0.1,
                     n_epoch = 1,
                     batch_size = 12,
                     chunk_size = 250,
                     full_sequences_only = FALSE,
                     min_seq_len = 50,
                     learning_rate = 3e-3,
                     n_workers = 1,
                     multi_process = FALSE,
                     sustain_track = FALSE,
                     sustain_iso_code = NULL,
                     sustain_region = NULL,
                     sustain_interval = 15,
                     trace = TRUE,
                     keras_trace = 1,
                     pytorch_trace = 1,
                     pytorch_safetensors = TRUE) {
      # Define steps for training (SFT) ----
      # Required steps
      super$set_SFT_load_existing_model(private$steps_for_training$load_existing_model)

      # Train method of super ----
      super$train(
        ml_framework = ml_framework,
        output_dir = output_dir,
        model_dir_path = model_dir_path,
        raw_texts = raw_texts,
        p_mask = p_mask,
        whole_word = whole_word,
        val_size = val_size,
        n_epoch = n_epoch,
        batch_size = batch_size,
        chunk_size = chunk_size,
        full_sequences_only = full_sequences_only,
        min_seq_len = min_seq_len,
        learning_rate = learning_rate,
        n_workers = n_workers,
        multi_process = multi_process,
        sustain_track = sustain_track,
        sustain_iso_code = sustain_iso_code,
        sustain_region = sustain_region,
        sustain_interval = sustain_interval,
        trace = trace,
        keras_trace = keras_trace,
        pytorch_trace = pytorch_trace,
        pytorch_safetensors = pytorch_safetensors
      )
    }
  )
)

.AIFETrObj[[AIFETrType$bert]] <- .AIFEBertTransformer$new
