#' @title Child `R6` class for creation and training of `Funnel` transformers
#'
#' @description This class has the following methods:
#'   * `create`: creates a new transformer based on `Funnel`.
#'   * `train`: trains and fine-tunes a `Funnel` model.
#'
#' @section Create: New models can be created using the `.AIFEFunnelTransformer$create` method.
#'
#'   Model is created with `separete_cls = TRUE`, `truncate_seq = TRUE`, and `pool_q_only = TRUE`.
#'
#' @section Train: To train the model, pass the directory of the model to the method `.AIFEFunnelTransformer$train`.
#'
#'   Pre-Trained models which can be fine-tuned with this function are available at <https://huggingface.co/>.
#'
#'   Training of the model makes use of dynamic masking.
#'
#' @param ml_framework `r paramDesc.ml_framework()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_region `r paramDesc.sustain_region()`
#' @param sustain_interval `r paramDesc.sustain_interval()`
#' @param trace `r paramDesc.trace()`
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
#'
#' @note The model uses a configuration with `truncate_seq = TRUE` to avoid implementation problems with tensorflow.
#'
#' @note This model uses a `WordPiece` tokenizer like `BERT` and can be trained with whole word masking. The transformer
#'   library may display a warning, which can be ignored.
#'
#' @references Dai, Z., Lai, G., Yang, Y. & Le, Q. V. (2020). Funnel-Transformer: Filtering out Sequential Redundancy
#'   for Efficient Language Processing. \doi{10.48550/arXiv.2006.03236}
#'
#' @references Hugging Face documentation
#'   * <https://huggingface.co/docs/transformers/model_doc/funnel#funnel-transformer>
#'   * <https://huggingface.co/docs/transformers/model_doc/funnel#transformers.FunnelModel>
#'   * <https://huggingface.co/docs/transformers/model_doc/funnel#transformers.TFFunnelModel>
#'
#' @family Transformers for developers
#'
#' @export
.AIFEFunnelTransformer <- R6::R6Class(
  classname = ".AIFEFunnelTransformer",
  inherit = .AIFEBaseTransformer,
  private = list(
    # == Attributes ====================================================================================================

    # Transformer's title
    title = "Funnel Model",

    # steps_for_creation `list()` that stores required and optional steps (functions) for creating a new transformer.
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
        configuration <- transformers$FunnelConfig(
          vocab_size = as.integer(length(self$temp$tokenizer$get_vocab())),
          block_sizes = as.integer(self$params$block_sizes),
          block_repeats = NULL,
          num_decoder_layers = as.integer(self$params$num_decoder_layers),
          d_model = as.integer(self$params$hidden_size),
          n_head = as.integer(self$params$num_attention_heads),
          d_head = as.integer(self$params$target_hidden_size),
          d_inner = as.integer(self$params$intermediate_size),
          hidden_act = self$params$hidden_act,
          hidden_dropout_prob = self$params$hidden_dropout_prob,
          attention_probs_dropout_prob = self$params$attention_probs_dropout_prob,
          activation_dropout = as.integer(self$params$activation_dropout),
          initializer_range = 0.02,
          layer_norm_eps = 1e-12,
          pooling_type = self$params$pooling_type,
          attention_type = "relative_shift",
          separate_cls = TRUE,
          truncate_seq = TRUE,
          pool_q_only = TRUE,
          max_position_embeddings = as.integer(self$params$max_position_embeddings),
        )

        if (self$params$ml_framework == "tensorflow") {
          self$temp$model <- transformers$TFFunnelModel(configuration)
        } else {
          self$temp$model <- transformers$FunnelModel(configuration)
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
          self$temp$model <- transformers$TFFunnelForMaskedLM$from_pretrained(
            self$params$model_dir_path,
            from_pt = self$temp$from_pt
          )
        } else {
          self$temp$model <- transformers$FunnelForMaskedLM$from_pretrained(
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

    #' @description Creates a new transformer based on `Funnel` and sets the title.
    #' @return This method returns nothing.
    initialize = function() {
      super$set_title(private$title)
      print(paste(private$title, "has been inizialized."))
    },


    # Create ----

    #' @description This method creates a transformer configuration based on the `Funnel` transformer base architecture
    #'   and a vocabulary based on `WordPiece` using the python `transformers` and `tokenizers` libraries.
    #'
    #'   This method adds the following *'dependent' parameters* to the base class's inherited `params` list:
    #'   * `vocab_do_lower_case`
    #'   * `target_hidden_size`
    #'   * `block_sizes`
    #'   * `num_decoder_layers`
    #'   * `pooling_type`
    #'   * `activation_dropout`
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
    #' @param target_hidden_size `r paramDesc.target_hidden_size()`
    #' @param block_sizes `r paramDesc.block_sizes()`
    #' @param num_decoder_layers `r paramDesc.num_decoder_layers()`
    #' @param pooling_type `r paramDesc.pooling_type()`
    #' @param activation_dropout `r paramDesc.activation_dropout()`
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
                      target_hidden_size = 64,
                      block_sizes = c(4, 4, 4),
                      num_attention_heads = 12,
                      intermediate_size = 3072,
                      num_decoder_layers = 2,
                      pooling_type = "mean",
                      hidden_act = "gelu",
                      hidden_dropout_prob = 0.1,
                      attention_probs_dropout_prob = 0.1,
                      activation_dropout = 0.0,
                      sustain_track = TRUE,
                      sustain_iso_code = NULL,
                      sustain_region = NULL,
                      sustain_interval = 15,
                      trace = TRUE,
                      pytorch_safetensors = TRUE) {
      # Init dependent parameters ----
      super$set_model_param("vocab_do_lower_case", vocab_do_lower_case)
      super$set_model_param("target_hidden_size", target_hidden_size)
      super$set_model_param("block_sizes", block_sizes)
      super$set_model_param("num_decoder_layers", num_decoder_layers)
      super$set_model_param("pooling_type", pooling_type)
      super$set_model_param("activation_dropout", activation_dropout)

      # Define steps for creation (SFC) ----
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

    #' @description This method can be used to train or fine-tune a transformer based on `Funnel` Transformer
    #'   architecture with the help of the python libraries `transformers`, `datasets`, and `tokenizers`.
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
                     sustain_track = TRUE,
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
