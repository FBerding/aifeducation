#' @title Child `R6` class for creation and training of `RoBERTa` transformers
#'
#' @description This class has the following methods:
#'   * `create`: creates a new transformer based on `RoBERTa`.
#'   * `train`: trains and fine-tunes a `RoBERTa` model.
#'
#' @section Create: New models can be created using the `.AIFERobertaTransformer$create` method.
#'
#' @section Train: To train the model, pass the directory of the model to the method `.AIFERobertaTransformer$train`.
#'
#'   Pre-Trained models which can be fine-tuned with this function are available at <https://huggingface.co/>.
#'
#'   Training of this model makes use of dynamic masking.
#'
#' @param ml_framework `r paramDesc.ml_framework()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_region `r paramDesc.sustain_region()`
#' @param sustain_interval `r paramDesc.sustain_interval()`
#' @param trace `r paramDesc.trace()`
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
#'
#' @references Liu, Y., Ott, M., Goyal, N., Du, J., Joshi, M., Chen, D., Levy, O., Lewis, M., Zettlemoyer, L., &
#'   Stoyanov, V. (2019). RoBERTa: A Robustly Optimized BERT Pretraining Approach. \doi{10.48550/arXiv.1907.11692}
#'
#' @references Hugging Face Documentation
#'   * <https://huggingface.co/docs/transformers/model_doc/roberta>
#'   * <https://huggingface.co/docs/transformers/model_doc/roberta#transformers.RobertaModel>
#'   * <https://huggingface.co/docs/transformers/model_doc/roberta#transformers.TFRobertaModel>
#'
#' @family Transformers for developers
#'
#' @export
.AIFERobertaTransformer <- R6::R6Class(
  classname = ".AIFERobertaTransformer",
  inherit = .AIFEBaseTransformer,
  private = list(
    # Transformer's title
    title = "RoBERTa Model",

    # steps_for_creation `list()` that stores required and optional steps (functions) for creating a new transformer.
    #
    # `create_final_tokenizer()` **adds** temporary `tokenizer` parameter to the private `temp` list.
    #
    # `create_transformer_model()` **uses** `tokenizer` and **adds** `model` temporary parameters to the private `temp`
    # list.
    #
    # Use the `super$set_SFC_*()` methods to set required/optional steps for creation in the base class, where `*` is
    # the name of the step.
    #
    # Use the `super$set_required_SFC()` method to set all required steps in the base class at once.
    #
    # See the private `steps_for_creation` list in the base class `.AIFEBaseTransformer`, `Longformer_like.SFC.*`
    # functions for details.
    steps_for_creation = list(
      check_max_pos_emb = function() check.max_position_embeddings(private$params$max_position_embeddings),
      create_tokenizer_draft = function() Longformer_like.SFC.create_tokenizer_draft(),
      calculate_vocab = function() Longformer_like.SFC.calculate_vocab(),
      save_tokenizer_draft = function() Longformer_like.SFC.save_tokenizer_draft(),
      create_final_tokenizer = function() {
        private$temp$tokenizer <- transformers$RobertaTokenizerFast(
          vocab_file = paste0(private$params$model_dir, "/", "vocab.json"),
          merges_file = paste0(private$params$model_dir, "/", "merges.txt"),
          bos_token = "<s>",
          eos_token = "</s>",
          sep_token = "</s>",
          cls_token = "<s>",
          unk_token = "<unk>",
          pad_token = "<pad>",
          mask_token = "<mask>",
          add_prefix_space = private$params$add_prefix_space,
          trim_offsets = private$params$trim_offsets
        )
      },
      create_transformer_model = function() {
        configuration <- transformers$RobertaConfig(
          vocab_size = as.integer(length(private$temp$tokenizer$get_vocab())),
          max_position_embeddings = as.integer(private$params$max_position_embeddings),
          hidden_size = as.integer(private$params$hidden_size),
          num_hidden_layers = as.integer(private$params$num_hidden_layer),
          num_attention_heads = as.integer(private$params$num_attention_heads),
          intermediate_size = as.integer(private$params$intermediate_size),
          hidden_act = private$params$hidden_act,
          hidden_dropout_prob = private$params$hidden_dropout_prob,
          attention_probs_dropout_prob = private$params$attention_probs_dropout_prob,
          type_vocab_size = as.integer(2),
          initializer_range = 0.02,
          layer_norm_eps = 1e-12,
          position_embedding_type = "absolute",
          is_decoder = FALSE,
          use_cache = TRUE
        )
        private$temp$model <- ifelse(
          private$params$ml_framework == "tensorflow",
          transformers$TFRobertaModel(configuration),
          transformers$RobertaModel(configuration)
        )
      }
    ),

    # steps_for_training `list()` that stores required and optional steps (functions) for training a new transformer.
    #
    # `load_existing_model()` **adds** `tokenizer` and `model` temporary parameters to the private `temp` list.
    #
    # Use the `super$set_SFT_*()` methods to set required/optional steps for training in the base class, where `*` is
    # the name of the step.
    #
    # See the private `steps_for_training` list in the base class `.AIFEBaseTransformer` for details.
    steps_for_training = list(
      load_existing_model = function() {
        if (private$params$ml_framework == "tensorflow") {
          private$temp$model <- transformers$TFRobertaForMaskedLM$from_pretrained(
            private$params$model_dir_path,
            from_pt = private$temp$from_pt
          )
        } else {
          private$temp$model <- transformers$RobertaForMaskedLM$from_pretrained(
            private$params$model_dir_path,
            from_tf = private$temp$from_tf,
            use_safetensors = private$temp$load_safe
          )
        }

        private$temp$tokenizer <- transformers$RobertaTokenizerFast$from_pretrained(private$params$model_dir_path)
      }
    )
  ),
  public = list(
    #' @description Creates a new transformer based on `RoBERTa` and sets the title.
    #' @return This method returns nothing.
    initialize = function() {
      super$set_title(private$title)
      print(paste(private$title, "has been inizialized."))
    },

    #' @description This method creates a transformer configuration based on the `RoBERTa` base architecture and a
    #'   vocabulary based on `Byte-Pair Encoding` (BPE) tokenizer using the python `transformers` and `tokenizers`
    #'   libraries.
    #'
    #'   This method adds the following *'dependent' parameters* to the base class' private `params` list:
    #'   * `add_prefix_space`
    #'   * `trim_offsets`
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
    #' @param add_prefix_space `r paramDesc.add_prefix_space()`
    #' @param trim_offsets `r paramDesc.trim_offsets()`
    #' @param num_hidden_layer `r paramDesc.num_hidden_layer()`
    #'
    #' @return This method does not return an object. Instead, it saves the configuration and vocabulary of the new
    #'   model to disk.
    create = function(ml_framework = aifeducation_config$get_framework()[1],
                      model_dir,
                      vocab_raw_texts = NULL,
                      vocab_size = 30522,
                      add_prefix_space = FALSE,
                      trim_offsets = TRUE,
                      max_position_embeddings = 512,
                      hidden_size = 768,
                      num_hidden_layer = 12,
                      num_attention_heads = 12,
                      intermediate_size = 3072,
                      hidden_act = "gelu",
                      hidden_dropout_prob = 0.1,
                      attention_probs_dropout_prob = 0.1,
                      sustain_track = TRUE,
                      sustain_iso_code = NULL,
                      sustain_region = NULL,
                      sustain_interval = 15,
                      trace = TRUE,
                      pytorch_safetensors = TRUE) {
      # Init dependent parameters ----
      super$set_model_param("add_prefix_space", add_prefix_space)
      super$set_model_param("trim_offsets", trim_offsets)
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
        add_prefix_space = add_prefix_space,
        trim_offsets = trim_offsets,
        max_position_embeddings = max_position_embeddings,
        hidden_size = hidden_size,
        num_hidden_layer = num_hidden_layer,
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

    #' @description This method can be used to train or fine-tune a transformer based on `RoBERTa` Transformer
    #'   architecture with the help of the python libraries `transformers`, `datasets`, and `tokenizers`.
    #'
    #' @param output_dir `r paramDesc.output_dir()`
    #' @param model_dir_path `r paramDesc.model_dir_path()`
    #' @param raw_texts `r paramDesc.raw_texts()`
    #' @param p_mask `r paramDesc.p_mask()`
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
    train = function(ml_framework = aifeducation_config$get_framework()[1],
                     output_dir,
                     model_dir_path,
                     raw_texts,
                     p_mask = 0.15,
                     val_size = 0.1,
                     n_epoch = 1,
                     batch_size = 12,
                     chunk_size = 250,
                     full_sequences_only = FALSE,
                     min_seq_len = 50,
                     learning_rate = 3e-2,
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
        whole_word = FALSE,
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
