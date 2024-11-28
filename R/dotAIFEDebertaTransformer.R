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

#' @title Child `R6` class for creation and training of `DeBERTa-V2` transformers
#'
#' @description This class has the following methods:
#'   * `create`: creates a new transformer based on `DeBERTa-V2`.
#'   * `train`: trains and fine-tunes a `DeBERTa-V2` model.
#'
#' @section Create: New models can be created using the `.AIFEDebertaTransformer$create` method.
#'
#' @section Train: To train the model, pass the directory of the model to the method `.AIFEDebertaTransformer$train`.
#'
#'    Pre-Trained models which can be fine-tuned with this function are available at <https://huggingface.co/>.
#'
#'    Training of this model makes use of dynamic masking.
#'
#' @param ml_framework `r paramDesc.ml_framework()`
#' @param text_dataset `r paramDesc.text_dataset()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_region `r paramDesc.sustain_region()`
#' @param sustain_interval `r paramDesc.sustain_interval()`
#' @param trace `r paramDesc.trace()`
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
#' @param log_dir `r paramDesc.log_dir()`
#' @param log_write_interval `r paramDesc.log_write_interval()`
#'
#'
#' @note For this model a `WordPiece` tokenizer is created. The standard implementation of `DeBERTa` version 2 from
#'   HuggingFace uses a `SentencePiece` tokenizer. Thus, please use `AutoTokenizer` from the `transformers` library to
#'   work with this model.
#'
#' @references He, P., Liu, X., Gao, J. & Chen, W. (2020). DeBERTa: Decoding-enhanced BERT with Disentangled Attention.
#'   \doi{10.48550/arXiv.2006.03654}
#'
#' @references Hugging Face documentatio
#'   * <https://huggingface.co/docs/transformers/model_doc/deberta-v2>
#'   * <https://huggingface.co/docs/transformers/model_doc/deberta-v2#transformers.DebertaV2ForMaskedLM>
#'   * <https://huggingface.co/docs/transformers/model_doc/deberta-v2#transformers.TFDebertaV2ForMaskedLM>
#'
#' @family Transformers for developers
#'
#' @export
.AIFEDebertaTransformer <- R6::R6Class(
  classname = ".AIFEDebertaTransformer",
  inherit = .AIFEBaseTransformer,
  private = list(
    # == Attributes ====================================================================================================

    # Transformer's title
    title = "DeBERTa Model",

    # steps_for_creation `list()` that stores required and optional steps (functions) for creation a new transformer.
    #
    # `create_transformer_model()` **uses** `tokenizer` and **adds** `model` temporary parameters to the inherited
    # `temp` list.
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
        configuration <- transformers$DebertaV2Config(
          vocab_size = as.integer(length(self$temp$tokenizer$get_vocab())),
          max_position_embeddings = as.integer(self$params$max_position_embeddings),
          hidden_size = as.integer(self$params$hidden_size),
          num_hidden_layers = as.integer(self$params$num_hidden_layer),
          num_attention_heads = as.integer(self$params$num_attention_heads),
          intermediate_size = as.integer(self$params$intermediate_size),
          hidden_act = self$params$hidden_act,
          hidden_dropout_prob = self$params$hidden_dropout_prob,
          attention_probs_dropout_prob = self$params$attention_probs_dropout_prob,
          type_vocab_size = as.integer(0),
          initializer_range = 0.02,
          layer_norm_eps = 1e-12,
          relative_attention = TRUE,
          max_relative_positions = as.integer(self$params$max_position_embeddings),
          pad_token_id = self$temp$tokenizer$pad_token_id,
          position_biased_input = FALSE,
          pos_att_type = c("p2c", "c2p")
        )

        if (self$params$ml_framework == "tensorflow") {
          self$temp$model <- transformers$TFDebertaV2ForMaskedLM(configuration)
        } else {
          self$temp$model <- transformers$DebertaV2ForMaskedLM(configuration)
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
          self$temp$model <- transformers$TFDebertaV2ForMaskedLM$from_pretrained(
            self$params$model_dir_path,
            from_pt = self$temp$from_pt
          )
        } else {
          self$temp$model <- transformers$DebertaV2ForMaskedLM$from_pretrained(
            self$params$model_dir_path,
            from_tf = self$temp$from_tf,
            use_safetensors = self$temp$load_safe
          )
        }

        self$temp$tokenizer <- transformers$AutoTokenizer$from_pretrained(self$params$model_dir_path)
      },

      # SFT: cuda_empty_cache ----
      cuda_empty_cache = function() {
        if (torch$cuda$is_available()) {
          torch$cuda$empty_cache()
        }
      }
    )
  ),
  public = list(
    # == Methods =======================================================================================================

    # New ----

    #' @description Creates a new transformer based on `DeBERTa-V2` and sets the title.
    #' @return This method returns nothing.
    initialize = function() {
      super$set_title(private$title)
      print(paste(private$title, "has been initialized."))
    },


    # Create ----

    #' @description This method creates a transformer configuration based on the `DeBERTa-V2` base architecture and a
    #'   vocabulary based on the `SentencePiece` tokenizer using the python `transformers` and `tokenizers` libraries.
    #'
    #'   This method adds the following *'dependent' parameters* to the base class's inherited `params` list:
    #'   * `vocab_do_lower_case`
    #'   * `num_hidden_layer`
    #'
    #' @param model_dir `r paramDesc.model_dir()`
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
                      text_dataset,
                      vocab_size = 128100,
                      vocab_do_lower_case = FALSE,
                      max_position_embeddings = 512,
                      hidden_size = 1536,
                      num_hidden_layer = 24,
                      num_attention_heads = 24,
                      intermediate_size = 6144,
                      hidden_act = "gelu",
                      hidden_dropout_prob = 0.1,
                      attention_probs_dropout_prob = 0.1,
                      sustain_track = TRUE,
                      sustain_iso_code = NULL,
                      sustain_region = NULL,
                      sustain_interval = 15,
                      trace = TRUE,
                      pytorch_safetensors = TRUE,
                      log_dir = NULL,
                      log_write_interval = 2) {
      # Init dependent parameters ----
      super$set_model_param("vocab_do_lower_case", vocab_do_lower_case)
      super$set_model_param("num_hidden_layer", num_hidden_layer)

      # Define steps for creation (SFC) ----
      # Required steps
      super$set_required_SFC(private$steps_for_creation)

      # Create method of super ----
      super$create(
        ml_framework = ml_framework,
        model_dir = model_dir,
        text_dataset = text_dataset,
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
        pytorch_safetensors = pytorch_safetensors,
        log_dir = log_dir,
        log_write_interval = log_write_interval
      )
    },


    # Train ----

    #' @description This method can be used to train or fine-tune a transformer based on `DeBERTa-V2` architecture with
    #'   the help of the python libraries `transformers`, `datasets`, and `tokenizers`.
    #'
    #' @param output_dir `r paramDesc.output_dir()`
    #' @param model_dir_path `r paramDesc.model_dir_path()`
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
                     text_dataset,
                     p_mask = 0.15,
                     whole_word = TRUE,
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
                     pytorch_safetensors = TRUE,
                     log_dir = NULL,
                     log_write_interval = 2) {
      # Define steps for training (SFT) ----
      # Required steps
      super$set_SFT_load_existing_model(private$steps_for_training$load_existing_model)
      # Optional steps
      super$set_SFT_cuda_empty_cache(private$steps_for_training$cuda_empty_cache)

      # Train method of super ----
      super$train(
        ml_framework = ml_framework,
        output_dir = output_dir,
        model_dir_path = model_dir_path,
        text_dataset = text_dataset,
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
        pytorch_safetensors = pytorch_safetensors,
        log_dir = log_dir,
        log_write_interval = log_write_interval
      )
    }
  )
)

.AIFETrObj[[AIFETrType$deberta_v2]] <- .AIFEDebertaTransformer$new
