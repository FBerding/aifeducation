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

# Need xformer to be installed

#' @title Child `R6` class for creation and training of `NeoBERT` transformers
#'
#' @description This class has the following methods:
#'   * `create`: creates a new transformer based on `NeoBERT`.
#'   * `train`: trains and fine-tunes a `NeoBERT` model.
#'
#' @section Create: New models can be created using the `.AIFENeoBertTransformer$create` method.
#'
#' @section Train: To train the model, pass the directory of the model to the method `.AIFENeoBertTransformer$train`.
#'
#'   Pre-Trained models that can be fine-tuned using this method are available at <https://huggingface.co/>.
#'
#'   The model is trained using dynamic masking, as opposed to the original paper, which used static masking.
#'
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
.AIFENeoBertTransformer <- R6::R6Class(
  classname = ".AIFENeoBertTransformer",
  inherit = .AIFEBaseTransformer,
  private = list(
    # == Attributes ====================================================================================================

    title = "NeoBERT Model",

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

      check_max_pos_emb = function(self) check.max_position_embeddings(self$params$max_position_embeddings),

      create_tokenizer_draft = function(self) Bert_like.SFC.create_tokenizer_draft(self),

      calculate_vocab = function(self) Bert_like.SFC.calculate_vocab(self),

      save_tokenizer_draft = function(self) Bert_like.SFC.save_tokenizer_draft(self),

      create_final_tokenizer = function(self) Bert_like.SFC.create_final_tokenizer(self),

      create_transformer_model = function(self) {
        self$temp$model = transformers$AutoModel$from_pretrained(
          "chandar-lab/NeoBERT",
          trust_remote_code = TRUE,
          device_map = "cpu",
          low_cpu_mem_usage = TRUE
        )
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

      load_existing_model = function(self) {
        self$temp$model <- transformers$AutoModel$from_pretrained(
          self$params$model_dir_path,
          from_tf = self$temp$from_tf,
          use_safetensors = self$temp$load_safe
        )

        self$temp$tokenizer <- transformers$AutoTokenizer$from_pretrained(self$params$model_dir_path)
      }

    )
  ),

  public = list(
    # == Methods =======================================================================================================

    # New ----

    #' @description Creates a new transformer based on `NeoBERT` and sets the title.
    #' @param init_trace `bool` option to show prints. If `TRUE` (by default) - messages will be shown, otherwise
    #'   (`FALSE`) - hidden.
    #' @return This method returns nothing.
    initialize = function(init_trace = TRUE) {
      super$init_transformer(private$title, init_trace)
    },


    # Create ----

    #' @description This method creates a transformer configuration based on the `NeoBERT` base architecture and a
    #'   vocabulary based on `WordPiece` by using the python libraries `transformers` and `tokenizers`.
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
    create = function(model_dir,
                      text_dataset,
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
                      pytorch_safetensors = TRUE,
                      log_dir = NULL,
                      log_write_interval = 2) {
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

    #' @description This method can be used to train or fine-tune a transformer based on `NeoBERT` architecture with
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
    train = function(output_dir,
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
                     pytorch_safetensors = TRUE,
                     log_dir = NULL,
                     log_write_interval = 2) {
      # Define steps for training (SFT) ----
      # Required steps
      super$set_SFT_load_existing_model(private$steps_for_training$load_existing_model)

      # Train method of super ----
      super$train(
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

.AIFETrObj[[AIFETrType$neobert]] <- .AIFENeoBertTransformer$new
.AIFETrTokenizer[[AIFETrType$neobert]] <- "AutoTokenizer"
.AIFETrConfig[[AIFETrType$neobert]] <- "AutoConfig"
.AIFETrModel[[AIFETrType$neobert]] <- "AutoModel"
.AIFETrModelMLM[[AIFETrType$neobert]] <- "AutoModel"
