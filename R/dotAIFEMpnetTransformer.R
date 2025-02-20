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

#' @title Child `R6` class for creation and training of `MPNet` transformers
#'
#' @description This class has the following methods:
#'   * `create`: creates a new transformer based on `MPNet`.
#'   * `train`: trains and fine-tunes a `MPNet` model.
#'
#' @section Create: New models can be created using the `.AIFEMpnetTransformer$create` method.
#'
#' @section Train: To train the model, pass the directory of the model to the method `.AIFEMpnetTransformer$train`.
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
#' @note Using this class with `tensorflow` is not supported. Supported framework is `pytorch`.
#'
#' @references Song,K., Tan, X., Qin, T., Lu, J. & Liu, T.-Y. (2020). MPNet: Masked and Permuted Pre-training for
#'   Language Understanding. \doi{10.48550/arXiv.2004.09297}
#'
#' @references Hugging Face documentation
#'   * <https://huggingface.co/docs/transformers/model_doc/mpnet>
#'   * <https://huggingface.co/docs/transformers/model_doc/mpnet#transformers.MPNetForMaskedLM>
#'   * <https://huggingface.co/docs/transformers/model_doc/mpnet#transformers.TFMPNetForMaskedLM>
#'
#' @family Transformers for developers
#'
#' @export
.AIFEMpnetTransformer <- R6::R6Class(
  classname = ".AIFEMpnetTransformer",
  inherit = .AIFEBaseTransformer,
  private = list(
    # == Attributes ====================================================================================================

    # Transformer's title
    title = "MPNet Model",

    # steps_for_creation `list()` that stores required and optional steps (functions) for creation a new transformer.
    steps_for_creation = list(

      # SFC: create_tokenizer_draft ----
      create_tokenizer_draft = function(self) {
        Bert_like.SFC.create_tokenizer_draft(
          self,
          sep_token = self$special_tokens_list$sep,
          sep_id = 2,
          cls_token = self$special_tokens_list$cls,
          cls_id = 0,
          unk_token = self$special_tokens_list$unk,
          special_tokens = unlist(unname(self$special_tokens_list))
        )
      },

      # SFC: calculate_vocab ----
      calculate_vocab = function(self) Bert_like.SFC.calculate_vocab(self),

      # SFC: save_tokenizer_draft ----
      save_tokenizer_draft = function(self) Bert_like.SFC.save_tokenizer_draft(self),

      # SFC: create_final_tokenizer ----
      create_final_tokenizer = function(self) {
        self$temp$tokenizer <- transformers$PreTrainedTokenizerFast(
          tokenizer_object = self$temp$tok_new,
          bos_token = self$special_tokens_list$cls,
          eos_token = self$special_tokens_list$sep,
          unk_token = self$special_tokens_list$unk,
          sep_token = self$special_tokens_list$sep,
          pad_token = self$special_tokens_list$pad,
          cls_token = self$special_tokens_list$cls,
          mask_token = self$special_tokens_list$mask
        )
      },

      # SFC: create_transformer_model ----
      create_transformer_model = function(self) {
        configuration <- transformers$MPNetConfig(
          vocab_size = as.integer(length(self$temp$tokenizer$get_vocab())),
          hidden_size = as.integer(self$params$hidden_size),
          num_hidden_layers = as.integer(self$params$num_hidden_layer),
          num_attention_heads = as.integer(self$params$num_attention_heads),
          intermediate_size = as.integer(self$params$intermediate_size),
          hidden_act = self$params$hidden_act,
          hidden_dropout_prob = self$params$hidden_dropout_prob,
          attention_probs_dropout_prob = self$params$attention_probs_dropout_prob,
          max_position_embeddings = as.integer(self$params$max_position_embeddings),
          initializer_range = 0.02,
          layer_norm_eps = 1e-12
        )

        if (self$params$ml_framework == "tensorflow") {
          run_py_file("MPNetForMPLM_TF.py")
          self$temp$model <- py$MPNetForMPLM_TF(configuration)
        } else {
          run_py_file("MPNetForMPLM_PT.py")
          device <- ifelse(torch$cuda$is_available(), "cuda", "cpu")
          self$temp$model <- py$MPNetForMPLM_PT(configuration)$to(device)
        }
      }
    ),


    # steps_for_training `list()` that stores required and optional steps (functions) for training a new transformer.
    steps_for_training = list(

      # SFT: load_existing_model ----
      load_existing_model = function(self) {
        if (self$params$ml_framework == "tensorflow") {
          self$temp$model <- py$MPNetForMPLM_TF$from_pretrained(
            self$params$model_dir_path,
            from_pt = self$temp$from_pt
          )
        } else {
          device <- ifelse(torch$cuda$is_available(), "cuda", "cpu")
          self$temp$model <- py$MPNetForMPLM_PT$from_pretrained(
            self$params$model_dir_path,
            from_tf = self$temp$from_tf,
            use_safetensors = self$temp$load_safe
          )$to(device)
        }

        self$temp$tokenizer <- transformers$AutoTokenizer$from_pretrained(self$params$model_dir_path)
      },

      # SFT: create_data_collator ----
      # Overwrite the default data collator
      create_data_collator = function(self) {
        collator_maker <- NULL
        if (self$params$ml_framework == "tensorflow") {
          run_py_file("DataCollatorForMPLM_TF.py")
          collator_maker <- py$CollatorMaker_TF(
            tokenizer = self$temp$tokenizer,
            mlm = TRUE,
            mlm_probability = self$params$p_mask,
            plm_probability = self$params$p_perm,
            mask_whole_words = self$params$whole_word
          )
        } else if (self$params$ml_framework == "pytorch") {
          run_py_file("DataCollatorForMPLM_PT.py")
          collator_maker <- py$CollatorMaker_PT(
            tokenizer = self$temp$tokenizer,
            mlm = TRUE,
            mlm_probability = self$params$p_mask,
            plm_probability = self$params$p_perm,
            mask_whole_words = self$params$whole_word
          )
        }
        if (!is.null(collator_maker)) {
          self$temp$data_collator <- collator_maker$collator$collate_batch
        }
      }
    )
  ),
  public = list(
    # == Attributes ====================================================================================================

    # Special tokens ----

    #' @field special_tokens_list `list` List for special tokens with the following elements:
    #'   * `cls` - CLS token representation (`<s>`)
    #'   * `pad` - pad token representation (`<pad>`)
    #'   * `sep` - sep token representation (`</s>`)
    #'   * `unk` - unk token representation (`<unk>`)
    #'   * `mask` - mask token representation (`<mask>`)
    special_tokens_list = list(
      cls = "<s>",
      pad = "<pad>",
      sep = "</s>",
      unk = "<unk>",
      mask = "<mask>"
    ),

    # == Methods =======================================================================================================

    # New ----

    #' @description Creates a new transformer based on `MPNet` and sets the title.
    #' @return This method returns nothing.
    initialize = function() {
      super$set_title(private$title)
      print(paste(private$title, "has been initialized."))
    },


    # Create ----

    #' @description This method creates a transformer configuration based on the `MPNet` base architecture.
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

      if (ml_framework == "tensorflow") {
        stop("Using AIFEMpnetTransformer with 'tensorflow' is not supported.")
      }

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

    #' @description This method can be used to train or fine-tune a transformer based on `MPNet` architecture with the
    #'   help of the python libraries `transformers`, `datasets`, and `tokenizers`.
    #'
    #'   This method adds the following *'dependent' parameter* to the base class's inherited `params` list:
    #'   * `p_perm`
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
    #' @param p_perm `double` Ratio that determines the number of words/tokens used for permutation.
    #'
    #' @return This method does not return an object. Instead the trained or fine-tuned model is saved to disk.
    train = function(ml_framework = "pytorch",
                     output_dir,
                     model_dir_path,
                     text_dataset,
                     p_mask = 0.15,
                     p_perm = 0.15,
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

      if (ml_framework == "tensorflow") {
        stop("Using AIFEMpnetTransformer with 'tensorflow' is not supported.")
      }

      # Init dependent parameters ----
      super$set_model_param("p_perm", p_perm)

      # Define steps for training (SFT) ----
      # Required steps
      super$set_SFT_load_existing_model(private$steps_for_training$load_existing_model)

      super$set_SFT_create_data_collator(private$steps_for_training$create_data_collator)

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

.AIFETrObj[[AIFETrType$mpnet]] <- .AIFEMpnetTransformer$new
