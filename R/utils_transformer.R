# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Estimate tokenizer statistics
#' @description Function for estimating the tokenizer statistics described by Kaya & Tantuğ (2024).
#'
#' @param dataset Object of class datasets.arrow_dataset.Dataset. The data set must contain a column `"length"`
#'   containing the number of tokens for every sequence and a column `"word_ids"` containing the word ids within every
#'   sequence.
#' @param step `string` indicating to which step the statistics belong. Recommended values are
#' * `"creation"` for the creation of the tokenizer.
#' * `"initial_training"` for the first training of the transformer.
#' * `"fine_tuning"` for all following trainings of the transformer.
#' * `"training"` for a training run of the transformer.
#' @return Returns a `list` with the following entries:
#' * n_sequences: Number of sequences
#' * n_words: Number for words in whole corpus
#' * n_tokens: Number of tokens in the whole corpus
#' * mu_t: eqn(n_tokens/n_sequences)
#' * mu_w: eqn(n_words/n_sequences)
#' * mu_g: eqn(n_tokens/n_words)
#'
#' @references Kaya, Y. B., & Tantuğ, A. C. (2024). Effect of tokenization granularity for Turkish large language
#' models. Intelligent Systems with Applications, 21, 200335. https://doi.org/10.1016/j.iswa.2024.200335
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
calc_tokenizer_statistics <- function(dataset, step = "creation") {
  # Argument Checking
  check_class(object=dataset, classes="datasets.arrow_dataset.Dataset", allow_NULL=FALSE)

  n_sequences <- dataset$num_rows
  n_words <- NA
  n_tokens <- NA
  mu_t <- NA
  mu_w <- NA
  mu_g <- NA

  if (step == "training" || step == "creation") {
    if ("word_ids" %in% dataset$column_names == FALSE) {
      stop("dataset must contain a column 'word_ids'.")
    }
    if ("length" %in% dataset$column_names == FALSE) {
      stop("dataset must contain a column 'length'.")
    }

    n_words <- 0
    n_tokens <- 0
    for (i in 1:n_sequences) {
      n_words <- n_words + length(unique(unlist(dataset[i - 1]$word_ids)))
      n_tokens <- n_tokens + dataset[i - 1]$length
    }

    mu_t <- n_tokens / n_sequences
    mu_w <- n_words / n_sequences
    mu_g <- n_tokens / n_words
  } else {
    stop(paste("Step", step, "is invalid. Allowed steps: creation or training"))
  }

  return(
    list(
      step = step,
      date = date(),
      n_sequences = n_sequences,
      n_words = n_words,
      n_tokens = n_tokens,
      mu_t = mu_t,
      mu_w = mu_w,
      mu_g = mu_g
    )
  )
}


#' @title Check `max_position_embeddings` argument of transformer
#' @description Used when creating and training transformers.
#'
#' @param max_position_embeddings `r paramDesc.max_position_embeddings()`
#' @return Warning if `max_position_embeddings` greater than 512.
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
check.max_position_embeddings <- function(max_position_embeddings) { # nolint
  if (max_position_embeddings > 512) {
    warning("Due to a quadratic increase in memory requirments it is not
            recommended to set max_position_embeddings above 512.
            If you want to analyse long documents please split your document
            into several chunks with an object of class TextEmbedding Model or
            use another transformer (e.g. longformer).")
  }
}

#' @title Check `hidden_act` argument of transformer
#' @description Used when creating and training transformers.
#'
#' @param hidden_act `r paramDesc.hidden_act()`
#' @return Error if `hidden_act` is not `"gelu"`, `"relu"`, `"silu"` or `"gelu_new"`.
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
check.hidden_act <- function(hidden_act) { # nolint
  if ((hidden_act %in% c("gelu", "relu", "silu", "gelu_new")) == FALSE) {
    stop("hidden_act must be gelu, relu, silu or gelu_new")
  }
}

#' @title Check `sustain_iso_code` argument of transformer
#' @description Used when creating and training transformers.
#'
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @return Error if `sustain_track` is `TRUE` and `sustain_iso_code` is missing (`NULL`).
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
check.sustain_iso_code <- function(sustain_iso_code, sustain_track) { # nolint
  if (sustain_track && is.null(sustain_iso_code)) {
    stop("Sustainability tracking is activated but iso code for the
         country is missing. Add iso code or deactivate tracking.")
  }
}

#' @title Check possible save formats
#' @description Used when creating and training transformers.
#'
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
#' @return Whether to save the model using `safetensors` or the traditional `pytorch` way.
#'
#' @importFrom reticulate py_module_available
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
check.possible_save_formats <- function(pytorch_safetensors) { # nolint
  safetensors_available <- reticulate::py_module_available("safetensors")
  pt_safe_save <- pytorch_safetensors && safetensors_available
  if (pytorch_safetensors && !safetensors_available) {
    warning("Python library 'safetensors' not available. Model will be saved
            in the standard pytorch format.")
  }
  return(pt_safe_save)
}

#' @title Check model files
#' @description Used when creating and training transformers. Checks `pytorch_model.bin`, `model.safetensors` and
#'   `tf_model.h5` files.
#'
#' @param model_dir_path `r paramDesc.model_dir_path()`
#' @return A list with the variables `from_tf` and `load_safe`.
#'
#' @importFrom reticulate py_module_available
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
check.model_files <- function(model_dir_path) { # nolint
  bin_exists <- file.exists(paste0(model_dir_path, "/pytorch_model.bin"))
  safetensors_exists <- file.exists(paste0(model_dir_path, "/model.safetensors"))
  h5_exists <- file.exists(paste0(model_dir_path, "/tf_model.h5"))

  if (!bin_exists && !safetensors_exists && !h5_exists) {
    stop("Directory does not contain a tf_model.h5, pytorch_model.bin or
         a model.safetensors file.")
  }

  from_tf <- !bin_exists && !safetensors_exists && h5_exists

  # Check to load from pt/bin or safetensors
  # Use safetensors as preferred method

  safetensors_available <- reticulate::py_module_available("safetensors")
  load_safe <- (safetensors_exists || from_tf) && safetensors_available

  return(
    list(
      from_tf = from_tf,
      load_safe = load_safe
    )
  )
}

#' @title Create `WordPiece` tokenizer
#' @description Used when creating transformers.
#'
#' @param vocab_do_lower_case `r paramDesc.vocab_do_lower_case()`
#' @param sep_token `string` Representation of the SEP token.
#' @param sep_id `int` ID of the SEP token.
#' @param cls_token `string` Representation of the CLS token.
#' @param cls_id `int` ID of the CLS token.
#' @param unk_token `string` Representation of the UNK token.
#' @return A new tokenizer object (`tokenizers.Tokenizer`) based on `tokenizers.models.WordPiece` model.
#'
#' @importFrom reticulate tuple
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
create_WordPiece_tokenizer <- function(# nolint
    vocab_do_lower_case,
    sep_token = "[SEP]",
    sep_id = 1,
    cls_token = "[CLS]",
    cls_id = 0,
    unk_token = "[UNK]") {
  tok_new <- tok$Tokenizer(tok$models$WordPiece(unk_token = unk_token))
  tok_new$normalizer <- tok$normalizers$BertNormalizer(
    lowercase = vocab_do_lower_case,
    clean_text = TRUE,
    handle_chinese_chars = TRUE,
    strip_accents = vocab_do_lower_case
  )
  tok_new$pre_tokenizer <- tok$pre_tokenizers$BertPreTokenizer()
  tok_new$post_processor <- tok$processors$BertProcessing(
    sep = reticulate::tuple(list(sep_token, as.integer(sep_id))),
    cls = reticulate::tuple(list(cls_token, as.integer(cls_id)))
  )
  tok_new$decode <- tok$decoders$WordPiece()
  return(tok_new)
}

#' @title Create `ByteLevelBPE` tokenizer
#' @description Used when creating transformers.
#'
#' @param max_position_embeddings `r paramDesc.max_position_embeddings()`
#' @param add_prefix_space `r paramDesc.add_prefix_space()`
#' @param trim_offsets `r paramDesc.trim_offsets()`
#' @return A new tokenizer object (`tokenizers.Tokenizer`) based on `tokenizers.models.ByteLevel` model.
#'
#' @family Transformer utils
#' @keywords internal
#' @noRd
create_ByteLevelBPE_tokenizer <- function(# nolint
    max_position_embeddings,
    add_prefix_space,
    trim_offsets) {
  tok_new <- tok$ByteLevelBPETokenizer(
    add_prefix_space = add_prefix_space,
    unicode_normalizer = "nfc",
    trim_offsets = trim_offsets,
    lowercase = FALSE
  )
  tok_new$enable_truncation(max_length = as.integer(max_position_embeddings))
  tok_new$enable_padding(pad_token = "<pad>")
  return(tok_new)
}

#' @title BERT-like creation step `create_tokenizer_draft`
#' @description Relevant only for transformer classes (BERT, DeBERTa, Funnel, etc.). Do not use outside the classes.
#'
#'   This function **adds** `special_tokens`, `tok_new`, `trainer` parameters into the `temp` list.
#'
#'   See private list `steps_for_creation` of [.AIFEBaseTransformer] class for details. This list has the elements
#'   as already defined functions that can add some temporary parameters into the `temp` list of the base class
#'   [.AIFEBaseTransformer] or use these temporary parameters.
#'
#' @param self Transformer `self`-object.
#' @param sep_token `string` Representation of the SEP token.
#' @param sep_id `int` ID of the SEP token.
#' @param cls_token `string` Representation of the CLS token.
#' @param cls_id `int` ID of the CLS token.
#' @param unk_token `string` Representation of the UNK token.
#' @param special_tokens `list` Special tokens for a trainer (`tokenizers.trainers.WordPieceTrainer`).
#' @return This function returns nothing.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
Bert_like.SFC.create_tokenizer_draft <- function( # nolint
    self,
    sep_token = "[SEP]",
    sep_id = 1,
    cls_token = "[CLS]",
    cls_id = 0,
    unk_token = "[UNK]",
    special_tokens = c("[CLS]", "[SEP]", "[PAD]", "[UNK]", "[MASK]")) {

  self$temp$special_tokens <- special_tokens
  self$temp$tok_new <- create_WordPiece_tokenizer(
    self$params$vocab_do_lower_case,
    sep_token = sep_token,
    sep_id = sep_id,
    cls_token = cls_token,
    cls_id = cls_id,
    unk_token = unk_token
  )

  self$temp$trainer <- tok$trainers$WordPieceTrainer(
    vocab_size = as.integer(self$params$vocab_size),
    special_tokens = self$temp$special_tokens,
    show_progress = self$params$trace
  )
}

#' @title BERT-like creation step `calculate_vocab`
#' @description Relevant only for transformer classes (BERT, DeBERTa, Funnel, etc.). Do not use outside the classes.
#'
#'   This function **uses** `tok_new`, `raw_text_dataset`, `trainer` parameters from the `temp` list.
#'
#'   See private list `steps_for_creation` of [.AIFEBaseTransformer] class for details. This list has the elements
#'   as already defined functions that can add some temporary parameters into the `temp` list of the base class
#'   [.AIFEBaseTransformer] or use these temporary parameters.
#'
#' @param self Transformer `self`-object.
#' @return This function returns nothing.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
Bert_like.SFC.calculate_vocab <- function(self) { # nolint
  run_py_file("datasets_transformer_compute_vocabulary.py")

  self$temp$tok_new$train_from_iterator(
    py$batch_iterator(
      batch_size = as.integer(200),
      dataset = self$temp$raw_text_dataset,
      log_file = self$temp$log_file,
      write_interval = self$params$log_write_interval,
      value_top = self$temp$value_top,
      total_top = self$temp$total_top,
      message_top = self$temp$message_top
    ),
    trainer = self$temp$trainer,
    length = length(self$temp$raw_text_dataset)
  )
}

#' @title BERT-like creation step `save_tokenizer_draft`
#' @description Relevant only for transformer classes (BERT, DeBERTa, Funnel, etc.). Do not use outside the classes.
#'
#'   This function **uses** `special_tokens`, `tok_new` parameters from the `temp` list.
#'
#'   See private list `steps_for_creation` of [.AIFEBaseTransformer] class for details. This list has the elements
#'   as already defined functions that can add some temporary parameters into the `temp` list of the base class
#'   [.AIFEBaseTransformer] or use these temporary parameters.
#'
#' @param self Transformer `self`-object.
#' @return This function returns nothing.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
Bert_like.SFC.save_tokenizer_draft <- function(self) { # nolint
  write(c(self$temp$special_tokens, names(self$temp$tok_new$get_vocab())),
    file = paste0(self$params$model_dir, "/", "vocab.txt")
  )
}

#' @title BERT-like creation steps
#' @description Relevant only for transformer classes (BERT, DeBERTa, Funnel, etc.). Do not use outside the classes.
#'
#'   This function **adds** `tokenizer` parameter into the `temp` list and **uses** from it `tok_new`.
#'
#'   See private list `steps_for_creation` of [.AIFEBaseTransformer] class for details. This list has the elements
#'   as already defined functions that can add some temporary parameters into the `temp` list of the base class
#'   [.AIFEBaseTransformer] or use these temporary parameters.
#'
#' @param self Transformer `self`-object.
#' @return This function returns nothing.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
Bert_like.SFC.create_final_tokenizer <- function(self) { # nolint
  self$temp$tokenizer <- transformers$PreTrainedTokenizerFast(
    tokenizer_object = self$temp$tok_new,
    unk_token = "[UNK]",
    sep_token = "[SEP]",
    pad_token = "[PAD]",
    cls_token = "[CLS]",
    mask_token = "[MASK]",
    bos_token = "[CLS]",
    eos_token = "[SEP]"
  )
}

#' @title Longformer-like creation step `create_tokenizer_draft`
#' @description Relevant only for transformer classes (Longformer, RoBERTa, etc.). Do not use outside the classes.
#'
#'   This function **adds** `tok_new` parameter into the `temp` list.
#'
#'   See private list `steps_for_creation` of [.AIFEBaseTransformer] class for details. This list has the elements
#'   as already defined functions that can add some temporary parameters into the `temp` list of the base class
#'   [.AIFEBaseTransformer] or use these temporary parameters.
#'
#' @param self Transformer `self`-object.
#' @return This function returns nothing.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
Longformer_like.SFC.create_tokenizer_draft <- function(self) { # nolint
  self$temp$tok_new <- create_ByteLevelBPE_tokenizer(
    self$params$max_position_embeddings,
    self$params$add_prefix_space,
    self$params$trim_offsets
  )
}

#' @title Longformer-like creation step `calculate_vocab`
#' @description Relevant only for transformer classes (Longformer, RoBERTa, etc.). Do not use outside the classes.
#'
#'   This function **uses** `tok_new` parameter from the `temp` list.
#'
#'   See private list `steps_for_creation` of [.AIFEBaseTransformer] class for details. This list has the elements
#'   as already defined functions that can add some temporary parameters into the `temp` list of the base class
#'   [.AIFEBaseTransformer] or use these temporary parameters.
#'
#' @param self Transformer `self`-object.
#' @return This function returns nothing.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
Longformer_like.SFC.calculate_vocab <- function(self) { # nolint
  run_py_file("datasets_transformer_compute_vocabulary.py")
  self$temp$tok_new$train_from_iterator(
    py$batch_iterator(
      batch_size = as.integer(200),
      dataset = self$temp$raw_text_dataset,
      log_file = self$temp$log_file,
      write_interval = self$params$log_write_interval,
      value_top = self$temp$value_top,
      total_top = self$temp$total_top,
      message_top = self$temp$message_top
    ),
    length = length(self$temp$raw_text_dataset),
    vocab_size = as.integer(self$params$vocab_size),
    special_tokens = c("<s>", "<pad>", "</s>", "<unk>", "<mask>")
  )
}

#' @title Longformer-like creation step `save_tokenizer_draft`
#' @description Relevant only for transformer classes (Longformer, RoBERTa, etc.). Do not use outside the classes.
#'
#'   This function **uses** `tok_new` parameter from the `temp` list.
#'
#'   See private list `steps_for_creation` of [.AIFEBaseTransformer] class for details. This list has the elements
#'   as already defined functions that can add some temporary parameters into the `temp` list of the base class
#'   [.AIFEBaseTransformer] or use these temporary parameters.
#'
#' @param self Transformer `self`-object.
#' @return This function returns nothing.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
Longformer_like.SFC.save_tokenizer_draft <- function(self) { # nolint
  self$temp$tok_new$save_model(self$params$model_dir)
}

#' @title Dataset tokenization
#' @description A given dataset must contain a column 'text' storing raw texts.
#'
#' @param dataset `datasets.arrow_dataset.Dataset` Dataset that contains a column 'text' storing the raw texts.
#' @param tokenizer `transformers.Tokenizer()` Tokenizer.
#' @param max_length `integer` Max length for a given tokenizer.
#'
#' @return Tokenized dataset with a given tokenizer.
#'
#' @family Defined steps for creation
#' @keywords internal
#' @noRd
tokenize_dataset <- function(dataset, tokenizer, max_length,
                             log_file = NULL, write_interval = 2,
                             value_top = 0, total_top = 1, message_top = "NA") {
  run_py_file("datasets_transformer_prepare_data.py")

  batch_size <- 2L

  tokenized_texts_raw <- dataset$map(
    py$tokenize_raw_text,
    batched = TRUE,
    batch_size = batch_size,
    fn_kwargs = reticulate::dict(
      list(
        tokenizer = tokenizer,
        truncation = TRUE,
        padding = FALSE,
        max_length = as.integer(max_length),
        return_overflowing_tokens = TRUE,
        return_length = TRUE,
        return_special_tokens_mask = TRUE,
        return_offsets_mapping = FALSE,
        return_attention_mask = TRUE,
        return_tensors = "np",
        request_word_ids = TRUE,
        log_file = log_file,
        write_interval = write_interval,
        value_top = value_top, total_top = total_top, message_top = message_top,
        total_middle = floor(dataset$num_rows / batch_size)
      )
    ),
    remove_columns = dataset$column_names
  )
  return(tokenized_texts_raw)
}
