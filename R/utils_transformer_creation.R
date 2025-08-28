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
#' @family Utils Transformers Creation Developers
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
#' @family Utils Transformers Creation Developers
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
#' @family Utils Transformers Creation Developers
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
#' @family Utils Transformers Creation Developers
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
#' @family Utils Transformers Creation Developers
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
#' @family Utils Transformers Creation Developers
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
#' @family Utils Transformers Creation Developers
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
#' @family Utils Transformers Creation Developers
#' @keywords internal
#' @noRd
tokenize_dataset <- function(dataset, tokenizer, max_length,add_special_tokens=TRUE,
                             log_file = NULL, write_interval = 2,
                             value_top = 0, total_top = 1, message_top = "NA") {
  run_py_file("datasets_transformer_prepare_data.py")

  batch_size <- 2L

  id=as.character(generate_id(16))

  tokenized_texts_raw <- dataset$map(
    py$tokenize_raw_text,
    batched = TRUE,
    batch_size = batch_size,
    load_from_cache_file=FALSE,
    keep_in_memory=FALSE,
    cache_file_name=paste0(create_and_get_tmp_dir(),"/",id),
    new_fingerprint=id,
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
        total_middle = floor(dataset$num_rows / batch_size),
        add_special_tokens=add_special_tokens
      )
    ),
    remove_columns = dataset$column_names
  )
  return(tokenized_texts_raw)
}

