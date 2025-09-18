#' @title Check `max_position_embeddings` argument of transformer
#' @description Used when creating and training transformers.
#'
#' @param max_position_embeddings `r get_param_doc_desc("max_position_embeddings")`
#' @return Warning if `max_position_embeddings` greater than 512.
#'
#' @family Utils Transformers Developers
#' @keywords internal
#' @noRd
check.max_position_embeddings <- function(max_position_embeddings) { # nolint
  if (max_position_embeddings > 512L) {
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
#' @param hidden_act `r get_param_doc_desc("hidden_act")`
#' @return Error if `hidden_act` is not `"GELU"`, `"ReLU"`, `"silu"` or `"gelu_new"`.
#'
#' @family Utils Transformers Developers
#' @keywords internal
#' @noRd
check.hidden_act <- function(hidden_act) { # nolint
  if (!(hidden_act %in% c("gelu", "relu", "silu", "gelu_new")) ) {
    stop("hidden_act must be gelu, relu, silu or gelu_new")
  }
}

#' @title Check `sustain_iso_code` argument of transformer
#' @description Used when creating and training transformers.
#'
#' @param sustain_iso_code `r `r get_param_doc_desc("sustain_iso_code")`()`
#' @param sustain_track `r `r get_param_doc_desc("sustain_track")`()`
#' @return Error if `sustain_track` is `TRUE` and `sustain_iso_code` is missing (`NULL`).
#'
#' @family Utils Transformers Developers
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
#' @param pytorch_safetensors `r get_param_doc_desc("pytorch_safetensors")`
#' @return Whether to save the model using `safetensors` or the traditional `pytorch` way.
#'
#' @importFrom reticulate py_module_available
#'
#' @family Utils Transformers Developers
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
#' @param model_dir_path `r get_param_doc_desc("model_dir_path")`
#' @return A list with the variables `from_tf` and `load_safe`.
#'
#' @importFrom reticulate py_module_available
#'
#' @family Utils Transformers Developers
#' @keywords internal
#' @noRd
check.model_files <- function(model_dir_path) { # nolint
  bin_exists <- file.exists(file.path(model_dir_path, "pytorch_model.bin"))
  safetensors_exists <- file.exists(file.path(model_dir_path, "model.safetensors"))
  h5_exists <- file.exists(file.path(model_dir_path, "tf_model.h5"))

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
#' @param vocab_do_lower_case `r get_param_doc_desc("vocab_do_lower_case")`
#' @param sep_token `string` Representation of the SEP token.
#' @param sep_id `int` ID of the SEP token.
#' @param cls_token `string` Representation of the CLS token.
#' @param cls_id `int` ID of the CLS token.
#' @param unk_token `string` Representation of the UNK token.
#' @return A new tokenizer object (`tokenizers.Tokenizer`) based on `tokenizers.models.WordPiece` model.
#'
#' @importFrom reticulate tuple
#'
#' @family Utils Transformers Developers
#' @keywords internal
#' @noRd
create_WordPiece_tokenizer <- function(
    # nolint
  vocab_do_lower_case,
  sep_token = "[SEP]",
  sep_id = 1L,
  cls_token = "[CLS]",
  cls_id = 0L,
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
#' @param max_position_embeddings `r get_param_doc_desc("max_position_embeddings")`
#' @param add_prefix_space `r get_param_doc_desc("add_prefix_space")`
#' @param trim_offsets `r get_param_doc_desc("trim_offsets")`
#' @return A new tokenizer object (`tokenizers.Tokenizer`) based on `tokenizers.models.ByteLevel` model.
#'
#' @family Utils Transformers Developers
#' @keywords internal
#' @noRd
create_ByteLevelBPE_tokenizer <- function(
    # nolint
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
