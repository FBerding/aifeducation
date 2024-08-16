#' @title Function for creating a new transformer based on Longformer
#' @description This function is deprecated. Please use instead `aife_transformer_maker$make("longformer")$create()`.
#'
#'   See \link{AIFETransformerMaker} (\link{aife_transformer_maker}), \link{.AIFELongformerTransformer} classes for
#'   details.
#'
#' @param ml_framework `r paramDesc.ml_framework()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_region `r paramDesc.sustain_region()`
#' @param sustain_interval `r paramDesc.sustain_interval()`
#' @param trace `r paramDesc.trace()`
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
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
#' @param attention_window `int` Size of the window around each token for attention mechanism in every layer.
#'
#' @return This function does not return an object. Instead the configuration and the vocabulary of the new model are
#'   saved on disk.
#'
#' @family Deprecated transformer
#' @export
create_longformer_model <- function(
    ml_framework = aifeducation_config$get_framework()[1],
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
    attention_window = 512,
    sustain_track = TRUE,
    sustain_iso_code = NULL,
    sustain_region = NULL,
    sustain_interval = 15,
    trace = TRUE,
    pytorch_safetensors = TRUE) {
  .Deprecated("aife_transformer_maker$make(\"longformer\")$create()")

  aife_transformer_maker$make("longformer")$create(
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
    attention_window = attention_window,
    sustain_track = sustain_track,
    sustain_iso_code = sustain_iso_code,
    sustain_region = sustain_region,
    sustain_interval = sustain_interval,
    trace = trace,
    pytorch_safetensors = pytorch_safetensors
  )
}

#' @title Function for training and fine-tuning a Longformer model
#' @description This function is deprecated. Please use instead `aife_transformer_maker$make("longformer")$train()`.
#'
#'   See \link{AIFETransformerMaker} (\link{aife_transformer_maker}), \link{.AIFELongformerTransformer} classes for
#'   details.
#'
#' @param ml_framework `r paramDesc.ml_framework()`
#' @param sustain_track `r paramDesc.sustain_track()`
#' @param sustain_iso_code `r paramDesc.sustain_iso_code()`
#' @param sustain_region `r paramDesc.sustain_region()`
#' @param sustain_interval `r paramDesc.sustain_interval()`
#' @param trace `r paramDesc.trace()`
#' @param pytorch_safetensors `r paramDesc.pytorch_safetensors()`
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
#' @return This function does not return an object. Instead the trained or fine-tuned model is saved to disk.
#'
#' @family Deprecated transformer
#' @export
train_tune_longformer_model <- function(
    ml_framework = aifeducation_config$get_framework()[1],
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
  .Deprecated("aife_transformer_maker$make(\"longformer\")$train()")

  aife_transformer_maker$make("longformer")$train(
    ml_framework = ml_framework,
    output_dir = output_dir,
    model_dir_path = model_dir_path,
    raw_texts = raw_texts,
    p_mask = p_mask,
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
