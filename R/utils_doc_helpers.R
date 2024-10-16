# Parameters' descriptions ----

#' @keywords internal
paramDesc.ml_framework <- function() {
  " `string` Framework to use for training and inference.
   * `ml_framework = \"tensorflow\"`: for 'tensorflow'.
   * `ml_framework = \"pytorch\"`: for 'pytorch'. "
}
#' @keywords internal
paramDesc.sustain_track <- function() {
  " `bool` If `TRUE` energy consumption is tracked during training via the python library codecarbon. "
}
#' @keywords internal
paramDesc.sustain_iso_code <- function() {
  " `string` ISO code (Alpha-3-Code) for the country. This variable must be set if sustainability should be tracked. A
  list can be found on Wikipedia: <https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes>. "
}
#' @keywords internal
paramDesc.sustain_region <- function() {
  " `string` Region within a country. Only available for USA and Canada. See the documentation of codecarbon for more
  information <https://mlco2.github.io/codecarbon/parameters.html>. "
}
#' @keywords internal
paramDesc.sustain_interval <- function() " `integer` Interval in seconds for measuring power usage. "
#' @keywords internal
paramDesc.trace <- function() {
  " `bool` `TRUE` if information about the progress should be printed to the console. "
}
#' @keywords internal
paramDesc.pytorch_safetensors <- function() {
  " `bool` Only relevant for pytorch models.
  * `TRUE`: a 'pytorch' model is saved in safetensors format.
  * `FALSE` (or 'safetensors' is not available): model is saved in the standard pytorch format (.bin). "
}
#' @keywords internal
paramDesc.log_dir <- function() " Path to the directory where the log files should be saved. "

# Create ----

#' @keywords internal
paramDesc.model_dir <- function() " `string` Path to the directory where the model should be saved. "
#' @keywords internal
paramDesc.vocab_raw_texts <- function() " `vector` containing the raw texts for creating the vocabulary. "
#' @keywords internal
paramDesc.vocab_size <- function() " `int` Size of the vocabulary. "
#' @keywords internal
paramDesc.max_position_embeddings <- function() {
  " `int` Number of maximum position embeddings. This parameter also determines the maximum length of a sequence which
  can be processed with the model. "
}
#' @keywords internal
paramDesc.hidden_size <- function() {
  " `int` Number of neurons in each layer. This parameter determines the dimensionality of the resulting text
  embedding. "
}
#' @keywords internal
paramDesc.hidden_act <- function() " `string` Name of the activation function. "
#' @keywords internal
paramDesc.hidden_dropout_prob <- function() " `double` Ratio of dropout. "
#' @keywords internal
paramDesc.attention_probs_dropout_prob <- function() " `double` Ratio of dropout for attention probabilities. "
#' @keywords internal
paramDesc.intermediate_size <- function() {
  " `int` Number of neurons in the intermediate layer of the attention mechanism. "
}
#' @keywords internal
paramDesc.num_attention_heads <- function() " `int` Number of attention heads. "

# Dependent parameters ----

#' @keywords internal
paramDesc.vocab_do_lower_case <- function() " `bool` `TRUE` if all words/tokens should be lower case. "
#' @keywords internal
paramDesc.num_hidden_layer <- function() " `int` Number of hidden layers. "
#' @keywords internal
paramDesc.target_hidden_size <- function() {
  " `int` Number of neurons in the final layer. This parameter determines the dimensionality of the resulting text
  embedding. "
}
#' @keywords internal
paramDesc.block_sizes <- function() " `vector` of `int` determining the number and sizes of each block. "
#' @keywords internal
paramDesc.num_decoder_layers <- function() " `int` Number of decoding layers. "
#' @keywords internal
paramDesc.activation_dropout <- function() {
  " `float` Dropout probability between the layers of the feed-forward blocks. "
}
#' @keywords internal
paramDesc.pooling_type <- function() {
  " `string` Type of pooling.
  * `\"mean\"` for pooling with mean.
  * `\"max\"` for pooling with maximum values. "
}
#' @keywords internal
paramDesc.add_prefix_space <- function() {
  " `bool` `TRUE` if an additional space should be inserted to the leading words. "
}
#' @keywords internal
paramDesc.trim_offsets <- function() " `bool` `TRUE` trims the whitespaces from the produced offsets. "
#' @keywords internal
paramDesc.attention_window <- function() {
  " `int` Size of the window around each token for attention mechanism in every layer. "
}


# Train ----

#' @keywords internal
paramDesc.output_dir <- function() {
  " `string` Path to the directory where the final model should be saved. If the directory does not exist, it will be
  created. "
}
#' @keywords internal
paramDesc.model_dir_path <- function() " `string` Path to the directory where the original model is stored. "
#' @keywords internal
paramDesc.raw_texts <- function() " `vector` containing the raw texts for training. "
#' @keywords internal
paramDesc.p_mask <- function() " `double` Ratio that determines the number of words/tokens used for masking. "
#' @keywords internal
paramDesc.whole_word <- function() {
  " `bool`
  * `TRUE`: whole word masking should be applied.
  * `FALSE`: token masking is used. "
}
#' @keywords internal
paramDesc.val_size <- function() " `double` Ratio that determines the amount of token chunks used for validation. "
#' @keywords internal
paramDesc.n_epoch <- function() " `int` Number of epochs for training. "
#' @keywords internal
paramDesc.batch_size <- function() " `int` Size of batches. "
#' @keywords internal
paramDesc.chunk_size <- function() " `int` Size of every chunk for training. "
#' @keywords internal
paramDesc.min_seq_len <- function() {
  " `int` Only relevant if `full_sequences_only = FALSE`. Value determines the minimal sequence length included in
  training process. "
}
#' @keywords internal
paramDesc.full_sequences_only <- function() {
  " `bool` `TRUE` for using only chunks with a sequence length equal to `chunk_size`. "
}
#' @keywords internal
paramDesc.learning_rate <- function() " `double` Learning rate for adam optimizer. "
#' @keywords internal
paramDesc.n_workers <- function() " `int` Number of workers. Only relevant if `ml_framework = \"tensorflow\"`. "
#' @keywords internal
paramDesc.multi_process <- function() {
  " `bool` `TRUE` if multiple processes should be activated. Only relevant if `ml_framework = \"tensorflow\"`. "
}
#' @keywords internal
paramDesc.keras_trace <- function() {
  " `int`
  * `keras_trace = 0`: does not print any information about the training process from keras on the console.
  * `keras_trace = 1`: prints a progress bar.
  * `keras_trace = 2`: prints one line of information for every epoch. Only relevant if `ml_framework = \"tensorflow\"`.
  "
}
#' @keywords internal
paramDesc.pytorch_trace <- function() {
  " `int`
  * `pytorch_trace = 0`: does not print any information about the training process from pytorch on the console.
  * `pytorch_trace = 1`: prints a progress bar. "
}

# Transformer types ----

#' @keywords internal
get_allowed_transformer_types <- function(in_quotation_marks = FALSE) {
  res_str <- ""
  if (in_quotation_marks) {
    for (i in 1:length(AIFETrType)) {
      tr_name <- names(AIFETrType)[i]
      if (i != 1) res_str <- paste0(res_str, ", ")
      res_str <- paste0(res_str, "'", tr_name, "'")
    }
  } else {
    res_str <- paste(unname(AIFETrType), collapse = ", ")
  }
  return(res_str)
}
#' @keywords internal
get_tr_types_list_decsription <- function() {
  list_description <- ""
  for (i in 1:length(AIFETrType)) {
    tr_name <- names(AIFETrType)[i]
    list_element <- paste0("* `", tr_name, "` = '", tr_name, "'")
    list_description <- paste0(list_description, "\n", list_element)
  }
  return(list_description)
}
