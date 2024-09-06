#' @title Transformer types
#' @description This list contains transformer types. Elements of the list can be used in the public `make` of the
#'   [AIFETransformerMaker] `R6` class as input parameter `type`.
#'
#'   It has the following elements:
#'   `r get_tr_types_list_decsription()`
#'
#'   Elements can be used like `AIFETrType$bert`, `AIFETrType$deberta_v2`, `AIFETrType$funnel`, etc.
#'
#' @family Transformer
#' @export
AIFETrType <- list(
  bert = "bert",
  roberta = "roberta",
  deberta_v2 = "deberta_v2",
  funnel = "funnel",
  longformer = "longformer",
  mpnet = "mpnet"
)

#' @title `R6` class for transformer creation
#' @description This class was developed to make the creation of transformers easier for users. Pass the transformer's
#'   type to the `make` method and get desired transformer. Now run the `create` or/and `train` methods of the new
#'   transformer.
#'
#'   The already created [aife_transformer_maker] object of this class can be used.
#'
#'   See [.AIFEBaseTransformer] class for details.
#'
#' @section Transformer parameters:
#'
#'   ## 'Static' parameters
#'   | Name | Type | Description |
#'   | - | - | - |
#'   | ml_framework | `string` | Framework to use for training and inference<sup>1</sup> |
#'   | sustain_track | `bool` | If `TRUE` energy consumption is tracked during training<sup>2</sup> |
#'   | sustain_iso_code | `string` | ISO code (Alpha-3-Code) for the country<sup>3</sup> |
#'   | sustain_region | `string` | Region within a country. Only available for USA and Canada<sup>4</sup> |
#'   | sustain_interval | `integer` | Interval in seconds for measuring power usage |
#'   | trace | `bool` | `TRUE` if information about the progress should be printed to the console |
#'   | pytorch_safetensors | `bool` | Choose between safetensors and standard pytorch format<sup>5</sup> |
#'
#'   <sup>1 Available frameworks are "tensorflow" and "pytorch"</sup>
#'
#'   <sup>2 Via the python library codecarbon</sup>
#'
#'   <sup>3 This variable must be set if sustainability should be tracked. A list can be found on Wikipedia:
#'   <https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes></sup>
#'
#'   <sup>4 See the documentation of codecarbon for more information
#'   <https://mlco2.github.io/codecarbon/parameters.html></sup>
#'
#'   <sup>5 Only relevant for pytorch models. `TRUE`: a 'pytorch' model is saved in safetensors format; `FALSE`
#'   (or 'safetensors' is not available): model is saved in the standard pytorch format (.bin)</sup>
#'
#'   ## 'Dynamic' parameters for creation
#'   | Name | Type | Description |
#'   | - | - | - |
#'   | model_dir | `string` | Path to the directory where the model should be saved |
#'   | vocab_raw_texts | `vector` | Contains the raw texts for creating the vocabulary |
#'   | vocab_size | `int` | Size of the vocabulary |
#'   | max_position_embeddings | `int` | Number of maximum position embeddings<sup>1</sup> |
#'   | hidden_size | `int` | Number of neurons in each layer<sup>2</sup> |
#'   | hidden_act | `string` | Name of the activation function |
#'   | hidden_dropout_prob | `double` | Ratio of dropout |
#'   | attention_probs_dropout_prob | `double` | Ratio of dropout for attention probabilities |
#'   | intermediate_size | `int` | Number of neurons in the intermediate layer of the attention mechanism |
#'   | num_attention_heads | `int` | Number of attention heads |
#'   | vocab_do_lower_case | `bool` | `TRUE` if all words/tokens should be lower case |
#'   | num_hidden_layer | `int` | Number of hidden layers |
#'   | target_hidden_size | `int` | Number of neurons in the final layer<sup>2</sup> |
#'   | block_sizes | `vector` | Contains `int`s that determine the number and sizes of each block |
#'   | num_decoder_layers | `int` | Number of decoding layers |
#'   | activation_dropout | `float` | Dropout probability between the layers of the feed-forward blocks |
#'   | pooling_type | `string` | Type of pooling<sup>3</sup> |
#'   | add_prefix_space | `bool` | `TRUE` if an additional space should be inserted to the leading words |
#'   | trim_offsets | `bool` | `TRUE` trims the whitespaces from the produced offsets |
#'   | attention_window | `int` | Size of the window around each token for attention mechanism in every layer |
#'
#'   <sup>1 This parameter also determines the maximum length of a sequence which can be processed with the model</sup>
#'
#'   <sup>2 This parameter determines the dimensionality of the resulting text embedding</sup>
#'
#'   <sup>3 `"mean"` and `"max"` for pooling with mean and maximum values respectively</sup>
#'
#'   ## 'Dynamic' parameters for training
#'   | Name | Type | Description |
#'   | - | - | - |
#'   | output_dir | `string` | Path to the directory where the final model should be saved<sup>1</sup> |
#'   | model_dir_path | `string` | Path to the directory where the original model is stored |
#'   | raw_texts | `vector` | Contains the raw texts for training |
#'   | p_mask | `double` | Ratio that determines the number of words/tokens used for masking |
#'   | whole_word | `bool` | Choose a type of masking<sup>2</sup> |
#'   | val_size | `double` | Ratio that determines the amount of token chunks used for validation |
#'   | n_epoch | `int` | Number of epochs for training |
#'   | batch_size | `int` | Size of batches |
#'   | chunk_size | `int` | Size of every chunk for training |
#'   | min_seq_len | `int` | Value determines the minimal sequence length included in training process<sup>3</sup> |
#'   | full_sequences_only | `bool` | `TRUE` for using only chunks with a sequence length equal to `chunk_size` |
#'   | learning_rate | `double` | Learning rate for adam optimizer |
#'   | n_workers | `int` | Number of workers<sup>4</sup> |
#'   | multi_process | `bool` | `TRUE` if multiple processes should be activated<sup>4</sup> |
#'   | keras_trace | `int` | Controls the information about the training process from keras on the console<sup>4,5</sup> |
#'   | pytorch_trace | `int` | Controls the information about the training process from pytorch on the console |
#'
#'   <sup>1 If the directory does not exist, it will be created</sup>
#'
#'   <sup>2 `TRUE`: whole word masking should be applied; `FALSE`: token masking is used</sup>
#'
#'   <sup>3 Only relevant if `full_sequences_only = FALSE`</sup>
#'
#'   <sup>4 Only relevant if `ml_framework = "tensorflow"`</sup>
#'
#'   <sup>5 `keras_trace = 0`: does not print any information; `keras_trace = 1`: prints a progress bar;
#'   `keras_trace = 2`: prints one line of information for every epoch</sup>
#'
#'   <sup>6 `pytorch_trace = 0`: does not print any information; `pytorch_trace = 1`: prints a progress bar</sup>
#'
#' @section Allowed transformers: Using the transformers will be shown in this section.
#'
#'   ## `BERT`
#'   ```r
#'   transformer <- aife_transformer_maker$make(AIFETrType$bert)
#'   ```
#'   ### Create
#'   ```r
#'   transformer$create(ml_framework = ml_framework,
#'                      model_dir = model_dir,
#'                      vocab_raw_texts = NULL,
#'                      vocab_size = 30522,
#'                      vocab_do_lower_case = FALSE,
#'                      max_position_embeddings = 512,
#'                      hidden_size = 768,
#'                      num_hidden_layer = 12,
#'                      num_attention_heads = 12,
#'                      intermediate_size = 3072,
#'                      hidden_act = "gelu",
#'                      hidden_dropout_prob = 0.1,
#'                      attention_probs_dropout_prob = 0.1,
#'                      sustain_track = TRUE,
#'                      sustain_iso_code = NULL,
#'                      sustain_region = NULL,
#'                      sustain_interval = 15,
#'                      trace = TRUE,
#'                      pytorch_safetensors = TRUE)
#'   ```
#'   ### Train
#'   ```r
#'   transformer$train(ml_framework = ml_framework,
#'                     output_dir = output_dir,
#'                     model_dir_path = model_dir_path,
#'                     raw_texts = raw_texts,
#'                     p_mask = 0.15,
#'                     whole_word = TRUE,
#'                     val_size = 0.1,
#'                     n_epoch = 1,
#'                     batch_size = 12,
#'                     chunk_size = 250,
#'                     full_sequences_only = FALSE,
#'                     min_seq_len = 50,
#'                     learning_rate = 3e-3,
#'                     n_workers = 1,
#'                     multi_process = FALSE,
#'                     sustain_track = TRUE,
#'                     sustain_iso_code = NULL,
#'                     sustain_region = NULL,
#'                     sustain_interval = 15,
#'                     trace = TRUE,
#'                     keras_trace = 1,
#'                     pytorch_trace = 1,
#'                     pytorch_safetensors = TRUE)
#'   ```
#'
#'   ## `DeBERTa-v2`
#'   ```r
#'   transformer <- aife_transformer_maker$make(AIFETrType$deberta)
#'   ```
#'   ### Create
#'   ```r
#'   transformer$create(ml_framework = ml_framework,
#'                      model_dir = model_dir,
#'                      vocab_raw_texts = vocab_raw_texts,
#'                      vocab_size = 128100,
#'                      vocab_do_lower_case = FALSE,
#'                      max_position_embeddings = 512,
#'                      hidden_size = 1536,
#'                      num_hidden_layer = 24,
#'                      num_attention_heads = 24,
#'                      intermediate_size = 6144,
#'                      hidden_act = "gelu",
#'                      hidden_dropout_prob = 0.1,
#'                      attention_probs_dropout_prob = 0.1,
#'                      sustain_track = TRUE,
#'                      sustain_iso_code = NULL,
#'                      sustain_region = NULL,
#'                      sustain_interval = 15,
#'                      trace = TRUE,
#'                      pytorch_safetensors = TRUE)
#'   ```
#'   ### Train
#'   ```r
#'   transformer$train(ml_framework = ml_framework,
#'                     output_dir = output_dir,
#'                     model_dir_path = model_dir_path,
#'                     raw_texts = raw_texts,
#'                     p_mask = 0.15,
#'                     whole_word = TRUE,
#'                     val_size = 0.1,
#'                     n_epoch = 1,
#'                     batch_size = 12,
#'                     chunk_size = 250,
#'                     full_sequences_only = FALSE,
#'                     min_seq_len = 50,
#'                     learning_rate = 3e-2,
#'                     n_workers = 1,
#'                     multi_process = FALSE,
#'                     sustain_track = TRUE,
#'                     sustain_iso_code = NULL,
#'                     sustain_region = NULL,
#'                     sustain_interval = 15,
#'                     trace = TRUE,
#'                     keras_trace = 1,
#'                     pytorch_trace = 1,
#'                     pytorch_safetensors = TRUE)
#'   ```
#'
#'   ## `RoBERTa`
#'   ```r
#'   transformer <- aife_transformer_maker$make(AIFETrType$roberta)
#'   ```
#'   ### Create
#'   ```r
#'   transformer$create(ml_framework = ml_framework,
#'                      model_dir = model_dir,
#'                      vocab_raw_texts = vocab_raw_texts,
#'                      vocab_size = 30522,
#'                      add_prefix_space = FALSE,
#'                      trim_offsets = TRUE,
#'                      max_position_embeddings = 512,
#'                      hidden_size = 768,
#'                      num_hidden_layer = 12,
#'                      num_attention_heads = 12,
#'                      intermediate_size = 3072,
#'                      hidden_act = "gelu",
#'                      hidden_dropout_prob = 0.1,
#'                      attention_probs_dropout_prob = 0.1,
#'                      sustain_track = TRUE,
#'                      sustain_iso_code = NULL,
#'                      sustain_region = NULL,
#'                      sustain_interval = 15,
#'                      trace = TRUE,
#'                      pytorch_safetensors = TRUE)
#'   ```
#'   ### Train
#'   ```r
#'   transformer$train(ml_framework = ml_framework,
#'                     output_dir = output_dir,
#'                     model_dir_path = model_dir_path,
#'                     raw_texts = raw_texts,
#'                     p_mask = 0.15,
#'                     val_size = 0.1,
#'                     n_epoch = 1,
#'                     batch_size = 12,
#'                     chunk_size = 250,
#'                     full_sequences_only = FALSE,
#'                     min_seq_len = 50,
#'                     learning_rate = 3e-2,
#'                     n_workers = 1,
#'                     multi_process = FALSE,
#'                     sustain_track = TRUE,
#'                     sustain_iso_code = NULL,
#'                     sustain_region = NULL,
#'                     sustain_interval = 15,
#'                     trace = TRUE,
#'                     keras_trace = 1,
#'                     pytorch_trace = 1,
#'                     pytorch_safetensors = TRUE)
#'   ```
#'
#'   ## `Funnel`
#'   ```r
#'   transformer <- aife_transformer_maker$make(AIFETrType$funnel)
#'   ```
#'   ### Create
#'   ```r
#'   transformer$create(ml_framework = ml_framework,
#'                      model_dir = model_dir,
#'                      vocab_raw_texts = vocab_raw_texts,
#'                      vocab_size = 30522,
#'                      vocab_do_lower_case = FALSE,
#'                      max_position_embeddings = 512,
#'                      hidden_size = 768,
#'                      target_hidden_size = 64,
#'                      block_sizes = c(4, 4, 4),
#'                      num_attention_heads = 12,
#'                      intermediate_size = 3072,
#'                      num_decoder_layers = 2,
#'                      pooling_type = "mean",
#'                      hidden_act = "gelu",
#'                      hidden_dropout_prob = 0.1,
#'                      attention_probs_dropout_prob = 0.1,
#'                      activation_dropout = 0.0,
#'                      sustain_track = TRUE,
#'                      sustain_iso_code = NULL,
#'                      sustain_region = NULL,
#'                      sustain_interval = 15,
#'                      trace = TRUE,
#'                      pytorch_safetensors = TRUE)
#'   ```
#'   ### Train
#'   ```r
#'   transformer$train(ml_framework = ml_framework,
#'                     output_dir = output_dir,
#'                     model_dir_path = model_dir_path,
#'                     raw_texts = raw_texts,
#'                     p_mask = 0.15,
#'                     whole_word = TRUE,
#'                     val_size = 0.1,
#'                     n_epoch = 1,
#'                     batch_size = 12,
#'                     chunk_size = 250,
#'                     full_sequences_only = FALSE,
#'                     min_seq_len = 50,
#'                     learning_rate = 3e-3,
#'                     n_workers = 1,
#'                     multi_process = FALSE,
#'                     sustain_track = TRUE,
#'                     sustain_iso_code = NULL,
#'                     sustain_region = NULL,
#'                     sustain_interval = 15,
#'                     trace = TRUE,
#'                     keras_trace = 1,
#'                     pytorch_trace = 1,
#'                     pytorch_safetensors = TRUE)
#'   ```
#'
#'   ## `Longformer`
#'   ```r
#'   transformer <- aife_transformer_maker$make(AIFETrType$longformer)
#'   ```
#'   ### Create
#'   ```r
#'   transformer$create(ml_framework = ml_framework,
#'                      model_dir = model_dir,
#'                      vocab_raw_texts = NULL,
#'                      vocab_size = 30522,
#'                      add_prefix_space = FALSE,
#'                      trim_offsets = TRUE,
#'                      max_position_embeddings = 512,
#'                      hidden_size = 768,
#'                      num_hidden_layer = 12,
#'                      num_attention_heads = 12,
#'                      intermediate_size = 3072,
#'                      hidden_act = "gelu",
#'                      hidden_dropout_prob = 0.1,
#'                      attention_probs_dropout_prob = 0.1,
#'                      attention_window = 512,
#'                      sustain_track = TRUE,
#'                      sustain_iso_code = NULL,
#'                      sustain_region = NULL,
#'                      sustain_interval = 15,
#'                      trace = TRUE,
#'                      pytorch_safetensors = TRUE)
#'   ```
#'   ### Train
#'   ```r
#'   transformer$train(ml_framework = ml_framework,
#'                     output_dir = output_dir,
#'                     model_dir_path = model_dir_path,
#'                     raw_texts = raw_texts,
#'                     p_mask = 0.15,
#'                     val_size = 0.1,
#'                     n_epoch = 1,
#'                     batch_size = 12,
#'                     chunk_size = 250,
#'                     full_sequences_only = FALSE,
#'                     min_seq_len = 50,
#'                     learning_rate = 3e-2,
#'                     n_workers = 1,
#'                     multi_process = FALSE,
#'                     sustain_track = TRUE,
#'                     sustain_iso_code = NULL,
#'                     sustain_region = NULL,
#'                     sustain_interval = 15,
#'                     trace = TRUE,
#'                     keras_trace = 1,
#'                     pytorch_trace = 1,
#'                     pytorch_safetensors = TRUE)
#'   ```
#'
#' @examples
#' # Create transformer maker
#' tr_maker <- AIFETransformerMaker$new()
#'
#' # Use 'make' method of the 'tr_maker' object
#' # Pass string with the type of transformers
#' # Allowed types are "bert", "deberta_v2", "funnel", etc. See aifeducation::AIFETrType list
#' my_bert <- tr_maker$make("bert")
#'
#' # Or use elements of the 'aifeducation::AIFETrType' list
#' my_longformer <- tr_maker$make(AIFETrType$longformer)
#'
#' # Run 'create' or 'train' methods of the transformer in order to create a
#' # new transformer or train the newly created one, respectively
#' # my_bert$create(...)
#' # my_bert$train(...)
#'
#' # my_longformer$create(...)
#' # my_longformer$train(...)
#'
#' @family Transformer
#' @export
AIFETransformerMaker <- R6::R6Class(
  classname = "AIFETransformerMaker",
  public = list(
    #' @description Creates a new transformer with the passed type.
    #' @param type `string` A type of the new transformer. Allowed types are `r get_allowed_transformer_types()`. See
    #'   [AIFETrType] list.
    #' @return If success - a new transformer, otherwise - an error (passed type is invalid).
    make = function(type) {
      transformer <- NULL
      if (type == AIFETrType$bert) {
        transformer <- .AIFEBertTransformer$new()
      } else if (type == AIFETrType$deberta_v2) {
        transformer <- .AIFEDebertaTransformer$new()
      } else if (type == AIFETrType$funnel) {
        transformer <- .AIFEFunnelTransformer$new()
      } else if (type == AIFETrType$longformer) {
        transformer <- .AIFELongformerTransformer$new()
      } else if (type == AIFETrType$roberta) {
        transformer <- .AIFERobertaTransformer$new()
      } else if (type == AIFETrType$mpnet) {
        transformer <- .AIFEMpnetTransformer$new()
      } else {
        stop(
          paste0(
            "Transformer type '", type, "' is invalid.",
            " Allowed types are: ", get_allowed_transformer_types(), ". "
          )
        )
      }
      return(transformer)
    }
  )
)

#' @title `R6` object of the `AIFETransformerMaker` class
#' @description Object for creating the transformers with different types. See [AIFETransformerMaker] class for
#'   details.
#'
#' @inheritSection AIFETransformerMaker Transformer parameters
#'
#' @inheritSection AIFETransformerMaker Allowed transformers
#'
#' @examples
#' # Use 'make' method of the 'aifeducation::aife_transformer_maker' object
#' # Pass string with the type of transformers
#' # Allowed types are "bert", "deberta_v2", "funnel", etc. See aifeducation::AIFETrType list
#' my_bert <- aife_transformer_maker$make("bert")
#'
#' # Or use elements of the 'aifeducation::AIFETrType' list
#' my_longformer <- aife_transformer_maker$make(AIFETrType$longformer)
#'
#' # Run 'create' or 'train' methods of the transformer in order to create a
#' # new transformer or train the newly created one, respectively
#' # my_bert$create(...)
#' # my_bert$train(...)
#'
#' # my_longformer$create(...)
#' # my_longformer$train(...)
#'
#' @family Transformer
#' @export
aife_transformer_maker <- AIFETransformerMaker$new()
