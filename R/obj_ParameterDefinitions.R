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


#' @family Parameter Dictionary
#' @keywords internal
TEClassifiers_class_names <- c()

#' @family Parameter Dictionary
#' @export
get_TEClassifiers_class_names <- function(super_class = NULL) {
  if (is.null(super_class)) {
    return(TEClassifiers_class_names)
  } else {
    class_names <- NULL
    for (class in TEClassifiers_class_names) {
      object <- create_object(class)
      if (super_class %in% class(object)) {
        class_names <- append(
          x = class_names,
          values = class
        )
      }
    }
    return(class_names)
  }
}


#' @family Parameter Dictionary
#' @export
get_param_dict <- function() {
  param <- list()
  #General---------------------------------------------------------------------
  param$name <- list(
    type = "string",
    allow_null = TRUE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "Name of the new model. Please refer to common name conventions.
    Free text can be used with parameter `label`. If set to `NULL` a unique ID
    is generated automatically.",
    default_value = NULL
  )

  param$label <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "Label for the new model. Here you can use free text.",
    default_value = NULL
  )
  #Transformer related---------------------------------------------------------
  param$pad_value=list(
      type = "int",
      min = -Inf,
      max = -100,
      allow_null = FALSE,
      allowed_values = NULL,
      desc = "Value indicating padding. This value should no be in the range of
      regluar values for computations. Thus it is not recommended to chance this value.
      Default is `-100`.",
      gui_box = NULL,
      gui_label = NULL,
      default_value = -100,
      default_historic = 0)
  param$param_pad_value=param$pad_value

  # Transformer related parameters
  param$pytorch_safetensors <- list(
    type = "bool",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "
  * `TRUE`: a 'pytorch' model is saved in safetensors format.
  * `FALSE` (or 'safetensors' is not available): model is saved in the standard pytorch format (.bin). ",
    gui_box = NULL,
    gui_label = NULL,
    default_value = TRUE
  )

  param$model_dir <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "Path to the directory where the model should be saved.",
    gui_box = NULL,
    gui_label = NULL,
    default_value = NULL
  )
  param$output_dir <- param$model_dir

  param$model_dir_path <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "Path to the directory where the original model is stored.",
    gui_box = NULL,
    gui_label = NULL,
    default_value = NULL
  )

  param$text_dataset <- list(
    type = c("LargeDataSetForText"),
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "[LargeDataSetForText] Object storing textual data.",
    gui_box = NULL,
    gui_label = "Text Collection",
    default_value = NULL
  )

  param$vocab_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1000,
    max = 500000,
    allowed_values = NULL,
    desc = "Size of the vocabulary.",
    gui_box = "Vocabulary",
    gui_label = "Vocabulary Size",
    default_value = 30000
  )

  param$vocab_do_lower_case <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "`TRUE` if all words/tokens should be lower case.",
    gui_box = "Vocabulary",
    gui_label = "Lower Case",
    default_value = FALSE
  )

  param$add_prefix_space <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "`TRUE` if an additional space should be inserted to the leading words.",
    gui_box = "Vocabulary",
    gui_label = "Add Prefix Space",
    default_value = FALSE
  )

  param$trim_offsets <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "`TRUE` trims the whitespaces from the produced offsets.",
    gui_box = "Vocabulary",
    gui_label = "Trim Offsets",
    default_value = FALSE
  )

  param$whole_word <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "* `TRUE`: whole word masking should be applied.
  * `FALSE`: token masking is used. ",
    gui_box = "Training Settings",
    gui_label = "Whole Word Masking",
    default_value = TRUE
  )

  param$full_sequences_only <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "`TRUE` for using only chunks with a sequence length equal to `chunk_size`.",
    gui_box = "Sequence Modeling",
    gui_label = "Full Sequences Only",
    default_value = FALSE
  )

  param$max_position_embeddings <- list(
    type = "int",
    allow_null = FALSE,
    min = 10,
    max = 4048,
    allowed_values = NULL,
    desc = "Number of maximum position embeddings. This parameter also determines the maximum length of a sequence which
  can be processed with the model.",
    gui_box = "Sequence Modeling",
    gui_label = "Max Sequence Length",
    default_value = 512
  )

  param$attention_window <- list(
    type = "int",
    allow_null = FALSE,
    min = 2,
    max = Inf,
    allowed_values = NULL,
    desc = "Size of the window around each token for attention mechanism in every layer.",
    gui_box = "Sequence Modeling",
    gui_label = "Attention Window",
    default_value = 512
  )

  param$hidden_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = 2048,
    allowed_values = NULL,
    desc = "Number of neurons in each layer. This parameter determines the dimensionality of the resulting text
  embedding.",
    gui_box = "Encoder Layers",
    gui_label = "Hidden Size",
    default_value = 768
  )

  param$hidden_act <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("gelu", "relu","silu","gelu_new"),
    desc = "Name of the activation function.",
    gui_box = "Encoder Layers",
    gui_label = "Hidden Activation Function",
    default_value = "gelu"
  )

  param$num_hidden_layer <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of hidden layers.",
    gui_box = "Encoder Layers",
    gui_label = "Number of Hidden Layers",
    default_value = 7
  )

  param$num_decoder_layers <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of decoding layers.",
    gui_box = "Decoder Layers",
    gui_label = "Number of Decoding Layers",
    default_value = 7
  )

  param$target_hidden_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of neurons in the final layer. This parameter determines the dimensionality of the resulting text
  embedding.",
    gui_box = "Encoder Layers",
    gui_label = "Target Hidden Size",
    default_value = 768
  )

  param$hidden_dropout_prob <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "Ratio of dropout.",
    gui_box = "Encoder Layers",
    gui_label = "Hidden Dropout",
    default_value = 0.5
  )

  param$activation_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "Dropout probability between the layers of the feed-forward blocks.",
    gui_box = "Encoder Layers",
    gui_label = "Dropout between the layers of the feed-forward blocks",
    default_value = 0.5
  )

  param$attention_probs_dropout_prob <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "Ratio of dropout for attention probabilities.",
    gui_box = "Encoder Layers",
    gui_label = "Dropout for Attention Probabilities",
    default_value = 0.1
  )

  param$p_mask <- list(
    type = "(double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "Ratio that determines the number of words/tokens used for masking.",
    gui_box = "Sequence Modeling",
    gui_label = "Masking Probability",
    default_value = 0.4
  )

  param$p_perm<- list(
    type = "(double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "Ratio that determines the number of words/tokens used for permutation.",
    gui_box = "Sequence Modeling",
    gui_label = "Permutation Probability",
    default_value = 0.4
  )

  param$block_sizes <- list(
    type = "vector",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "`vector` of `int` determining the number and sizes of each block.",
    gui_box = "Encoder Layers",
    gui_label = "Block Size",
    default_value = 0.1
  )

  param$pooling_type <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("mean", "max"),
    desc = "Type of pooling.
  * `\"mean\"` for pooling with mean.
  * `\"max\"` for pooling with maximum values. ",
    gui_box = "Encoder Layers",
    gui_label = "Pooling Type",
    default_value = "max"
  )

  param$chunk_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "Size of every chunk for training. ",
    gui_box = "Sequence Modeling",
    gui_label = "Size of every Chunk",
    default_value = 4
  )

  param$min_seq_len <- list(
    type = "int",
    allow_null = FALSE,
    min = 10,
    max = Inf,
    allowed_values = NULL,
    desc = "Only relevant if `full_sequences_only = FALSE`. Value determines the minimal sequence length included in
  training process. ",
    gui_box = "Sequence Modeling",
    gui_label = "Min Sequence Length",
    default_value = 10
  )

  #Data related-----------------------------------------------------------------
  param$data_embeddings = list(
      type = c("EmbeddedText", "LargeDataSetForTextEmbeddings"),
      allow_null = FALSE,
      desc = "Object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings]."
    )
  param$text_embeddings <- param$data_embeddings

  param$data_targets = list(
      type = c("factor"),
      allow_null = FALSE,
      desc = "containing the labels for cases stored in embeddings. Factor must be
      named and has to use the same names as used in in the embeddings."
    )
  param$target_levels <- list(
    type = "vector",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "containing the levels (categories or classes) within the target data. Please
    note that order matters. For ordinal data please ensure that the levels are sorted correctly with later levels
    indicating a higher category/class. For nominal data the order does not matter.",
    gui_label = "Target Levels",
    default_value = NULL
  )
  param$class_levels <- param$target_levels

  param$data_folds = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "determining the number of cross-fold samples.",
      gui_box = "General Settings",
      gui_label = "Number of Folds",
      default_value = 5
    )
  param$folds <- param$data_folds

  param$data_val_size = list(
      type = c("(double)"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc = "between 0 and 1, indicating the proportion of cases which should be
      used for the validation sample during the estimation of the model.
      The remaining cases are part of the training data.",
      gui_box = "General Settings",
      gui_label = "Size of Validation Data Set",
      default_value = 0.1
    )
  param$val_size <- param$data_val_size

  param$balance_class_weights = list(
      type = c("bool"),
      allow_null = FALSE,
      desc = "If `TRUE` class weights are generated based on the frequencies of the
      training data with the method Inverse Class Frequency. If `FALSE` each class has the weight 1.",
      gui_box = "Loss",
      gui_label = "Balance Class Weights",
      default_value = TRUE
    )
  param$balance_sequence_length = list(
      type = c("bool"),
      allow_null = FALSE,
      desc = "If `TRUE` sample weights are generated for the length of sequences based on
      the frequencies of the training data with the method Inverse Class Frequency.
      If `FALSE` each sequences length has the weight 1.",
      gui_box = "Loss",
      gui_label = "Balance Sequence Length",
      default_value = TRUE
    )
  param$one_hot_encoding = list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "If `TRUE` all labels are converted to one hot encoding.",
    default_value = NULL
  )
  param$add_matrix_map = list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "If `TRUE` all embeddings are transformed into a two dimensional matrix.
      The number of rows equals the number of cases. The number of columns equals `times*features`.",
    default_value = NULL
  )

  # Synthetic cases-------------------------------------------------------------
  param$use_sc = list(
      type = c("bool"),
      allow_null = FALSE,
      desc = "`TRUE` if the estimation should integrate synthetic cases. `FALSE` if not.",
      gui_box = "Synthetic Cases",
      gui_label = "Use Synthetic Cases",
      default_value = FALSE
    )
  param$sc_method = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = c("knnor"),
      desc = "containing the method for generating synthetic cases.",
      gui_box = "Synthetic Cases",
      gui_label = "Method for Creating Synthetic Cases",
      default_value = "knnor"
    )
  param$sc_methods <- param$sc_method

  param$sc_min_k = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "determining the minimal number of k which is used for creating synthetic units.",
      gui_box = "Synthetic Cases",
      gui_label = "Min k",
      default_value = 1
    )
  param$sc_max_k = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "determining the maximal number of k which is used for creating synthetic units.",
      gui_box = "Synthetic Cases",
      gui_label = "Max k",
      default_value = 1
    )

  param$n_cores = list(
    type = c("int"),
    allow_null = FALSE,
    min = 1,
    max = Inf,
    desc = "Number of cores which should be used during the calculation of synthetic cases. Only relevant if `use_sc=TRUE`.",
    default_value = 1
  )

  # Pseudo labeling------------------------------------------------------------
  param$use_pl = list(
      type = c("bool"),
      allow_null = FALSE,
      desc = "`TRUE` if the estimation should integrate pseudo-labeling. `FALSE` if not.",
      gui_box = "Pseudo Labeling",
      gui_label = "Use Pseudo Labeling",
      default_value = FALSE
    )
  param$pl_max_steps = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "determining the maximum number of steps during pseudo-labeling.",
      gui_box = "Pseudo Labeling",
      gui_label = "Number of Steps",
      default_value = 5
    )
  param$pl_anchor = list(
      type = "double",
      min = 0,
      max = 1,
      allow_null = FALSE,
      allowed_values = NULL,
      desc = "indicating the reference point for sorting the new cases of every label.",
      gui_box = "Pseudo Labeling",
      gui_label = "Certainty Anchor Value",
      default_value = 1
    )
  param$pl_max = list(
      type = c("(double"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc = "setting the maximal level of confidence for considering a case for pseudo-labeling.",
      gui_box = "Pseudo Labeling",
      gui_label = "Max Certainty",
      default_value = 1
    )
  param$pl_min = list(
      type = c("double)"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc = "setting the mnimal level of confidence for considering a case for pseudo-labeling.",
      gui_box = "Pseudo Labeling",
      gui_label = "Min Certainty",
      default_value = 0
    )

  #Sustainability--------------------------------------------------------------
  param$sustain_track = list(
      type = c("bool"),
      allow_null = FALSE,
      desc = "If `TRUE` energy consumption is tracked during training via the python library 'codecarbon'.",
      gui_box = NULL,
      default_value = TRUE
    )
  param$sustain_iso_code = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = NULL,
      desc = "ISO code (Alpha-3-Code) for the country. This variable must be set if
      sustainability should be tracked. A list can be found on Wikipedia:
      [https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes].",
      gui_box = "Sustainability",
      gui_label = "Alpha-3-Code",
      default_value = "DEU"
    )
  param$sustain_region = list(
      type = c("string"),
      allow_null = TRUE,
      allowed_values = NULL,
      desc = "Region within a country. Only available for USA and Canada See the documentation of
      codecarbon for more information. [https://mlco2.github.io/codecarbon/parameters.html]",
      gui_box = NULL,
      default_value = NULL
    )
  param$sustain_interval = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "Interval in seconds for measuring power usage.",
      gui_box = NULL,
      default_value = 15
    )

  #Training related------------------------------------------------------------
  param$cls_loss_fct_name<- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("FocalLoss", "CrossEntropyLoss"),
    desc = "Name of the loss function to use during training.",
    gui_box = "General Settings",
    gui_label = "Loss Function",
    default_value = "FocalLoss"
  )
  param$pt_loss_fct_name= param$cls_loss_fct_name
  param$pt_loss_fct_name$allowed_values=c("MultiWayContrastiveLoss")
  param$pt_loss_fct_name$default_value=c("MultiWayContrastiveLoss")
  param$pt_loss_fct_name$gui_box = "General Settings"

  param$optimizer <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("adam", "rmsprop", "adamw","sgd"),
    desc = "determining the optimizer used for training.",
    gui_box = "General Settings",
    gui_label = "Optimizer",
    default_value = "adamw"
  )

  param$epochs = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "Number of training epochs.",
      gui_box = "General Settings",
      gui_label = "Epochs",
      default_value = 100
    )
  param$n_epoch <- param$epochs

  param$batch_size = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "Size of the batches for training.",
      gui_box = "General Settings",
      gui_label = "Batch Size",
      default_value = 32
    )
  param$lr_rate = list(
    type = c("(double"),
    allow_null = FALSE,
    min = 0,
    max = 1,
    desc = "Initial learning rate for the training.",
    magnitude = 0.1,
    gui_box = "Learning Rate",
    gui_label = "Learning Rate",
    default_value = 1e-3
  )
  param$learning_rate <- param$lr_rate

  param$lr_warm_up_ratio = list(
    type = c("(double)"),
    allow_null = FALSE,
    min = 0,
    max = .50,
    desc = "Number of epochs used for warm up.",
    gui_box = "Learning Rate",
    gui_label = "Warm Up Ratio",
    default_value = 0.01
  )
  param$dir_checkpoint = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = NULL,
      desc = "Path to the directory where the checkpoint during training should be saved.
      If the directory does not exist, it is created."
    )

  #Logging related-------------------------------------------------------------
  param$log_dir = list(
      type = c("string"),
      allow_null = TRUE,
      allowed_values = NULL,
      desc = "Path to the directory where the log files should be saved.
      If no logging is desired set this argument to `NULL`.",
      default_value = NULL
    )
  param$log_write_interval = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc = "Time in seconds determining the interval in which the logger should try to update
      the log files. Only relevant if `log_dir` is not `NULL`.",
      default_value = 60
    )
  param$trace = list(
      type = c("bool"),
      allow_null = FALSE,
      desc = "`TRUE` if information about the estimation phase should be printed to the console.",
      default_value = FALSE
    )
  param$ml_trace = list(
      type = "int",
      min = 0,
      max = 1,
      allow_null = FALSE,
      allowed_values = NULL,
      desc = "`ml_trace=0` does not print any information about the training process from pytorch on the console.",
      default_value = 0
    )
  param$pytorch_trace <- param$ml_trace

  #Meta Learning related--------------------------------------------------------
  param$embedding_dim = list(
      desc = " determining the number of dimensions for the embedding.",
      type = "int",
      max = Inf,
      min = 2,
      allow_null = FALSE,
      gui_box = "General Settings",
      gui_label = "Number of Dimensions for Embeddings",
      default_value = 2
    )
  param$Ns = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc = "Number of cases for every class in the sample.",
      gui_label = "Number of Cases in the Sample",
      gui_box = "Sampling",
      default_value = 5
    )
  param$Nq = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc = "Number of cases for every class in the query.",
      gui_label = "Number of Cases in the Query",
      gui_box = "Sampling",
      default_value = 3
    )
  param$loss_alpha = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc = "Value between 0 and 1 indicating how strong the loss should focus on pulling cases to
      its corresponding prototypes or pushing cases away from other prototypes. The higher the value the more the
      loss concentrates on pulling cases to its corresponding prototypes.",
      gui_box = "Loss",
      gui_label = "Alpha",
      default_value = 0.6
    )
  param$loss_margin = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc = "Value greater 0 indicating the minimal distance of every case from prototypes of other classes.",
      gui_box = "Loss",
      gui_label = "Margin",
      default_value = 0.05
    )
  param$sampling_separate = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc = "If `TRUE` the cases for every class are divided into a data set for sample and
      for query. These are never mixed. If `TRUE` sample and query cases are drawn from the same data pool. That is,
      a case can be part of sample in one epoch and in another epoch it can be part of query. It is ensured that a
      case is never part of sample and query at the same time. In addition, it is ensured that every cases exists
      only once during a training step.",
      gui_box = "Sampling",
      gui_label = "Strictly Separte Sample and Query",
      default_value = FALSE
    )
  param$sampling_shuffle = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc = "if `TRUE` cases a randomly drawn from the data during every step. If `FALSE` the
      cases are not shuffled.",
      gui_box = "Sampling",
      gui_label = "Shuffle Order of Cases",
      default_value = TRUE
    )

  #FeatureExtractor related-----------------------------------------------------
  param$features = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc = "Number of features the model should use.",
      gui_box = "General Settings",
      gui_label = "Number of Features",
      default_value = 64
    )
  param$method = list(
      type = "string",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = c("dense", "lstm"),
      desc = "Method to use for the feature extraction. `'lstm'` for an extractor based on LSTM-layers or `'dense'` for dense layers.",
      default_value = "dense",
      gui_box = "General Settings",
      gui_label = "Method"
  )
  param$noise_factor = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc = "Value between 0 and a value lower 1 indicating how much noise should
      be added to the input during training.",
      default_value = 1e-2,
      gui_box = "General Settings",
      gui_label = "Noise Factor"
    )
  param$feature_extractor <- list(
    type = "TEFeatureExtractor",
    allow_null = TRUE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "Object of class [TEFeatureExtractor] which should be used in order to reduce
    the number of dimensions of the text embeddings. If no feature extractor should be applied set `NULL`.",
    gui_label = "Feature Extractor",
    default_value = NULL
  )

  #Layer configuration=========================================================
  # Global settings------------------------------------------------------------
  param$residual_type = list(
    type = "string",
    min = NULL,
    max = NULL,
    allow_null = FALSE,
    allowed_values = c("residual_gate", "addition","None"),
    desc = "Type residual connenction for all layers and stack of layers.",
    gui_box = "General Settings",
    gui_label = "Residual Connection",
    default_value = "residual_gate",
    default_historic = NULL
  )

  param$normalization_type = list(
    type = "string",
    min = NULL,
    max = NULL,
    allow_null = FALSE,
    allowed_values = c("layer_norm", "None"),
    desc = "Type of normalization applied to all layers and stack layers.",
    gui_box = "General Settings",
    gui_label = "Residual Connection",
    default_value = "layer_norm",
    default_historic = NULL
  )

  #Intermediate Feature--------------------------------------------------------
  param$intermediate_features<-list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of features to be extracted at the end of the model.",
    gui_box = "Intermediate Features",
    gui_label = "Size",
    default_value = 32
  )

  param$cls_pooling_type = list(
    type = "string",
    min = NULL,
    max = NULL,
    allow_null = FALSE,
    allowed_values = c("max", "min", "min_max"),
    desc = "Type of extracting intermediate features.",
    gui_box = "Intermediate Features",
    gui_label = "Feature Extraction Method",
    default_value = "min_max",
    default_historic = NULL
  )


  # Parametrizations------------------------------------------------------------
  param$parametrizations = list(
    type = "string",
    min = NULL,
    max = NULL,
    allow_null = FALSE,
    allowed_values = c("None", "orthogonal", "weight_norm", "spectral_norm"),
    desc = "Re-Parametrizations for all layers except recurrent layers.",
    gui_box = "General Settings",
    gui_label = "Re-Parametrization",
    default_value = "None",
    default_historic = "None"
  )
  param$rec_parametrizations=param$parametrizations
  param$rec_parametrizations$gui_box="Recurrent Layers"

  param$tf_parametrizations=param$parametrizations
  param$tf_parametrizations$gui_box="Transformer Encoder Layers"

  param$dense_parametrizations=param$parametrizations
  param$dense_parametrizations$gui_box="Dense Layers"

  param$conv_parametrizations=param$parametrizations
  param$conv_parametrizations$gui_box="N-Gram Layers"

  param$feat_parametrizations=param$parametrizations
  param$feat_parametrizations$gui_box="Intermediate Features"

  #Bias------------------------------------------------------------------------
  param$bias <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "If `TRUE` a bias term is added to all layers. If `FALSE` no bias term is added to the layers.",
    gui_box = "General Settings",
    gui_label = "Add Bias",
    default_value = FALSE,
    default_historic = TRUE
  )
  param$rec_bias=param$bias
  param$rec_bias$gui_box="Recurrent Layers"
  param$tf_bias=param$bias
  param$tf_bias$gui_box="Transformer Encoder Layers"
  param$dense_bias=param$bias
  param$dense_bias$gui_box="Dense Layers"
  param$conv_bias=param$bias
  param$conv_bias$gui_box="N-Gram Layers"
  param$feat_bias=param$bias
  param$feat_bias$gui_box="Intermediate Features"

  #Activation functions---------------------------------------------------------
  param$act_fct = list(
    type = "string",
    min = NULL,
    max = NULL,
    allow_null = FALSE,
    allowed_values = c("elu", "leakyrelu", "relu", "gelu", "sigmoid", "tanh", "prelu"),
    desc = "Activation function for all layers except recurrent layers.",
    gui_box = "General Settings",
    gui_label = "Activation Function",
    default_value = "elu",
    default_historic = "gelu"
  )
  param$feat_act_fct=param$act_fct
  param$feat_act_fct$gui_box="Intermediate Features"
  param$conv_act_fct=param$act_fct
  param$conv_act_fct$gui_box="N-Gram Layers"
  param$dense_act_fct=param$act_fct
  param$dense_act_fct$gui_box="Dense Layers"
  param$rec_act_fct=param$act_fct
  param$rec_act_fct$gui_box="Recurrent Layers"
  param$tf_act_fct=param$act_fct
  param$tf_act_fct$gui_box="Transformer Encoder Layers"

  #Recurrent Layer--------------------------------------------------------------
  param$rec_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "determining the dropout between recurrent layers.",
    gui_box = "Recurrent Layers",
    gui_label = "Dropout",
    default_value = 0.5
  )

  param$rec_type <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("gru", "lstm"),
    desc = "Type of the recurrent layers. `rec_type='gru'` for Gated Recurrent Unit and `rec_type='lstm'` for Long Short-Term Memory.",
    gui_box = "Recurrent Layers",
    gui_label = "Type",
    default_value = "gru"
  )

  param$rec_bidirectional <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "If `TRUE` a bidirectional version of the recurrent layers is used.",
    gui_box = "Recurrent Layers",
    gui_label = "Bidirectional",
    default_value = FALSE
  )

  param$rec_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of neurons for each recurrent layer.",
    gui_box = "Recurrent Layers",
    gui_label = "Size",
    default_value = 32
  )

  param$rec_layers <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of recurrent layers.",
    gui_box = "Recurrent Layers",
    gui_label = "Number of Layers",
    default_value = 1
  )
  param$rec_n_layers=param$rec_layers

  #Dense Layer------------------------------------------------------------------
  param$dense_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "determining the dropout between dense layers.",
    gui_box = "Dense Layers",
    gui_label = "Dropout",
    default_value = 0.5
  )
  param$dense_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of neurons for each dense layer.",
    gui_box = "Dense Layers",
    gui_label = "Size",
    default_value = 32
  )

  param$dense_layers <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc = "Number of dense layers.",
    gui_box = "Dense Layers",
    gui_label = "Number of Layers",
    default_value = 0
  )
  param$dense_n_layers=param$dense_layers

  #Scaling Layer----------------------------------------------------------------
  param$feat_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "determining the dropout for the dense projection of the scaling layer.",
    gui_box = "Scaling Layer",
    gui_label = "Dropout",
    default_value = 0.1
  )

  param$feat_size=param$dense_size
  param$feat_size$min=2

  #Transformer Layer------------------------------------------------------------
  param$encoder_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc = "determining the dropout for the dense projection within the transformer encoder layers.",
    gui_box = "Transformer Encoder Layers",
    gui_label = "Dense Dropout",
    default_value = 0.1
  )
  param$tf_dropout_rate_1=param$encoder_dropout
  param$tf_dropout_rate_1$desc="determining the dropout after the attention mechanism within the transformer encoder layers."
  param$tf_dropout_rate_1$gui_label="Attention Dropout"
  param$tf_dropout_rate_2=param$encoder_dropout

  param$self_attention_heads <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc = "determining the number of attention heads for a self-attention layer. Only relevant if `attention_type='multihead'`",
    gui_box = "Transformer Encoder Layers",
    gui_label = "Number of Attention Heads",
    default_value = 2
  )
  param$num_attention_heads <- param$self_attention_head
  param$tf_num_heads=param$self_attention_head

  param$intermediate_size <- list(
    type = "int",
    allow_null = TRUE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc = "determining the size of the projection layer within a each transformer encoder.",
    gui_box = "Transformer Encoder Layers",
    gui_label = "Intermediate Size",
    default_value = 128
  )
  param$tf_dense_dim=param$intermediate_size

  param$attention_type <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("fourier", "multihead"),
    desc = "Choose the relevant attention type. Please note that you may see different
    values for a case for different input orders if you choose `fourier` on linux.",
    gui_box = "Transformer Encoder Layers",
    gui_label = "Attention Type",
    default_value = "fourier"
  )
  param$tf_attention_type=param$attention_type

  param$add_pos_embedding <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc = "`TRUE` if positional embedding should be used.",
    gui_box = "Transformer Encoder Layers",
    gui_label = "Add Positional Embedding",
    default_value = FALSE
  )

  param$tf_embedding_type=list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("absolute"),
    desc = "Type of processing positional information.",
    gui_box = "Transformer Encoder Layers",
    gui_label = "Positional Information",
    default_value = "absolute"
  )

  param$repeat_encoder <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc = "determining how many times the encoder should be added to the network.",
    gui_box = "Transformer Encoder Layers",
    gui_label = "Number of Layers",
    default_value = 0
  )
  param$tf_n_layers=param$repeat_encoder

  #Conv Layer-------------------------------------------------------------------
  param$conv_n_layers<- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc = "determining how many times the n-gram layers should be added to the network.",
    gui_box = "N-Gram Layers",
    gui_label = "Number of Layers",
    default_value = 0
  )
  param$conv_ks_min<- list(
    type = "int",
    allow_null = FALSE,
    min = 2,
    max = Inf,
    allowed_values = NULL,
    desc = "determining the minimal window size for n-grams.",
    gui_box = "N-Gram Layers",
    gui_label = "Smallest N-Gram",
    default_value = 2
  )
  param$conv_ks_max<- list(
    type = "int",
    allow_null = FALSE,
    min = 2,
    max = Inf,
    allowed_values = NULL,
    desc = "determining the maximal window size for n-grams.",
    gui_box = "N-Gram Layers",
    gui_label = "Biggest N-Gram",
    default_value = 2
  )

  #Parallel specific-----------------------------------------------------------
  param$merge_attention_type=param$attention_type
  param$merge_attention_type$gui_box = "Merge Layer"

  param$merge_num_heads=param$self_attention_head
  param$merge_num_heads$gui_box = "Merge Layer"

  #Prototpye specific----------------------------------------------------------
  param$metric_type=list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("euclidean"),
    desc = "Type of metric used for calculating the distance.",
    gui_box = "Loss",
    gui_label = "Metric Type",
    default_value = "euclidean"
  )

  #============================================================================
  return(param)
}

#' @family Parameter Dictionary
#' @export
get_param_def <- function(param_name) {
  return(get_param_dict()[[param_name]])
}

#' @family Parameter Dictionary
#' @export
get_param_doc_desc <- function(param_name) {
  param_def <- get_param_def(param_name = param_name)

  if (length(param_def$type) == sum(param_def$type %in% c("bool", "int", "double", "(double", "double)", "(double)", "string", "vector", "list"))) {
    is_class <- TRUE
  } else {
    is_class <- FALSE
  }

  desc <- param_def$desc

  if (is_class == TRUE) {
    type <- paste0("`", param_def$type, "`")
    type <- stringi::stri_replace_all(
      str = type, replacement = "",
      regex = "\\(|\\)"
    )
    if (param_def$type == "bool") {
      allowed_values <- NULL
    } else if (param_def$type == "string") {
      if (is.null(param_def$allowed_values)) {
        allowed_values <- "any"
      } else {
        allowed_values <- paste(paste0("'", param_def$allowed_values, "'"), collapse = ", ")
      }
    } else if (param_def$type %in% c("double", "(double", "double)", "(double)")) {
      if (param_def$min != -Inf) {
        if (param_def$type == "(double" | param_def$type == "(double)") {
          border_min <- paste(param_def$min, "<")
        } else {
          border_min <- paste(param_def$min, "<=")
        }
      } else {
        border_min <- NULL
      }
      if (param_def$max != Inf) {
        if (param_def$type == "double)" | param_def$type == "(double)") {
          border_max <- paste("<", param_def$max)
        } else {
          border_max <- paste("<=", param_def$max)
        }
      } else {
        border_max <- NULL
      }
      allowed_values <- paste0("`", border_min, " x ", border_max, "`")
    } else if (param_def$type == "int") {
      if (param_def$min != -Inf) {
        border_min <- paste(param_def$min, "<=")
      } else {
        border_min <- NULL
      }
      if (param_def$max != Inf) {
        border_max <- paste("<=", param_def$max)
      } else {
        border_max <- NULL
      }
      allowed_values <- paste0("`", border_min, " x ", border_max, "`")
    } else {
      allowed_values <- NULL
    }
  } else {
    type <- paste0("`", paste0(param_def$type, collapse = ", "), "`")
    allowed_values <- NULL
  }

  if (!is.null(allowed_values)) {
    allowed_values <- paste("Allowed values:", allowed_values)
  } else {
    allowed_values <- ""
  }

  desc_string <- paste0(
    type, " ",
    desc, " ",
    allowed_values
  )
  return(desc_string)
}

#' @title Called arguments
#' @description Function for receiving all arguments that were called by a method or function.
#'
#' @param n `int` level of the nested environments where to extract the arguments.
#'
#' @importFrom rlang caller_fn
#' @importFrom rlang fn_fmls
#'
#' @return Returns names `list` of all arguments and their values.
#'
#' @family Parameter Dictionary
#' @export
get_called_args <- function(n = 1) {
  fn <- rlang::caller_fn(n)
  formal_args <- rlang::fn_fmls(fn)
  final_args <- formal_args
  for (arguments in names(formal_args)) {
    final_args[arguments] <- list(get(x = arguments, envir = rlang::caller_env(n)))
  }
  return(final_args)
}

get_magnitude_values <- function(min, max, magnitude, n_elements = 9) {
  value_vector <- vector(length = n_elements)
  for (i in seq_along(value_vector)) {
    value_vector[i] <- max * magnitude^i
  }
  return(value_vector)
}
