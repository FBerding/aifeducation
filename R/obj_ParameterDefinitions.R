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
#' @export
get_param_dict <- function() {
  param <- list(
    act_fct=list(
      type="string",
      min=NULL,
      max=NULL,
      allow_null = FALSE,
      allowed_values = c("elu","leakyrelu","relu","gelu","sigmoid","tanh","prelu"),
      desc="Activation function for all layers except recurrent layers.",
      gui_box="General Settings",
      gui_label="Activation Function",
      default_value="elu",
      default_historic="gelu"
    ),
    parametrizations=list(
      type="string",
      min=NULL,
      max=NULL,
      allow_null = FALSE,
      allowed_values = c("None","orthogonal","weight_norm","spectral_norm"),
      desc="Re-Parametrizations for all layers except recurrent layers.",
      gui_box="General Settings",
      gui_label="Re-Parametrization",
      default_value="None",
      default_historic="None"
    ),
    data_embeddings = list(
      type = c("EmbeddedText", "LargeDataSetForTextEmbeddings"),
      allow_null = FALSE,
      desc="Object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings]."
    ),
    data_targets = list(
      type = c("factor"),
      allow_null = FALSE,
      desc="containing the labels for cases stored in embeddings. Factor must be
      named and has to use the same names as used in in the embeddings."
    ),
    data_folds = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="determining the number of cross-fold samples.",
      gui_box="General Settings",
      gui_label="Number of Folds",
      default_value=5
    ),
    data_val_size = list(
      type = c("(double)"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="between 0 and 1, indicating the proportion of cases of each class which should be
      used for the validation sample during the estimation of the model.
      The remaining cases are part of the training data.",
      gui_box="General Settings",
      gui_label="Size of Validation Data Set",
      default_value=0.1
    ),
    balance_class_weights = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="If `TRUE` class weights are generated based on the frequencies of the
      training data with the method Inverse Class Frequency. If `FALSE` each class has the weight 1.",
      gui_box="Loss",
      gui_label="Balance Class Weights",
      default_value=TRUE
    ),
    balance_sequence_length = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="If `TRUE` sample weights are generated for the length of sequences based on
      the frequencies of the training data with the method Inverse Class Frequency.
      If `FALSE` each sequences length has the weight 1.",
      gui_box="Loss",
      gui_label="Balance Sequence Length",
      default_value=TRUE
    ),
    use_sc = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="`TRUE` if the estimation should integrate synthetic cases. `FALSE` if not.",
      gui_box="Synthetic Cases",
      gui_label="Use Synthetic Cases",
      default_value=FALSE
    ),
    sc_method = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = c("knnor"),
      desc="containing the method for generating synthetic cases.",
      gui_box="Synthetic Cases",
      gui_label="Method for Creating Synthetic Cases",
      default_value="knnor"
    ),
    sc_min_k = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="determining the minimal number of k which is used for creating synthetic units.",
      gui_box="Synthetic Cases",
      gui_label="Min k",
      default_value=1
    ),
    sc_max_k = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="determining the maximal number of k which is used for creating synthetic units.",
      gui_box="Synthetic Cases",
      gui_label="Max k",
      default_value=1
    ),
    use_pl = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="`TRUE` if the estimation should integrate pseudo-labeling. `FALSE` if not.",
      gui_box="Pseudo Labeling",
      gui_label="Use Pseudo Labeling",
      default_value=FALSE
    ),
    pl_max_steps = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="determining the maximum number of steps during pseudo-labeling.",
      gui_box="Pseudo Labeling",
      gui_label="Number of Steps",
      default_value=5
    ),
    pl_anchor=list(
      type="double",
      min=0,
      max=1,
      allow_null=FALSE,
      allowed_values=NULL,
      desc="indicating the reference point for sorting the new cases of every label.",
      gui_box="Pseudo Labeling",
      gui_label="Certainty Anchor Value",
      default_value=1
    ),
    pl_max = list(
      type = c("(double"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="setting the maximal level of confidence for considering a case for pseudo-labeling.",
      gui_box="Pseudo Labeling",
      gui_label="Max Certainty",
      default_value=1
    ),
    pl_min = list(
      type = c("double)"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="setting the mnimal level of confidence for considering a case for pseudo-labeling.",
      gui_box="Pseudo Labeling",
      gui_label="Min Certainty",
      default_value=0
    ),
    sustain_track = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="If `TRUE` energy consumption is tracked during training via the python library 'codecarbon'.",
      gui_box=NULL,
      default_value=TRUE

    ),
    sustain_iso_code = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = NULL,
      desc="ISO code (Alpha-3-Code) for the country. This variable must be set if
      sustainability should be tracked. A list can be found on Wikipedia:
      [https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes].",
      gui_box="Sustainability",
      gui_label="Alpha-3-Code",
      default_value="DEU"
    ),
    sustain_region = list(
      type = c("string"),
      allow_null = TRUE,
      allowed_values = NULL,
      desc="Region within a country. Only available for USA and Canada See the documentation of
      codecarbon for more information. [https://mlco2.github.io/codecarbon/parameters.html]",
      gui_box=NULL,
      default_value=NULL
    ),
    sustain_interval = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Interval in seconds for measuring power usage.",
      gui_box=NULL,
      default_value=15
    ),
    epochs = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Number of training epochs.",
      gui_box="General Settings",
      gui_label="Epochs",
      default_value=100
    ),
    batch_size = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Size of the batches for training.",
      gui_box="General Settings",
      gui_label="Batch Size",
      default_value=32
    ),
    dir_checkpoint = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = NULL,
      desc="Path to the directory where the checkpoint during training should be saved.
      If the directory does not exist, it is created."
    ),
    log_dir = list(
      type = c("string"),
      allow_null = TRUE,
      allowed_values = NULL,
      desc="Path to the directory where the log files should be saved.
      If no logging is desired set this argument to `NULL`.",
      default_value=NULL
    ),
    log_write_interval = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Time in seconds determining the interval in which the logger should try to update
      the log files. Only relevant if `log_dir` is not `NULL`.",
      default_value=60
    ),
    trace = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="`TRUE` if information about the estimation phase should be printed to the console.",
      default_value=FALSE
    ),
    ml_trace=list(
      type="int",
      min=0,
      max=1,
      allow_null=FALSE,
      allowed_values=NULL,
      desc="`ml_trace=0` does not print any information about the training process from pytorch on the console.",
      default_value=0
    ),
    n_cores = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Number of cores which should be used during the calculation of synthetic cases. Only relevant if `use_sc=TRUE`.",
      default_value=1
    ),
    lr_rate = list(
      type = c("(double"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="Initial learning rate for the training.",
      magnitude=0.1,
      gui_box="Learning Rate",
      gui_label="Learning Rate",
      default_value=1e-3
    ),
    lr_warm_up_ratio = list(
      type = c("(double)"),
      allow_null = FALSE,
      min = 0,
      max = .50,
      desc="Number of epochs used for warm up.",
      gui_box="Learning Rate",
      gui_label="Warm Up Ratio",
      default_value=0.01
    ),
    embedding_dim=list(
      desc=" determining the number of dimensions for the text embedding.",
      type="int",
      max=Inf,
      min=1,
      allow_null=FALSE,
      gui_box="General Settings",
      gui_label="Number of Dimensions for Embeddings",
      default_value=2
    ),
    Ns = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc="Number of cases for every class in the sample.",
      gui_label="Number of Cases in the Sample",
      gui_box="Sampling",
      default_value=5
    ),
    Nq = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc="Number of cases for every class in the query.",
      gui_label="Number of Cases in the Query",
      gui_box="Sampling",
      default_value=3
    ),
    loss_alpha = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc="Value between 0 and 1 indicating how strong the loss should focus on pulling cases to
      its corresponding prototypes or pushing cases away from other prototypes. The higher the value the more the
      loss concentrates on pulling cases to its corresponding prototypes.",
      gui_box="Loss",
      gui_label="Alpha",
      default_value=0.6
    ),
    loss_margin = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc="Value greater 0 indicating the minimal distance of every case from prototypes of other classes.",
      gui_box="Loss",
      gui_label="Margin",
      default_value=0.5
    ),
    sampling_separate = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc="If `TRUE` the cases for every class are divided into a data set for sample and
      for query. These are never mixed. If `TRUE` sample and query cases are drawn from the same data pool. That is,
      a case can be part of sample in one epoch and in another epoch it can be part of query. It is ensured that a
      case is never part of sample and query at the same time. In addition, it is ensured that every cases exists
      only once during a training step.",
      gui_box="Sampling",
      gui_label="Strictly Separte Sample and Query",
      default_value=FALSE
    ),
    sampling_shuffle = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc="if `TRUE` cases a randomly drawn from the data during every step. If `FALSE` the
      cases are not shuffled.",
      gui_box="Sampling",
      gui_label="Shuffle Order of Cases",
      default_value=TRUE
    ),
    features = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc="Number of features the model should use.",
      gui_box="General Settings",
      gui_label="Number of Features",
      default_value=64
    ),
    method = list(
      type = "string",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = c("dense", "lstm"),
      desc="Method to use for the feature extraction. `'lstm'` for an extractor based on LSTM-layers or `'dense'` for dense layers.",
      default_value="dense",
      gui_box="General Settings",
      gui_label="Method"
    ),
    noise_factor = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc="Value between 0 and a value lower 1 indicating how much noise should
      be added to the input during training.",
      default_value=1e-2,
      gui_box="General Settings",
      gui_label="Noise Factor"
    ),
    one_hot_encoding = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc="If `TRUE` all labels are converted to one hot encoding.",
      default_value=NULL
    ),
    add_matrix_map = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc="If `TRUE` all embeddings are transformed into a two dimensional matrix.
      The number of rows equals the number of cases. The number of columns equals `times*features`.",
      default_value=NULL

    )
  )
  param$val_size <- param$data_val_size
  param$folds <-  param$data_folds
  param$sc_methods=param$sc_method

  param$name <- list(
    type = "string",
    allow_null = TRUE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="Name of the new model. Please refer to common name conventions.
    Free text can be used with parameter `label`. If set to `NULL` a unique ID
    is generated automatically.",
    default_value=NULL
  )

  param$label <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="Label for the new model. Here you can use free text.",
    default_value=NULL
  )

  param$text_embeddings <- param$data_embeddings

  param$feature_extractor <- list(
    type = "TEFeatureExtractor",
    allow_null = TRUE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="Object of class [TEFeatureExtractor] which should be used in order to reduce
    the number of dimensions of the text embeddings. If no feature extractor should be applied set `NULL`.",
    gui_label="Feature Extractor",
    default_value=NULL
  )

  param$target_levels <- list(
    type = "vector",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="containing the levels (categories or classes) within the target data. Please
    note that order matters. For ordinal data please ensure that the levels are sorted correctly with later levels
    indicating a higher category/class. For nominal data the order does not matter.",
    gui_label="Target Levels",
    default_value=NULL
  )
  param$class_levels <- param$target_levels

  param$bias <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="If `TRUE` a bias term is added to all layers. If `FALSE` no bias term is added to the layers.",
    gui_box="General Settings",
    gui_label="Add Bias",
    default_value=FALSE,
    default_historic=TRUE
  )

  param$dense_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc="Number of neurons for each dense layer.",
    gui_box="Dense Layers",
    gui_label="Size",
    default_value=32
  )

  param$dense_layers <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="Number of dense layers.",
    gui_box="Dense Layers",
    gui_label="Number of Layers",
    default_value=0
  )

  param$rec_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc="Number of neurons for each recurrent layer.",
    gui_box="Recurrent Layers",
    gui_label="Size",
    default_value=32
  )

  param$rec_layers <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="Number of recurrent layers.",
    gui_box="Recurrent Layers",
    gui_label="Number of Layers",
    default_value=1
  )

  param$rec_type <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("gru", "lstm"),
    desc="Type of the recurrent layers. `rec_type='gru'` for Gated Recurrent Unit and `rec_type='lstm'` for Long Short-Term Memory.",
    gui_box="Recurrent Layers",
    gui_label="Type",
    default_value="gru"
  )

  param$rec_bidirectional <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="If `TRUE` a bidirectional version of the recurrent layers is used.",
    gui_box="Recurrent Layers",
    gui_label="Bidirectional",
    default_value=FALSE
  )

  param$self_attention_heads <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="determining the number of attention heads for a self-attention layer. Only relevant if `attention_type='multihead'`",
    gui_box="Encoder Layers",
    gui_label="Number of Attention Heads",
    default_value=2
  )

  param$intermediate_size <- list(
    type = "int",
    allow_null = TRUE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc="determining the size of the projection layer within a each transformer encoder.",
    gui_box="Encoder Layers",
    gui_label="Intermediate Size",
    default_value=128
  )

  param$attention_type <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("fourier", "multihead"),
    desc="Choose the relevant attention type. Please note that you may see different
    values for a case for different input orders if you choose `fourier` on linux.",
    gui_box="Encoder Layers",
    gui_label="Attention Type",
    default_value="fourier"
  )

  param$add_pos_embedding <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="`TRUE` if positional embedding should be used.",
    gui_box="Encoder Layers",
    gui_label="Add Positional Embedding",
    default_value=FALSE
  )

  param$dense_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc="determining the dropout between dense layers.",
    gui_box="Dense Layers",
    gui_label="Dropout",
    default_value=0.5
  )

  param$rec_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc="determining the dropout between recurrent layers.",
    gui_box="Recurrent Layers",
    gui_label="Dropout",
    default_value=0.5
  )

  param$encoder_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc="determining the dropout for the dense projection within the encoder layers.",
    gui_box="Encoder Layers",
    gui_label="Dropout",
    default_value=0.1
  )

  param$repeat_encoder <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="determining how many times the encoder should be added to the network.",
    gui_box="Encoder Layers",
    gui_label="Number of Layers",
    default_value=0
  )

  param$optimizer <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("adam", "rmsprop", "adamw"),
    desc="determining the optimizer used for training.",
    gui_box="General Settings",
    gui_label="Optimizer",
    default_value="adamw"
  )

  return(param)
}

#' @family Parameter Dictionary
#' @export
get_param_def <- function(param_name) {
  return(get_param_dict()[[param_name]])
}

#' @family Parameter Dictionary
#' @export
get_param_doc_desc=function(param_name){
  param_def=get_param_def(param_name=param_name)

  if(length(param_def$type)==sum(param_def$type %in% c("bool", "int", "double", "(double", "double)", "(double)", "string", "vector", "list"))){
    is_class=TRUE
  } else {
    is_class=FALSE
  }

  desc=param_def$desc

  if(is_class==TRUE){
    type=paste0("`",param_def$type,"`")
    type=stringi::stri_replace_all(
      str=type,replacement = "",
      regex = "\\(|\\)"
    )
    if(param_def$type=="bool"){
      allowed_values=NULL
    } else if(param_def$type=="string"){
      if(is.null(param_def$allowed_values)){
        allowed_values="any"
      } else {
        allowed_values=paste(paste0("'",param_def$allowed_values,"'"),collapse = ", ")
      }
    } else if (param_def$type%in%c("double", "(double", "double)", "(double)")){
      if(param_def$min!=-Inf){
        if(param_def$type=="(double"|param_def$type=="(double)"){
          border_min=paste(param_def$min,"<")
        } else {
          border_min=paste(param_def$min,"<=")
        }
      } else {
        border_min=NULL
      }
      if(param_def$max!=Inf){
        if(param_def$type=="double)"|param_def$type=="(double)"){
          border_max=paste("<",param_def$max)
        } else {
          border_max=paste("<=",param_def$max)
        }
      } else {
        border_max=NULL
      }
      allowed_values=paste0("`",border_min," x ",border_max,"`")
    } else if(param_def$type=="int"){
      if(param_def$min!=-Inf){
        border_min=paste(param_def$min,"<=")
      } else {
        border_min=NULL
      }
      if(param_def$max!=Inf){
        border_max=paste("<=",param_def$max)
      } else {
        border_max=NULL
      }
      allowed_values=paste0("`",border_min," x ",border_max,"`")
    } else {
      allowed_values=NULL
    }
  } else {
    type=paste0("`",paste0(param_def$type,collapse = ", "),"`")
    allowed_values=NULL
  }

  if(!is.null(allowed_values)){
    allowed_values=paste("Allowed values:",allowed_values)
  } else {
    allowed_values=""
  }

  desc_string=paste0(
    type," ",
    desc," ",
    allowed_values
  )
return(desc_string)
}

#' @title Called arguments
#' @description Function for receiving all arguments that were called by a method or function.
#'
#'@param n `int` level of the nested environments where to extract the arguments.
#'
#' @importFrom rlang caller_fn
#' @importFrom rlang fn_fmls
#'
#' @return Returns names `list` of all arguments and their values.
#'
#' @family Utils
#' @export
get_called_args=function(n=1){
  fn=rlang::caller_fn(n)
  formal_args=rlang::fn_fmls(fn)
  final_args=formal_args
  for (arguments in names(formal_args)) {
    final_args[arguments]=list(get(x=arguments,envir = rlang::caller_env(n)))
  }
  return(final_args)
}

get_magnitude_values=function(min,max,magnitude,n_elements=9){
  value_vector=vector(length = n_elements)
  for(i in seq_along(value_vector)){
    value_vector[i]=max*magnitude^i
  }
  return(value_vector)
}
