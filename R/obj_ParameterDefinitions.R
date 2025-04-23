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
      desc="Activation function for all layers except recurrent layers."
    ),
    parametrizations=list(
      type="string",
      min=NULL,
      max=NULL,
      allow_null = FALSE,
      allowed_values = c("None","orthogonal","weight_norm","spectral_norm"),
      desc="Re-Parametrizations for all layers except recurrent layers."
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
      desc="determining the number of cross-fold samples."
    ),
    data_val_size = list(
      type = c("(double)"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="between 0 and 1, indicating the proportion of cases of each class which should be
      used for the validation sample during the estimation of the model.
      The remaining cases are part of the training data."
    ),
    balance_class_weights = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="If `TRUE` class weights are generated based on the frequencies of the
      training data with the method Inverse Class Frequency. If `FALSE` each class has the weight 1."
    ),
    balance_sequence_length = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="If `TRUE` sample weights are generated for the length of sequences based on
      the frequencies of the training data with the method Inverse Class Frequency.
      If `FALSE` each sequences length has the weight 1."
    ),
    use_sc = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="`TRUE` if the estimation should integrate synthetic cases. `FALSE` if not."
    ),
    sc_method = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = c("knnor"),
      desc="containing the method for generating synthetic cases."
    ),
    sc_min_k = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="determining the minimal number of k which is used for creating synthetic units."
    ),
    sc_max_k = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="determining the maximal number of k which is used for creating synthetic units."
    ),
    use_pl = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="`TRUE` if the estimation should integrate pseudo-labeling. `FALSE` if not."
    ),
    pl_max_steps = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="determining the maximum number of steps during pseudo-labeling."
    ),
    pl_anchor=list(
      type="double",
      min=0,
      max=1,
      allow_null=FALSE,
      allowed_values=NULL,
      desc="indicating the reference point for sorting the new cases of every label."
    ),
    pl_max = list(
      type = c("(double"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="setting the maximal level of confidence for considering a case for pseudo-labeling."
    ),
    pl_min = list(
      type = c("double)"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="setting the mnimal level of confidence for considering a case for pseudo-labeling."
    ),
    sustain_track = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="If `TRUE` energy consumption is tracked during training via the python library 'codecarbon'."
    ),
    sustain_iso_code = list(
      type = c("string"),
      allow_null = FALSE,
      allowed_values = NULL,
      desc="ISO code (Alpha-3-Code) for the country. This variable must be set if
      sustainability should be tracked. A list can be found on Wikipedia:
      [https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes]."
    ),
    sustain_region = list(
      type = c("string"),
      allow_null = TRUE,
      allowed_values = NULL,
      desc="Region within a country. Only available for USA and Canada See the documentation of
      codecarbon for more information. [https://mlco2.github.io/codecarbon/parameters.html]"
    ),
    sustain_interval = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Interval in seconds for measuring power usage."
    ),
    epochs = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Number of training epochs."
    ),
    batch_size = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Size of the batches for training."
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
      If no logging is desired set this argument to `NULL`."
    ),
    log_write_interval = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Time in seconds determining the interval in which the logger should try to update
      the log files. Only relevant if `log_dir` is not `NULL`."
    ),
    trace = list(
      type = c("bool"),
      allow_null = FALSE,
      desc="`TRUE` if information about the estimation phase should be printed to the console."
    ),
    ml_trace=list(
      type="int",
      min=0,
      max=1,
      allow_null=FALSE,
      allowed_values=NULL,
      desc="`ml_trace=0` does not print any information about the training process from pytorch on the console."
    ),
    n_cores = list(
      type = c("int"),
      allow_null = FALSE,
      min = 1,
      max = Inf,
      desc="Number of cores which should be used during the calculation of synthetic cases. Only relevant if `use_sc=TRUE`."
    ),
    lr_rate = list(
      type = c("(double"),
      allow_null = FALSE,
      min = 0,
      max = Inf,
      desc="Initial learning rate for the training."
    ),
    lr_warm_up_ratio = list(
      type = c("(double)"),
      allow_null = FALSE,
      min = 0,
      max = 1,
      desc="Number of epochs used for warm up."
    ),
    embedding_dim=list(
      desc=" determining the number of dimensions for the text embedding.",
      type="int",
      max=Inf,
      min=1,
      allow_null=FALSE
    ),
    Ns = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc="Number of cases for every class in the sample."
    ),
    Nq = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc="Number of cases for every class in the query."
    ),
    loss_alpha = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc="Value between 0 and 1 indicating how strong the loss should focus on pulling cases to
      its corresponding prototypes or pushing cases away from other prototypes. The higher the value the more the
      loss concentrates on pulling cases to its corresponding prototypes."
    ),
    loss_margin = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc="Value greater 0 indicating the minimal distance of every case from prototypes of other classes."
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
      only once during a training step."
    ),
    sampling_shuffle = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc="if `TRUE` cases a randomly drawn from the data during every step. If `FALSE` the
      cases are not shuffled."
    ),
    features = list(
      type = "int",
      allow_null = FALSE,
      min = 1,
      max = Inf,
      allowed_values = NULL,
      desc="Number of features the model should use."
    ),
    method = list(
      type = "string",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = c("dense", "lstm"),
      desc="Method to use for the feature extraction. `'lstm'` for an extractor based on LSTM-layers or `'dense'` for dense layers."
    ),
    noise_factor = list(
      type = "double",
      allow_null = FALSE,
      min = 0,
      max = 1,
      allowed_values = NULL,
      desc="Value between 0 and a value lower 1 indicating how much noise should
      be added to the input during training."
    ),
    one_hot_encoding = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc="If `TRUE` all labels are converted to one hot encoding."
    ),
    add_matrix_map = list(
      type = "bool",
      allow_null = FALSE,
      min = NULL,
      max = NULL,
      allowed_values = NULL,
      desc="If `TRUE` all embeddings are transformed into a two dimensional matrix.
      The number of rows equals the number of cases. The number of columns equals `times*features`."
    )
  )
  param$val_size <- param$data_val_size
  param$folds <-  param$data_folds
  param$sc_methods=param$sc_method

  param$name <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="Name of the new model. Please refer to common name conventions.
    Free text can be used with parameter `label`."
  )

  param$label <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="Label for the new model. Here you can use free text."
  )

  param$text_embeddings <- param$data_embeddings

  param$feature_extractor <- list(
    type = "TEFeatureExtractor",
    allow_null = TRUE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="Object of class [TEFeatureExtractor] which should be used in order to reduce
    the number of dimensions of the text embeddings. If no feature extractor should be applied set `NULL`."
  )

  param$target_levels <- list(
    type = "vector",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="containing the levels (categories or classes) within the target data. Please
    note that order matters. For ordinal data please ensure that the levels are sorted correctly with later levels
    indicating a higher category/class. For nominal data the order does not matter."
  )
  param$class_levels <- param$target_levels

  param$bias <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="If `TRUE` a bias term is added to all layers. If `FALSE` no bias term is added to the layers."
  )

  param$dense_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc="Number of neurons for each dense layer."
  )

  param$dense_layers <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="Number of dense layers."
  )

  param$rec_size <- list(
    type = "int",
    allow_null = FALSE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc="Number of neurons for each recurrent layer."
  )

  param$rec_layers <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="Number of recurrent layers."
  )

  param$rec_type <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("gru", "lstm"),
    desc="Type of the recurrent layers. `rec_type='gru'` for Gated Recurrent Unit and `rec_type='lstm'` for Long Short-Term Memory."
  )

  param$rec_bidirectional <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="If `TRUE` a bidirectional version of the recurrent layers is used."
  )

  param$self_attention_heads <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="determining the number of attention heads for a self-attention layer. Only relevant if `attention_type='multihead'`"
  )

  param$intermediate_size <- list(
    type = "int",
    allow_null = TRUE,
    min = 1,
    max = Inf,
    allowed_values = NULL,
    desc="determining the size of the projection layer within a each transformer encoder."
  )

  param$attention_type <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("fourier", "multihead"),
    desc="Choose the relevant attention type. Please note that you may see different
    values for a case for different input orders if you choose `fourier` on linux."
  )

  param$add_pos_embedding <- list(
    type = "bool",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = NULL,
    desc="`TRUE` if positional embedding should be used."
  )

  param$dense_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc="determining the dropout between dense layers."
  )

  param$rec_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc="determining the dropout between recurrent layers."
  )

  param$encoder_dropout <- list(
    type = "double)",
    allow_null = FALSE,
    min = 0,
    max = 1,
    allowed_values = NULL,
    desc="determining the dropout for the dense projection within the encoder layers."
  )

  param$repeat_encoder <- list(
    type = "int",
    allow_null = FALSE,
    min = 0,
    max = Inf,
    allowed_values = NULL,
    desc="determining how many times the encoder should be added to the network."
  )

  param$optimizer <- list(
    type = "string",
    allow_null = FALSE,
    min = NULL,
    max = NULL,
    allowed_values = c("adam", "rmsprop", "adamw"),
    desc="determining the optimizer used for training."
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

