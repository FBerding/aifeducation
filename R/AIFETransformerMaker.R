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

#' @title Transformer types
#' @description This list contains transformer types. Elements of the list can be used in [aife_transformer.make()]
#'   function as input parameter `type`.
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
  mpnet = "mpnet",
  modernbert = "modernbert"
)

#' @title Transformer objects
#' @description This list contains transformer objects. Elements of the list can be used in [aife_transformer.make()]
#'   function as input parameter `type`. This list is not designed to be used directly.
#'
#'   It has the following elements: `r get_allowed_transformer_types()`
#'
#' @family Transformers for developers
#' @keywords internal
.AIFETrObj <- list()

#' @title Tokenizer class names
#' @description This list contains tokenizer class names. Elements of the list are used in
#'   [aife_transformer.load_tokenizer()] to get a tokenizer for a transformer model. This list is not designed to be
#'   used directly.
#'
#' @family Transformers for developers
#' @keywords internal
.AIFETrTokenizer <- list()

#' @title Configuration class names
#' @description This list contains configuration class names. Elements of the list are used in
#'   [aife_transformer.load_model_config()] to get a configuration for a transformer model. This list is not designed to
#'   be used directly.
#'
#' @family Transformers for developers
#' @keywords internal
.AIFETrConfig <- list()

#' @title Model class names
#' @description This list contains model class names. Elements of the list are used in [aife_transformer.load_model()]
#'   to get a transformer model. This list is not designed to be used directly.
#'
#' @family Transformers for developers
#' @keywords internal
.AIFETrModel <- list()

#' @title MLM-Model class names
#' @description This list contains MLM-model class names. Elements of the list are used in [aife_transformer.load_model()]
#'   to get a transformer MLM-model. This list is not designed to be used directly.
#'
#' @family Transformers for developers
#' @keywords internal
.AIFETrModelMLM <- list()


#' @title Check transformer type
#' @description Check the passed type of transformer.
#'
#' @param type `string` A type of the new transformer. Allowed types are `r get_allowed_transformer_types()`. See
#'   [AIFETrType] list.
#'
#' @returns If success - nothing, otherwise - an error.
#'
#' @family Transformers for developers
#' @keywords internal
.aife_transformer.check_type <- function(type) {
  if (type %in% names(.AIFETrObj) == FALSE) {
    stop(
      paste0(
        "Transformer type '", type, "' is invalid.",
        " Allowed types are: ", get_allowed_transformer_types(), ". "
      )
    )
  }
}

#' @title Make a transformer
#' @description Creates a new transformer with the passed type.
#' See p.3 Transformer Maker in
#'   [Transformers for Developers](https://fberding.github.io/aifeducation/articles/transformers.html) for details.
#'
#'   See [.AIFEBaseTransformer] class for details.
#' @param type `string` A type of the new transformer. Allowed types are `r get_allowed_transformer_types()`. See
#'   [AIFETrType] list.
#' @param init_trace `bool` option to show prints. If `TRUE` (by default) - messages will be shown, otherwise
#'   (`FALSE`) - hidden.
#' @return If success - a new transformer, otherwise - an error (passed type is invalid).
#'
#' @family Transformer
#' @export
aife_transformer.make <- function(type, init_trace = TRUE) {
  .aife_transformer.check_type(type)

  transformer <- .AIFETrObj[[type]](init_trace)

  return(transformer)
}

#' @title Load a tokenizer
#' @description Loads a tokenizer for a transformer with the passed type.
#'
#' @param type `string` A type of the transformer. Allowed types are `r get_allowed_transformer_types()`. See
#'   [AIFETrType] list.
#' @param model_dir `r paramDesc.model_dir_path()`
#' @return If success - a tokenizer, otherwise - an error (passed type is invalid).
#'
#' @family Transformers for developers
#' @export
aife_transformer.load_tokenizer <- function(type, model_dir) {
  .aife_transformer.check_type(type)

  tokenizer_str <- .AIFETrTokenizer[[type]]
  tokenizer <- transformers[[tokenizer_str]]$from_pretrained(model_dir)

  return(tokenizer)
}

#' @title Load a model configuration
#' @description Loads a configuration for a transformer with the passed type.
#'
#' @param type `string` A type of the transformer. Allowed types are `r get_allowed_transformer_types()`. See
#'   [AIFETrType] list.
#' @param model_dir `r paramDesc.model_dir_path()`
#' @return If success - a model configuration, otherwise - an error (passed type is invalid).
#'
#' @family Transformers for developers
#' @keywords internal
#' @export
aife_transformer.load_model_config <- function(type, model_dir) {
  .aife_transformer.check_type(type)

  config_str <- .AIFETrConfig[[type]]
  model_config <- transformers[[config_str]]$from_pretrained(model_dir)

  return(model_config)
}

#' @title Load a transformer model
#' @description Loads a model for a transformer with the passed type.
#'
#' @param type `string` A type of the transformer. Allowed types are `r get_allowed_transformer_types()`. See
#'   [AIFETrType] list.
#' @param model_dir `r paramDesc.model_dir_path()`
#' @param from_tf `bool` Whether to load the model weights from a TensorFlow checkpoint save file.
#' @param load_safe `bool` Whether or not to use safetensors checkpoints.
#' @param add_pooler `bool` Whether to add a pooling layer, `FALSE` by default.
#' @return If success - a transformer model, otherwise - an error (passed type is invalid).
#'
#' @family Transformers for developers
#' @keywords internal
#' @export
aife_transformer.load_model <- function(type, model_dir, from_tf, load_safe, add_pooler = FALSE) {
  .aife_transformer.check_type(type)

  config <- aife_transformer.load_model_config(type, model_dir)

  model_str <- .AIFETrModel[[type]]
  if(type=="modernbert"|type=="funnel"){
    model <- transformers[[model_str]]$from_pretrained(
      model_dir,
      from_tf = from_tf,
      use_safetensors = load_safe,
      #add_pooling_layer = add_pooler,
      config = config
    )
  } else {
    model <- transformers[[model_str]]$from_pretrained(
      model_dir,
      from_tf = from_tf,
      use_safetensors = load_safe,
      add_pooling_layer = add_pooler,
      config = config
    )
  }


  return(model)
}

#' @title Load a MLM-model
#' @description Loads a MLM-model for a transformer with the passed type.
#'
#' @param type `string` A type of the transformer. Allowed types are `r get_allowed_transformer_types()`. See
#'   [AIFETrType] list.
#' @param model_dir `r paramDesc.model_dir_path()`
#' @param from_tf `bool` Whether to load the model weights from a TensorFlow checkpoint save file.
#' @param load_safe `bool` Whether or not to use safetensors checkpoints.
#' @return If success - a MLM-model, otherwise - an error (passed type is invalid).
#'
#' @family Transformers for developers
#' @export
aife_transformer.load_model_mlm <- function(type, model_dir, from_tf, load_safe) {
  .aife_transformer.check_type(type)

  is_py <- type == AIFETrType$mpnet

  model_mlm_str <- .AIFETrModelMLM[[type]]
  model_mlm_class <- NULL
  if (is_py) model_mlm_class <- py[[model_mlm_str]]
  else model_mlm_class <- transformers[[model_mlm_str]]

  model_mlm <- model_mlm_class$from_pretrained(
    model_dir,
    from_tf = from_tf,
    use_safetensors = load_safe
  )

  return(model_mlm)
}
