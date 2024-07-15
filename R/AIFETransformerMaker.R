#' @title Transformer types
#' @description This list contains transformer types. Elements of the list can be used in the public `make` of the
#'   \link{AIFETransformerMaker} `R6` class as input parameter `type`.
#'
#'   It has the following elements:
#'   * `bert` = "bert"
#'   * `deberta` = "deberta_v2"
#'   * `funnel` = "funnel"
#'   * `longformer` = "longformer"
#'   * `roberta` = "roberta"
#'
#'   Elements can be used like `AIFETrType$bert`, `AIFETrType$deberta`, `AIFETrType$funnel`, etc.
#'
#' @family Transformer
#' @export
AIFETrType <- list(
  bert = "bert",
  deberta_v2 = "deberta_v2",
  funnel = "funnel",
  longformer = "longformer",
  roberta = "roberta"
)

#' @title `R6` class for transformer creation
#' @description This class was developed to make the creation of transformers easier for users. Pass the transformer's
#'   type to the `make` method and get desired transformer. Now run the `create` or/and `train` methods of the new transformer.
#'
#'   The already created \link{aife_transformer_maker} object of this class can be used.
#'
#'   See \link{.AIFEBaseTransformer} class for details.
#'
#' @examples
#' # Create transformer maker
#' tr_maker <- AIFETransformerMaker$new()
#'
#' # Use 'make' method of the 'tr_maker' object
#' # Pass string with the type of transformers
#' # Allowed types are "bert", "deberta_v2", "funnel", "longformer", "roberta"
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
    #'   \link{AIFETrType} list.
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
#' @description Object for creating the transformers with different types. See \link{AIFETransformerMaker} class for
#'   details.
#'
#' @examples
#' # Use 'make' method of the 'aifeducation::aife_transformer_maker' object
#' # Pass string with the type of transformers
#' # Allowed types are "bert", "deberta_v2", "funnel", "longformer", "roberta"
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
