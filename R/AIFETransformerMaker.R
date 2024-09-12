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

#' @title Transformer objects
#' @description This list contains transformer objects. Elements of the list are used in the public `make` of the
#'   [AIFETransformerMaker] `R6` class. This list is not designed to be used directly.
#'
#'   It has the following elements: `r get_allowed_transformer_types()`
#'
#' @family Transformers for developers
#' @keywords internal
.AIFETrObj <- list()

#' @title `R6` class for transformer creation
#' @description This class was developed to make the creation of transformers easier for users. Pass the transformer's
#'   type to the `make` method and get desired transformer. Now run the `create` or/and `train` methods of the new
#'   transformer.
#'
#'   The already created [aife_transformer_maker] object of this class can be used.
#'
#'   See p.3 Transformer Maker in
#'   [Transformers for Developers](https://fberding.github.io/aifeducation/articles/transformers.html) for details.
#'
#'   See [.AIFEBaseTransformer] class for details.
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
      if (type %in% names(.AIFETrObj)) {
        transformer <- .AIFETrObj[[type]]()
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
