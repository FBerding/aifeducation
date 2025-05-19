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
  mpnet = "mpnet"
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
  transformer <- NULL
  if (type %in% names(.AIFETrObj)) {
    transformer <- .AIFETrObj[[type]](init_trace)
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
