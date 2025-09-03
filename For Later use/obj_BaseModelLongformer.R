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

#' @title Longformer
#' @description Represents models based on Longformer
#' @references Beltagy, I., Peters, M. E., & Cohan, A. (2020). Longformer: The Long-Document Transformer.
#'   \doi{10.48550/arXiv.2004.05150}
#' @return `r get_description("return_object")`
#' @family Base Model
#' @export
BaseModelLongformer <- R6::R6Class(
  classname = "BaseModelLongformer",
  inherit = BaseModelCore,
  private = list(
    model_type = "longformer",

    adjust_max_sequence_length=2,

    create_model=function(args){
      configuration <- transformers$LongformerConfig(
        vocab_size = as.integer(length(args$tokenizer$get_tokenizer()$get_vocab())+length(unique(args$tokenizer$get_tokenizer()$special_tokens_map))),
        max_position_embeddings = as.integer(args$max_position_embeddings),
        hidden_size = as.integer(args$hidden_size),
        num_hidden_layers = as.integer(args$num_hidden_layers),
        num_attention_heads = as.integer(args$num_attention_heads),
        intermediate_size = as.integer(args$intermediate_size),
        hidden_act = tolower(args$hidden_act),
        hidden_dropout_prob = args$hidden_dropout_prob,
        attention_probs_dropout_prob = args$attention_probs_dropout_prob,
        attention_window = as.integer(args$attention_window),
        type_vocab_size = as.integer(2),
        initializer_range = 0.02,
        layer_norm_eps = 1e-12
      )

      private$model <- transformers$LongformerForMaskedLM(configuration)
    },
    load_BaseModel=function(dir_path){
      private$model <- transformers$LongformerForMaskedLM$from_pretrained(dir_path)
    }
  ),
  public = list(
    #---------------------------------------------------------------------------
    #' @description Configures a new object of this class.
    #' @param tokenizer `r get_param_doc_desc("tokenizer")`
    #' @param max_position_embeddings `r get_param_doc_desc("max_position_embeddings")`
    #' @param hidden_size `r get_param_doc_desc("hidden_size")`
    #' @param num_hidden_layers `r get_param_doc_desc("num_hidden_layers")`
    #' @param num_attention_heads `r get_param_doc_desc("num_attention_heads")`
    #' @param intermediate_size `r get_param_doc_desc("intermediate_size")`
    #' @param hidden_act `r get_param_doc_desc("hidden_act")`
    #' @param hidden_dropout_prob `r get_param_doc_desc("hidden_dropout_prob")`
    #' @param attention_probs_dropout_prob `r get_param_doc_desc("attention_probs_dropout_prob")`
    #' @param attention_window `r get_param_doc_desc("attention_window")`
    #' @return `r get_description("return_nothing")`
    configure = function(tokenizer,
                         max_position_embeddings = 512,
                         hidden_size = 768,
                         num_hidden_layers = 12,
                         num_attention_heads = 12,
                         intermediate_size = 3072,
                         hidden_act = "GELU",
                         hidden_dropout_prob = 0.1,
                         attention_probs_dropout_prob = 0.1,
                         attention_window = 512) {
      arguments <- get_called_args(n = 1)
      private$do_configuration(args = arguments)
    }
  )
)

#Add the model to the user list
BaseModelsIndex$Longformer=("BaseModelLongformer")
