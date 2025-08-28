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

#' @title
#' @description
#' @return
#' @family Base Model
#' @export
BaseModelModernBert <- R6::R6Class(
  classname = "BaseModelModernBert",
  inherit = BaseModelCore,
  private = list(
    create_model=function(args){
      configuration <- transformers$ModernBertConfig(
        vocab_size = as.integer(length(args$tokenizer$get_tokenizer()$get_vocab())),
        hidden_size = as.integer(args$hidden_size),
        intermediate_size = as.integer(args$intermediate_size),
        num_hidden_layers = as.integer(args$num_hidden_layer),
        num_attention_heads = as.integer(args$num_attention_heads),
        hidden_activation = tolower(args$hidden_act),
        max_position_embeddings = as.integer(args$max_position_embeddings),
        initializer_range = 0.02,
        norm_eps = 1e-12,
        pad_token_id = args$tokenizer$get_tokenizer()$pad_token_id,
        embedding_dropout = args$hidden_dropout_prob,
        mlp_dropout = args$hidden_dropout_prob,
        attention_dropout = args$attention_probs_dropout_prob
      )
      private$model <- transformers$ModernBertForMaskedLM(configuration)

    },
    load_BaseModel=function(dir_path){
      private$model <- transformers$ModernBertForMaskedLM$from_pretrained(dir_path)
    }
  ),
  public = list(
    #---------------------------------------------------------------------------
    configure = function(tokenizer,
                         max_position_embeddings = 512,
                         hidden_size = 768,
                         num_hidden_layer = 12,
                         num_attention_heads = 12,
                         intermediate_size = 3072,
                         hidden_act = "GELU",
                         hidden_dropout_prob = 0.1,
                         attention_probs_dropout_prob = 0.1) {
      arguments <- get_called_args(n = 1)
      private$do_configuration(args = arguments, model_type = "modernbert")
    }
  )
)

#Add the model to the user list
BaseModelsIndex$ModernBert=("BaseModelModernBert")
