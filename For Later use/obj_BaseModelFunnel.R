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

#' @title Funnel transformer
#' @description Represents models based on the Funnel-Transformer.
#' @references Dai, Z., Lai, G., Yang, Y. & Le, Q. V. (2020). Funnel-Transformer: Filtering out Sequential Redundancy
#'   for Efficient Language Processing. \doi{10.48550/arXiv.2006.03236}
#' @return `r get_description("return_object")`
#' @family Base Model
#' @export
BaseModelFunnel <- R6::R6Class(
  classname = "BaseModelFunnel",
  inherit = BaseModelCore,
  private = list(

    model_type="funnel",

    adjust_max_sequence_length=1,

    create_model=function(args){
      configuration <- transformers$FunnelConfig(
        vocab_size = as.integer(length(args$tokenizer$get_tokenizer()$get_vocab())),
        block_sizes = as.integer(args$block_sizes),
        block_repeats = NULL,
        num_decoder_layers = as.integer(args$num_decoder_layers),
        d_model = as.integer(args$hidden_size),
        n_head = as.integer(args$num_attention_heads),
        d_head = as.integer(args$hidden_size),
        d_inner = as.integer(args$intermediate_size),
        hidden_act = tolower(args$hidden_act),
        hidden_dropout_prob = args$hidden_dropout_prob,
        attention_probs_dropout_prob = args$attention_probs_dropout_prob,
        activation_dropout = as.integer(args$activation_dropout),
        initializer_range = 0.02,
        layer_norm_eps = 1e-12,
        pooling_type = tolower(args$funnel_pooling_type),
        attention_type = "relative_shift",
        separate_cls = TRUE,
        truncate_seq = TRUE,
        pool_q_only = TRUE,
        max_position_embeddings = as.integer(args$max_position_embeddings),
      )
      private$model <- transformers$FunnelForMaskedLM(configuration)
  },
  load_BaseModel=function(dir_path){
    private$model <- transformers$FunnelForMaskedLM$from_pretrained(dir_path)
  }
  ),
  public = list(
    #---------------------------------------------------------------------------
    #' @description Configures a new object of this class.
    #' @param tokenizer `r get_param_doc_desc("tokenizer")`
    #' @param max_position_embeddings `r get_param_doc_desc("max_position_embeddings")`
    #' @param hidden_size `r get_param_doc_desc("hidden_size")`
    #' @param block_sizes `r get_param_doc_desc("block_sizes")`
    #' @param num_hidden_layers `r get_param_doc_desc("num_hidden_layers")`
    #' @param num_attention_heads `r get_param_doc_desc("num_attention_heads")`
    #' @param num_decoder_layers `r get_param_doc_desc("num_decoder_layers")`
    #' @param funnel_pooling_type `r get_param_doc_desc("funnel_pooling_type")`
    #' @param intermediate_size `r get_param_doc_desc("intermediate_size")`
    #' @param hidden_act `r get_param_doc_desc("hidden_act")`
    #' @param hidden_dropout_prob `r get_param_doc_desc("hidden_dropout_prob")`
    #' @param attention_probs_dropout_prob `r get_param_doc_desc("attention_probs_dropout_prob")`
    #' @param activation_dropout `r get_param_doc_desc("activation_dropout")`
    #' @return `r get_description("return_nothing")`
    configure = function(tokenizer,
                         max_position_embeddings = 512,
                         hidden_size = 768,
                         block_sizes = c(4, 4, 4),
                         num_attention_heads = 12,
                         intermediate_size = 3072,
                         num_decoder_layers = 2,
                         funnel_pooling_type = "Mean",
                         hidden_act = "GELU",
                         hidden_dropout_prob = 0.1,
                         attention_probs_dropout_prob = 0.1,
                         activation_dropout = 0.0) {
      arguments <- get_called_args(n = 1)
      private$do_configuration(args = arguments)
    }
  )
)

#Add the model to the user list
BaseModelsIndex$Funnel=("BaseModelFunnel")
