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

#' @title ModernBert
#' @description Represents models based on Modern Bert.
#' @references Devlin, J., Chang, M.‑W., Lee, K., & Toutanova, K. (2019). BERT: Pre-training of Deep Bidirectional
#'   Transformers for Language Understanding. In J. Burstein, C. Doran, & T. Solorio (Eds.), Proceedings of the 2019
#'   Conference of the North (pp. 4171--4186). Association for Computational Linguistics. \doi{10.18653/v1/N19-1423}
#' @return `r get_description("return_object")`
#' @family Base Model
#' @export
BaseModelModernBert <- R6::R6Class(
  classname = "BaseModelModernBert",
  inherit = BaseModelCore,
  private = list(
    model_type = "modernbert",

    create_model=function(args){
      configuration <- transformers$ModernBertConfig(
        vocab_size = as.integer(length(args$tokenizer$get_tokenizer()$get_vocab())),
        hidden_size = as.integer(args$hidden_size),
        intermediate_size = as.integer(args$intermediate_size),
        num_hidden_layers = as.integer(args$num_hidden_layers),
        num_attention_heads = as.integer(args$num_attention_heads),
        hidden_activation = tolower(args$hidden_act),
        max_position_embeddings = as.integer(args$max_position_embeddings),
        initializer_range = 0.02,
        norm_eps = 1e-12,
        pad_token_id = args$tokenizer$get_tokenizer()$pad_token_id,
        eos_token_id= args$tokenizer$get_tokenizer()$eos_token_id,
        bos_token_id= args$tokenizer$get_tokenizer()$bos_token_id,
        cls_token_id=args$tokenizer$get_tokenizer()$cls_token_id,
        sep_token_id= args$tokenizer$get_tokenizer()$sep_token_id,
        embedding_dropout = args$embedding_dropout,
        mlp_dropout = args$mlp_dropout,
        attention_dropout = args$attention_dropout,
        deterministic_flash_attn =TRUE
      )

      private$model <- transformers$ModernBertForMaskedLM(configuration)

    },
    load_BaseModel=function(dir_path){
      private$model <- transformers$ModernBertForMaskedLM$from_pretrained(dir_path)
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
    #' @param embedding_dropout `r get_param_doc_desc("embedding_dropout")`
    #' @param mlp_dropout `r get_param_doc_desc("mlp_dropout")`
    #' @param attention_dropout `r get_param_doc_desc("attention_dropout")`
    #' @return `r get_description("return_nothing")`
    configure = function(tokenizer,
                         max_position_embeddings = 512,
                         hidden_size = 768,
                         num_hidden_layers = 12,
                         num_attention_heads = 12,
                         intermediate_size = 3072,
                         hidden_act = "GELU",
                         embedding_dropout = 0.1,
                         mlp_dropout=0.1,
                         attention_dropout = 0.1) {
      arguments <- get_called_args(n = 1)
      private$do_configuration(args = arguments)
    }
  )
)

#Add the model to the user list
BaseModelsIndex$ModernBert=("BaseModelModernBert")
