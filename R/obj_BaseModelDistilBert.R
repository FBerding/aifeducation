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

#' @title  DistilBERT
#' @description Represents models based on  DistilBERT.
#' @references Asnh, V., Debut, L., Chaumond, J. & Wolf, T. (2019).
#' DistilBERT, a distilled version of BERT: smaller, faster, cheaper and lighter.
#' \doi{10.48550/arXiv.1910.01108}
#' @return `r get_description("return_object")`
#' @family Base Model
#' @export
BaseModelDistilBERT <- R6::R6Class(
  classname = "BaseModelDistilBERT",
  inherit = BaseModelCore,
  private = list(
    model_type = "distilbert",
    slow_tokenizer="BertTokenizer",
    adjust_max_sequence_length = 0L,
    return_token_type_ids = FALSE,
    create_model = function(args) {
      configuration <- transformers$DistilBertConfig(
        vocab_size = as.integer(length(args$tokenizer$get_tokenizer()$get_vocab())),
        max_position_embeddings = as.integer(args$max_position_embeddings),
        sinusoidal_pos_embds =FALSE,
        n_layers= as.integer(args$n_layers),
        n_heads= as.integer(args$n_heads),
        dim= as.integer(args$dim),
        hidden_dim= as.integer(args$hidden_dim),
        dropout= args$dropout,
        activation= args$activation,
        attention_dropout  = args$attention_dropout,
        initializer_range = 0.02,
        qa_dropout =0.1,
        seq_classif_dropout =0.2,
        pad_token_id = as.integer(args$tokenizer$get_tokenizer()["pad_token_id"]),
        bos_token_id =as.integer(args$tokenizer$get_tokenizer()["bos_token_id"]),
        eos_token_id =as.integer(args$tokenizer$get_tokenizer()["eos_token_id"]),
        tie_word_embeddings =TRUE
      )
      private$model <- transformers$DistilBertForMaskedLM(configuration)
    },
    load_BaseModel = function(dir_path) {
      private$model <- transformers$DistilBertForMaskedLM$from_pretrained(dir_path)
    },
    #---------------------------------------------------------------------------
    check_arg_combinations = function(args) {
      if (args$hidden_size %% args$num_attention_heads != 0L) {
        stop("hidden_size must be a multiple auf num_attention_heads.")
      }
    }
  ),
  public = list(
    #---------------------------------------------------------------------------
    #' @description Configures a new object of this class.
    #' Please ensure that your chosen configuration comply with the following
    #' guidelines:
    #' * hidden_size is a multiple of num_attention_heads.
    #'
    #' @param tokenizer `r get_param_doc_desc("tokenizer")`
    #' @param max_position_embeddings `r get_param_doc_desc("max_position_embeddings")`
    #' @param dim `r get_param_doc_desc("dim")`
    #' @param n_layers `r get_param_doc_desc("n_layers")`
    #' @param n_heads `r get_param_doc_desc("n_heads")`
    #' @param hidden_dim `r get_param_doc_desc("hidden_dim")`
    #' @param activation `r get_param_doc_desc("activation")`
    #' @param dropout `r get_param_doc_desc("dropout")`
    #' @param attention_dropout `r get_param_doc_desc("attention_dropout")`
    #' @return `r get_description("return_nothing")`
    configure = function(tokenizer,
                         max_position_embeddings = 512L,
                         dim  = 768L,
                         n_layers = 12L,
                         n_heads = 12L,
                         hidden_dim  = 3072L,
                         activation = "GELU",
                         dropout = 0.1,
                         attention_dropout = 0.1) {
      arguments <- get_called_args(n = 1L)
      private$do_configuration(args = arguments)
    }
  )
)

# Add the model to the user list
BaseModelsIndex$robertaxml <- list(
  class_name="BaseModelRobertaXML",
  model_type="robertaxml",
  reference="Asnh, V., Debut, L., Chaumond, J. & Wolf, T. (2019).
  DistilBERT, a distilled version of BERT: smaller, faster, cheaper and lighter.
  doi: [10.48550/arXiv.1910.01108](https://doi.org/10.48550/arXiv.1910.01108)",
  req_sentencepiece=FALSE
)
