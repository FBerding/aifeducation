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
BaseModelMPNet <- R6::R6Class(
  classname = "BaseModelMPNet",
  inherit = BaseModelCore,
  private = list(
    create_model=function(args){
      configuration <- transformers$MPNetConfig(
        vocab_size = as.integer(length(args$tokenizer$get_tokenizer()$get_vocab())),
        hidden_size = as.integer(args$hidden_size),
        num_hidden_layers = as.integer(args$num_hidden_layer),
        num_attention_heads = as.integer(args$num_attention_heads),
        intermediate_size = as.integer(args$intermediate_size),
        hidden_act = tolower(args$hidden_act),
        hidden_dropout_prob = args$hidden_dropout_prob,
        attention_probs_dropout_prob = args$attention_probs_dropout_prob,
        max_position_embeddings = as.integer(args$max_position_embeddings),
        initializer_range = 0.02,
        layer_norm_eps = 1e-12
      )

      run_py_file("MPNetForMPLM_PT.py")
      device <- ifelse(torch$cuda$is_available(), "cuda", "cpu")
      private$model <- py$MPNetForMPLM_PT(configuration)$to(device)

    },
    #--------------------------------------------------------------------------
    create_data_collator = function() {
      collator_maker <- NULL
      run_py_file("DataCollatorForMPLM_PT.py")
      collator_maker <- py$CollatorMaker_PT(
        tokenizer = self$Tokenizer$get_tokenizer(),
        mlm = TRUE,
        mlm_probability = self$last_training$config$p_mask,
        plm_probability =  self$last_training$config$p_perm,
        mask_whole_words =  self$last_training$config$whole_word
      )
      return(collator_maker$collator$collate_batch)
    },
    load_BaseModel=function(dir_path){
      private$model <- py$MPNetForMPLM_PT$from_pretrained(
        dir_path,
        from_tf = FALSE,
        use_safetensors = TRUE)
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
      private$do_configuration(args = arguments, model_type = "mpnet")
    },
    #--------------------------------------------------------------------------
    train=function(text_dataset,
                   p_mask = 0.15,
                   p_perm = 0.15,
                   whole_word = TRUE,
                   val_size = 0.1,
                   n_epoch = 1,
                   batch_size = 12,
                   max_sequence_length = 250,
                   full_sequences_only = FALSE,
                   min_seq_len = 50,
                   learning_rate = 3e-3,
                   sustain_track = FALSE,
                   sustain_iso_code = NULL,
                   sustain_region = NULL,
                   sustain_interval = 15,
                   trace = TRUE,
                   pytorch_trace = 1,
                   log_dir = NULL,
                   log_write_interval = 2){
      private$do_training(args=get_called_args(n=1))
    }
  )
)

#Add the model to the user list
BaseModelsIndex$MPNet=("BaseModelMPNet")
