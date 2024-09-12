#' @family Transformer internal
#' @keywords internal
.AIFECustomTransformer <- R6::R6Class(
  classname = ".AIFECustomTransformer",
  inherit = .AIFEBaseTransformer,
  # private attributes
  private = list(
    title = "Custom Model",
    steps_for_creation = list(
      # required
      create_tokenizer_draft = function(self) { },
      calculate_vocab = function(self) { },
      save_tokenizer_draft = function(self) { },
      create_final_tokenizer = function(self) { },
      create_transformer_model = function(self) { }
      # optional
      # check_max_pos_emb = function(self) { }
    ),
    steps_for_training = list(
      # required
      load_existing_model = function(self) { }
      # optional
      # cuda_empty_cache = function() { },
      # create_data_collator = function() { }
    )
  ),
  # public methods
  public = list(
    initialize = function() {
      super$set_title(private$title)
    },
    create = function( # --------------------------
                      ml_framework,
                      model_dir,
                      vocab_raw_texts,
                      vocab_size,
                      # ...
                      trace,
                      pytorch_safetensors,
                      # --------------------------
                      dep_param1,
                      dep_param2,
                      # ...
                      dep_paramN) {
      # -----------------------------------------
      super$set_model_param("dep_param1", dep_param1)
      super$set_model_param("dep_param2", dep_param2)
      # ...
      super$set_model_param("dep_paramN", dep_paramN)

      # -----------------------------------------
      super$set_required_SFC(private$steps_for_creation)

      # optional
      # super$set_SFC_check_max_pos_emb(private$steps_for_creation$check_max_pos_emb)

      # -----------------------------------------
      super$create(
        ml_framework = ml_framework,
        model_dir = model_dir,
        vocab_raw_texts = vocab_raw_texts,
        vocab_size = vocab_size,
        # ...
        trace = trace,
        pytorch_safetensors = pytorch_safetensors
      )
    },
    train = function( # --------
                     ml_framework,
                     # ...
                     # --------
                     dep_param1,
                     # ...
                     dep_paramN) {
      # -----------------------------------------
      super$set_model_param("dep_param1", dep_param1)
      # ...
      super$set_model_param("dep_paramN", dep_paramN)

      # -----------------------------------------
      super$set_SFT_load_existing_model(private$steps_for_training$load_existing_model)
      # optional
      # super$set_SFT_cuda_empty_cache(private$steps_for_training$cuda_empty_cache)
      # super$set_SFT_create_data_collator(private$steps_for_training$create_data_collator)

      # -----------------------------------------
      super$train(
        ml_framework = ml_framework,
        # ...
      )
    }
  )
)

# .AIFETrObj[[AIFETrType$custom]] <- .AIFECustomTransformer$new
