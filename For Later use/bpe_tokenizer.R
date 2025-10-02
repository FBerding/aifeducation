# ===============================================================================

#' @title BPE-Tokenizer
#' @description Tokenizer based on a Byte-Pair Encoding model.
#' @return `r get_description("return_object")`
#' @family Tokenizer
#' @export
BPETokenizer <- R6::R6Class(
  classname = "BPETokenizer",
  inherit = TokenizerBase,
  private = list(),
  public = list(
    #--------------------------------------------------------------------------
    #' @description Configures a new object of this class.
    #' @param vocab_size `r get_param_doc_desc("vocab_size")`
    #' @param add_prefix_space `r get_param_doc_desc("add_prefix_space")`
    #' @param trim_offsets `r get_param_doc_desc("trim_offsets")`
    #' @param vocab_do_lower_case `r get_param_doc_desc("vocab_do_lower_case")`
    #' @return `r get_description("return_nothing")`
    configure = function(vocab_size = 2000L,
                         add_prefix_space = TRUE,
                         trim_offsets = FALSE,
                         vocab_do_lower_case = FALSE) {
      private$load_reload_python_scripts()
      private$check_config_for_FALSE()

      private$save_all_args(
        args = get_called_args(n = 1L),
        group = "configure"
      )

      # Set package versions
      private$set_package_versions()

      # Set configured to TRUE to avoid changes in the model
      private$set_configuration_to_TRUE()
    },
    #--------------------------------------------------------------------------
    #' @description Trains a new object of this class
    #' @param text_dataset `r get_param_doc_desc("text_dataset")`
    #' @param statistics_max_tokens_length `r get_param_doc_desc("statistics_max_tokens_length")`
    #' @param sustain_track `r get_param_doc_desc("sustain_track")`
    #' @param sustain_iso_code `r get_param_doc_desc("sustain_iso_code")`
    #' @param sustain_region `r get_param_doc_desc("sustain_region")`
    #' @param sustain_interval `r get_param_doc_desc("sustain_interval")`
    #' @param sustain_log_level `r get_description("sustain_log_level")`
    #' @param trace `r get_param_doc_desc("trace")`
    #' @return `r get_description("return_nothing")`
    train = function(text_dataset,
                     statistics_max_tokens_length = 512L,
                     sustain_track = FALSE,
                     sustain_iso_code = NULL,
                     sustain_region = NULL,
                     sustain_interval = 15L,
                     sustain_log_level="warning",
                     trace = FALSE) {
      private$check_config_for_TRUE()
      private$check_for_untrained()

      private$save_all_args(
        args = get_called_args(n = 1L),
        group = "training"
      )

      private$init_and_start_sustainability_tracking()

      # Define tokens
      sep_token <- "[SEP]"
      sep_id <- 1L
      cls_token <- "[CLS]"
      cls_id <- 0L
      unk_token <- "[UNK]"
      pad_token <- "[PAD]"
      mask_token <- "[MASK]"
      bos_token <- "[CLS]"
      eos_token <- "[SEP]"

      special_tokens <- c(
        cls_token,
        sep_token,
        unk_token,
        pad_token,
        mask_token,
        bos_token,
        eos_token
      )

      tok_new <- tok$Tokenizer(
        tok$models$BPE(
          unk_token = unk_token
        )
      )

      if (private$model_config$vocab_do_lower_case) {
        tok_new$normalizer <- tok$normalizers$Sequence(
          c(tok$normalizers$Lowercase(), tok$normalizers$NFC())
        )
      } else {
        tok_new$normalizer <- tok$normalizers$NFC()
      }

      tok_new$post_processor <- tok$processors$RobertaProcessing(
        trim_offsets = private$model_config$trim_offsets,
        add_prefix_space = private$model_config$add_prefix_space,
        sep = reticulate::tuple(sep_token, as.integer(sep_id)),
        cls = reticulate::tuple(cls_token, as.integer(cls_id))
      )

      tok_new$decoder <- tok$decoders$ByteLevel()

      tok_new$enable_truncation(max_length = 512L)
      tok_new$enable_padding(pad_token = pad_token)

      # configurate training
      trainer <- tok$trainers$BpeTrainer(
        vocab_size = as.integer(private$model_config$vocab_size),
        special_tokens = special_tokens,
        show_progress = trace
      )

      # calculate the model
      run_py_file("datasets_transformer_compute_vocabulary.py")

      tok_new$train_from_iterator(
        iterator = py$batch_iterator(
          batch_size = 200L,
          dataset = text_dataset$get_dataset(),
          log_file = NULL,
          write_interval = 2L,
          value_top = 0L,
          total_top = 1L,
          message_top = "NA"
        ),
        trainer = trainer,
        length = as.integer(text_dataset$n_rows())
      )

      # Create the complete and final model
      private$model <- transformers$PreTrainedTokenizerFast(
        tokenizer_object = tok_new,
        unk_token = unk_token,
        sep_token = sep_token,
        pad_token = pad_token,
        cls_token = cls_token,
        mask_token = mask_token,
        bos_token = bos_token,
        eos_token = eos_token
      )

      # Calculate tokenizer statistics
      private$tokenizer_statistics <- self$calculate_statistics(
        text_dataset = text_dataset,
        statistics_max_tokens_length = statistics_max_tokens_length,
        step = "creation"
      )

      # Update
      private$model_config$vocab_size <- length(private$model$get_vocab()) + length(special_tokens)

      # Set trained field
      private$trained <- TRUE

      private$stop_sustainability_tracking("Create tokenizer")
    }
  )
)

# Add the model to the user list
TokenizerIndex$BPETokenizer <- ("BPETokenizer")
