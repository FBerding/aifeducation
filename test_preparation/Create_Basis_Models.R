# This file does not contain any tests. It is used for creating base models
# that can be used for testing TextEmbeddingModels.

# Config-------------------------------------------------------------------------
root_path_data <- testthat::test_path("test_data/TEM")

create_dir(root_path_data, FALSE)

ml_frameworks <- c(
  "tensorflow",
  "pytorch"
)

method_list <- list(
  tensorflow = c(
    "bert",
    "roberta",
    "longformer",
    "funnel",
    "deberta_v2"
  ),
  pytorch = c(
    "bert",
    "roberta",
    "longformer",
    "funnel",
    "deberta_v2",
    "mpnet"
  )
)

example_data <- imdb_movie_reviews
example_data["bib_entry"] <- NA
example_data["license"] <- NA
example_data["url_license"] <- NA
example_data["text_license"] <- NA
example_data["url_source"] <- NA

text_data <- LargeDataSetForText$new(example_data)

trace <- FALSE
epochs <- 2
# Start creation and training---------------------------------------------------


for (framework in ml_frameworks) {
  root_path_results <- paste0(root_path_data, "/", framework)
  create_dir(root_path_results, FALSE)

  for (method in method_list[[framework]]) {
    root_path_results_model <- paste0(root_path_results, "/", method)
    create_dir(root_path_results_model, FALSE)

    # BaseModels
    transformer <- aife_transformer_maker$make(method)

    # Create & Train -------------------------------------------------------------------
    if (method == "bert") {
      transformer$create(
        ml_framework = framework,
        model_dir = root_path_results_model,
        text_dataset = text_data,
        vocab_size = 30522,
        vocab_do_lower_case = FALSE,
        max_position_embeddings = 512,
        hidden_size = 256,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 512,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_probs_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )

      transformer$train(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        text_dataset = text_data,
        p_mask = 0.15,
        whole_word = TRUE,
        val_size = 0.1,
        n_epoch = epochs,
        batch_size = 12,
        chunk_size = 250,
        full_sequences_only = FALSE,
        min_seq_len = 50,
        learning_rate = 3e-3,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = as.numeric(trace),
        pytorch_trace = as.numeric(trace),
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
    } else if (method == "mpnet") {
      transformer$create(
        ml_framework = framework,
        model_dir = root_path_results_model,
        text_dataset = text_data,
        vocab_size = 30522,
        vocab_do_lower_case = FALSE,
        max_position_embeddings = 512,
        hidden_size = 256,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 512,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_probs_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
      transformer$train(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        text_dataset = text_data,
        p_mask = 0.15,
        p_perm = 0.15,
        whole_word = TRUE,
        val_size = 0.1,
        n_epoch = epochs,
        batch_size = 12,
        chunk_size = 250,
        full_sequences_only = FALSE,
        min_seq_len = 50,
        learning_rate = 3e-3,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = as.numeric(trace),
        pytorch_trace = as.numeric(trace),
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
    } else if (method == "roberta") {
      transformer$create(
        ml_framework = framework,
        model_dir = root_path_results_model,
        text_dataset = text_data,
        vocab_size = 30522,
        add_prefix_space = FALSE,
        trim_offsets = TRUE,
        max_position_embeddings = 512,
        hidden_size = 256,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 512,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_probs_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
      transformer$train(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        text_dataset = text_data,
        p_mask = 0.15,
        val_size = 0.1,
        n_epoch = epochs,
        batch_size = 12,
        chunk_size = 250,
        full_sequences_only = FALSE,
        min_seq_len = 50,
        learning_rate = 3e-2,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = as.numeric(trace),
        pytorch_trace = as.numeric(trace),
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
    } else if (method == "longformer") {
      transformer$create(
        ml_framework = framework,
        model_dir = root_path_results_model,
        text_dataset = text_data,
        vocab_size = 30522,
        add_prefix_space = FALSE,
        trim_offsets = TRUE,
        max_position_embeddings = 512,
        hidden_size = 256,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 512,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_probs_dropout_prob = 0.1,
        attention_window = 512,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
      transformer$train(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        text_dataset = text_data,
        p_mask = 0.15,
        val_size = 0.1,
        n_epoch = epochs,
        batch_size = 12,
        chunk_size = 250,
        full_sequences_only = FALSE,
        min_seq_len = 50,
        learning_rate = 3e-2,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = as.numeric(trace),
        pytorch_trace = as.numeric(trace),
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
    } else if (method == "funnel") {
      transformer$create(
        ml_framework = framework,
        model_dir = root_path_results_model,
        text_dataset = text_data,
        vocab_size = 30522,
        vocab_do_lower_case = FALSE,
        max_position_embeddings = 512,
        hidden_size = 256,
        target_hidden_size = 32,
        block_sizes = c(4, 4, 4),
        num_attention_heads = 2,
        intermediate_size = 512,
        num_decoder_layers = 2,
        pooling_type = "mean",
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_probs_dropout_prob = 0.1,
        activation_dropout = 0.0,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
      transformer$train(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        text_dataset = text_data,
        p_mask = 0.15,
        whole_word = TRUE,
        val_size = 0.1,
        n_epoch = epochs,
        batch_size = 12,
        chunk_size = 250,
        full_sequences_only = FALSE,
        min_seq_len = 50,
        learning_rate = 3e-3,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = as.numeric(trace),
        pytorch_trace = as.numeric(trace),
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
    } else if (method == "deberta_v2") {
      transformer$create(
        ml_framework = framework,
        model_dir = root_path_results_model,
        text_dataset = text_data,
        vocab_size = 30000,
        vocab_do_lower_case = FALSE,
        max_position_embeddings = 512,
        hidden_size = 256,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 512,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_probs_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
      transformer$train(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        text_dataset = text_data,
        p_mask = 0.15,
        whole_word = TRUE,
        val_size = 0.1,
        n_epoch = epochs,
        batch_size = 12,
        chunk_size = 250,
        full_sequences_only = FALSE,
        min_seq_len = 50,
        learning_rate = 3e-2,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = as.numeric(trace),
        pytorch_trace = as.numeric(trace),
        pytorch_safetensors = TRUE,
        log_dir = root_path_results_model
      )
    }
  }
}
