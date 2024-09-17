# This file does not contain any tests. It is used for creating base models
# that can be used for testing TextEmbeddingModels.

# Config-------------------------------------------------------------------------
root_path_data <- testthat::test_path("test_data/TextEmbeddingModel")

if (dir.exists(root_path_data) == FALSE) {
  dir.create(root_path_data)
}

ml_frameworks <- c(
  "tensorflow",
  "pytorch"
)

method_list <- list(
  # "tensorflow"=c(#"bert",
  #               #"roberta"#,
  #               "longformer",
  #               "funnel",
  #               "deberta_v2"
  # ),
  "pytorch" = c( # "bert",
    #           #"roberta",
    "mpnet" # ,
    # "longformer",
    # "funnel",
    # "deberta_v2"
  )
)

example_data <- imdb_movie_reviews
trace <- FALSE

# Start creation and training---------------------------------------------------
for (framework in ml_frameworks) {
  root_path_results <- paste0(root_path_data, "/", framework)
  if (dir.exists(root_path_results) == FALSE) {
    dir.create(root_path_results)
  }

  for (method in method_list[[framework]]) {
    root_path_results_model <- paste0(root_path_results, "/", method)
    if (dir.exists(root_path_results_model) == FALSE) {
      dir.create(root_path_results_model)
    }

    # Base Models
    if (method == "bert") {
      create_bert_model(
        ml_framework = framework,
        model_dir = root_path_results_model,
        vocab_raw_texts = example_data$text,
        vocab_size = 50000,
        vocab_do_lower_case = FALSE,
        max_position_embeddings = 512,
        hidden_size = 256,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 256,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace
      )

      train_tune_bert_model(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        raw_texts = example_data$text[1:10],
        p_mask = 0.15,
        whole_word = TRUE,
        full_sequences_only = TRUE,
        val_size = 0.25,
        n_epoch = 2,
        batch_size = 2,
        chunk_size = 100,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = 0
      )
    } else if (method == "mpnet") {
      transformer <- aife_transformer_maker$make(AIFETrType$mpnet)
      transformer$create(
        ml_framework = framework,
        model_dir = root_path_results_model,
        vocab_raw_texts = example_data$text[1:10],
        vocab_size = 30522,
        vocab_do_lower_case = FALSE,
        max_position_embeddings = 512,
        hidden_size = 256,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 1028,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_probs_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = TRUE,
        pytorch_safetensors = TRUE
      )

      transformer$train(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        raw_texts = example_data$text[1:10],
        p_mask = 0.15,
        p_perm = 0.15,
        whole_word = TRUE,
        val_size = 0.1,
        n_epoch = 2,
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
        trace = TRUE,
        keras_trace = 1,
        pytorch_trace = 1,
        pytorch_safetensors = TRUE
      )
    } else if (method == "roberta") {
      create_roberta_model(
        ml_framework = framework,
        model_dir = root_path_results_model,
        vocab_raw_texts = example_data$text,
        vocab_size = 10000,
        add_prefix_space = TRUE,
        max_position_embeddings = 512,
        hidden_size = 32,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 128,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace
      )
      train_tune_roberta_model(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        raw_texts = example_data$text[1:5],
        p_mask = 0.30,
        val_size = 0.1,
        n_epoch = 2,
        batch_size = 1,
        chunk_size = 70,
        full_sequences_only = TRUE,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        keras_trace = 0,
        trace = trace
      )
    } else if (method == "longformer") {
      create_longformer_model(
        ml_framework = framework,
        model_dir = root_path_results_model,
        vocab_raw_texts = example_data$text[1:500],
        vocab_size = 10000,
        add_prefix_space = FALSE,
        max_position_embeddings = 512,
        hidden_size = 32,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 128,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        attention_window = 40,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace
      )
      train_tune_longformer_model(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        raw_texts = example_data$text[1:5],
        p_mask = 0.30,
        val_size = 0.1,
        n_epoch = 2,
        batch_size = 1,
        chunk_size = 512,
        full_sequences_only = FALSE,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        keras_trace = 0,
        trace = trace
      )
    } else if (method == "funnel") {
      create_funnel_model(
        ml_framework = framework,
        model_dir = root_path_results_model,
        vocab_raw_texts = example_data$text,
        vocab_size = 10000,
        max_position_embeddings = 512,
        hidden_size = 32,
        block_sizes = c(2, 2, 2),
        num_decoder_layers = 2,
        num_attention_heads = 2,
        intermediate_size = 128,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace
      )
      train_tune_funnel_model(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        raw_texts = example_data$text[1:20],
        p_mask = 0.15,
        whole_word = FALSE,
        val_size = 0.1,
        n_epoch = 2,
        batch_size = 2,
        min_seq_len = 50,
        full_sequences_only = TRUE,
        chunk_size = 250,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace,
        keras_trace = 0
      )
    } else if (method == "deberta_v2") {
      create_deberta_v2_model(
        ml_framework = framework,
        model_dir = root_path_results_model,
        vocab_raw_texts = example_data$text,
        vocab_size = 10000,
        do_lower_case = FALSE,
        # add_prefix_space=FALSE,
        max_position_embeddings = 512,
        hidden_size = 32,
        num_hidden_layer = 2,
        num_attention_heads = 2,
        intermediate_size = 128,
        hidden_act = "gelu",
        hidden_dropout_prob = 0.1,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace = trace
      )
      train_tune_deberta_v2_model(
        ml_framework = framework,
        output_dir = root_path_results_model,
        model_dir_path = root_path_results_model,
        raw_texts = example_data$text[1:5],
        p_mask = 0.15,
        whole_word = FALSE,
        val_size = 0.1,
        n_epoch = 2,
        batch_size = 2,
        chunk_size = 100,
        full_sequences_only = FALSE,
        n_workers = 1,
        multi_process = FALSE,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        keras_trace = 0,
        trace = trace
      )
    }
  }
}
