# This file does not contain any tests. It is used for creating FeatureExtractors
# that can be used for testing Classifiers
testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE, check = "pytorch"),
  message = "Necessary python modules not available"
)

#Config transformer library
transformers$utils$logging$set_verbosity_error()
os$environ$setdefault("TOKENIZERS_PARALLELISM", "false")

# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

#config trace
trace=FALSE

#config ai method
ai_method="bert"

root_path_output=testthat::test_path("test_data_tmp")
test_path_create=paste0(root_path_output,"/transformer_create")
test_path_train=paste0(root_path_output,"/transformer_train")
path_test_data=testthat::test_path("test_data_tmp/Embeddings")

create_dir(root_path_output, FALSE)
create_dir(test_path_create, FALSE)
create_dir(test_path_train, FALSE)
create_dir(path_test_data,FALSE)

test_that("Generating Test Data", {
  train_data=LargeDataSetForText$new(imdb_movie_reviews)

  base_model<-aife_transformer_maker$make(ai_method)
  base_model$create(
    ml_framework = "pytorch",
    model_dir = test_path_create,
    text_dataset = train_data,
    vocab_size = 30000,
    vocab_do_lower_case = FALSE,
    max_position_embeddings = 512,
    hidden_size = 64,
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

  Sys.sleep(5)

  base_model$train(
    ml_framework = "pytorch",
    output_dir = test_path_train,
    model_dir_path = test_path_create,
    text_dataset = train_data,
    p_mask = 0.15,
    whole_word = TRUE,
    full_sequences_only = TRUE,
    val_size = 0.25,
    n_epoch = 10,
    batch_size = 25,
    chunk_size = 512,
    n_workers = 1,
    multi_process = FALSE,
    sustain_track = TRUE,
    sustain_iso_code = "DEU",
    sustain_region = NULL,
    sustain_interval = 15,
    trace = trace,
    keras_trace = as.numeric(trace),
    pytorch_trace = as.numeric(trace)
  )
  Sys.sleep(5)

  #Clean data
  unlink(x=test_path_create,
         recursive = TRUE)

  Sys.sleep(5)

  text_embedding_model <- TextEmbeddingModel$new()
  text_embedding_model$configure(
    model_name = "text_embedding_model_for_test",
    model_label = "Text Embedding for Test",
    model_language = "english",
    method = ai_method,
    ml_framework = "pytorch",
    max_length = 512,
    chunks = 6,
    overlap = 10,
    emb_layer_min = 1,
    emb_layer_max = 2,
    emb_pool_type = "average",
    model_dir = test_path_train
  )

  embeddings <- text_embedding_model$embed_large(train_data,trace=trace)
  embeddings<-embeddings$convert_to_EmbeddedText()
  save_to_disk(object = embeddings,
               dir_path = path_test_data,
               folder_name = "imdb_embeddings")

  Clean data
  unlink(x=test_path_train,
         recursive = TRUE)

  #Save test data
  expect_true(file.exists(paste0(path_test_data,"/imdb_embeddings/r_config_state.rda")))

})
