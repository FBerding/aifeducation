testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                  message = "Necessary python modules not available")
aifeducation::set_config_gpu_low_memory()

test_that("create_bert_model", {
  example_data<-data.frame(
    id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
    label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
  example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

  expect_no_error(
    create_bert_model(
    model_dir="tmp",
    vocab_raw_texts=example_data$text,
    vocab_size=30522,
    vocab_do_lower_case=FALSE,
    max_position_embeddings=512,
    hidden_size=768,
    num_hidden_layer=12,
    num_attention_heads=12,
    intermediate_size=3072,
    hidden_act="gelu",
    hidden_dropout_prob=0.1,
    trace=FALSE))

  expect_no_error(
    create_bert_model(
      model_dir="tmp",
      vocab_raw_texts=example_data$text,
      vocab_size=30522,
      vocab_do_lower_case=TRUE,
      max_position_embeddings=512,
      hidden_size=768,
      num_hidden_layer=12,
      num_attention_heads=12,
      intermediate_size=3072,
      hidden_act="gelu",
      hidden_dropout_prob=0.1,
      trace=FALSE))
})



