testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                  message = "Necessary python modules not available")

if(dir.exists(testthat::test_path("test_data/bert"))==FALSE){
  dir.create(testthat::test_path("test_data/bert"))
}

aifeducation::set_config_gpu_low_memory()

test_that("create_bert_model", {
  example_data<-data.frame(
    id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
    label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
  example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

  expect_no_error(
    create_bert_model(
    model_dir=testthat::test_path("test_data/bert"),
    vocab_raw_texts=example_data$text[1:500],
    vocab_size=30522,
    vocab_do_lower_case=FALSE,
    max_position_embeddings=512,
    hidden_size=256,
    num_hidden_layer=2,
    num_attention_heads=8,
    intermediate_size=1024,
    hidden_act="gelu",
    hidden_dropout_prob=0.1,
    sustain_track=TRUE,
    sustain_iso_code = "DEU",
    sustain_region = NULL,
    sustain_interval = 15,
    trace=FALSE))

  expect_no_error(
    create_bert_model(
      model_dir=testthat::test_path("test_data/bert"),
      vocab_raw_texts=example_data$text[1:500],
      vocab_size=30522,
      vocab_do_lower_case=TRUE,
      max_position_embeddings=512,
      hidden_size=256,
      num_hidden_layer=2,
      num_attention_heads=8,
      intermediate_size=1024,
      hidden_act="gelu",
      hidden_dropout_prob=0.1,
      sustain_track=FALSE,
      sustain_iso_code = "DEU",
      sustain_region = NULL,
      sustain_interval = 15,
      trace=FALSE))
})



