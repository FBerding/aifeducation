testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                  message = "Necessary python modules not available")

if(dir.exists(testthat::test_path("test_data/roberta"))==FALSE){
  dir.create(testthat::test_path("test_data/roberta"))
}

aifeducation::set_config_gpu_low_memory()

test_that("create_roberta_model", {
  example_data<-data.frame(
    id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
    label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
  example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

  expect_no_error(
    create_roberta_model(
    model_dir=testthat::test_path("test_data/roberta"),
    vocab_raw_texts=example_data$text,
    vocab_size=30522,
    add_prefix_space=FALSE,
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
    create_roberta_model(
      model_dir=testthat::test_path("test_data/roberta"),
      vocab_raw_texts=example_data$text,
      vocab_size=30522,
      add_prefix_space=TRUE,
      max_position_embeddings=512,
      hidden_size=256,
      num_hidden_layer=2,
      num_attention_heads=8,
      intermediate_size=1024,
      hidden_act="gelu",
      hidden_dropout_prob=0.1,
      trace=FALSE))
})



