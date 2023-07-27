
testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                      message = "Necessary python modules not available")

tmp_path="test_data/roberta"
testthat::skip_if_not(condition=dir.exists(testthat::test_path(tmp_path)),
                      message = "Necessary bert model not available")

if(dir.exists(testthat::test_path("tmp_full_models"))==FALSE){
  dir.create(testthat::test_path("tmp_full_models"))
}

aifeducation::set_config_gpu_low_memory()

#-------------------------------------------------------------------------------
example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id2,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

#-------------------------------------------------------------------------------
bert_modeling<-TextEmbeddingModel$new(
  model_name="roberta_embedding",
  model_label="Text Embedding via RoBERTa",
  model_version="0.0.1",
  model_language="english",
  method = "roberta",
  max_length = 256,
  chunks=4,
  overlap=40,
  aggregation="last",
  model_dir=testthat::test_path(tmp_path))

model_name=bert_modeling$get_model_info()$model_name

test_that("creation_roberta", {
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})

test_that("Saving Model Roberta", {
  expect_no_error(
    bert_modeling$save_model(testthat::test_path("tmp"))
  )
})

test_that("Loading Model Roberta", {
  expect_no_error(
    bert_modeling$load_model(testthat::test_path("tmp"))
  )
})

test_that("embedding_roberta", {
  embeddings<-bert_modeling$embed(raw_text = example_data$text[1:10],
                                  doc_id = example_data$id[1:10])
  expect_s3_class(embeddings, class="EmbeddedText")

  embeddings<-NULL
  embeddings<-bert_modeling$embed(raw_text = example_data$text[1:1],
                                  doc_id = example_data$id[1:1])
  expect_s3_class(embeddings, class="EmbeddedText")
})

test_that("encoding_roberta", {
  encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                  token_encodings_only = TRUE)
  expect_length(encodings,10)
  expect_type(encodings,type="list")
})

test_that("decoding_roberta", {
  encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                  token_encodings_only = TRUE)
  decodings<-bert_modeling$decode(encodings)
  expect_length(decodings,10)
  expect_type(decodings,type="list")
})

test_that("ROBERTA Save Total Model H5", {
  expect_no_error(
    save_ai_model(model=bert_modeling,
                  model_dir = testthat::test_path("tmp_full_models"),
                  save_format = "H5")
  )
})

test_that("ROBERTA Load Total Model H5", {
  bert_modeling<-NULL
  bert_modeling<-load_ai_model(
    model_dir = testthat::test_path(paste0("tmp_full_models/",model_name))
  )
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})

test_that("ROBERTA Save Total Model TF", {
  expect_no_error(
    save_ai_model(model=bert_modeling,
                  model_dir = testthat::test_path("tmp_full_models"),
                  save_format = "tf")
  )
})

test_that("ROBERTA Load Total Model TF", {
  bert_modeling<-NULL
  bert_modeling<-load_ai_model(
    model_dir = testthat::test_path(paste0("tmp_full_models/",model_name))
  )
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})
