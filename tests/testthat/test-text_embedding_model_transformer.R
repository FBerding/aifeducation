testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(),
                      message = "Necessary python modules not available")

tmp_path="language_models/bert-base-uncased"
testthat::skip_if_not(condition=dir.exists(tmp_path),
                      message = "Necessary bert model not available")

#-------------------------------------------------------------------------------
example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id2,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

#-------------------------------------------------------------------------------
bert_modeling<-TextEmbeddingModel$new(
  model_name="bert_embedding",
  model_label="Text Embedding via BERT",
  model_version="0.0.1",
  model_language="english",
  method = "bert",
  max_length = 512,
  chunks=15,
  overlap=4,
  aggregation="last",
  use_cls_token=TRUE,
  model_dir=tmp_path)

test_that("creation_bert", {
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})

test_that("embedding_bertg", {
  embeddings<-bert_modeling$embed(raw_text = example_data$text[1:10],
                                        doc_id = example_data$id[1:10])
  expect_s3_class(embeddings, class="EmbeddedText")
})

test_that("encoding_bert", {
  encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                  token_encodings_only = TRUE)
  expect_length(encodings,10)
  expect_type(encodings,type="list")
})

test_that("decoding_bert", {
  encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                  token_encodings_only = TRUE)
  decodings<-bert_modeling$decode(encodings)
  expect_length(decodings,10)
  expect_type(decodings,type="list")
})

