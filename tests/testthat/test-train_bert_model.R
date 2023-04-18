testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace=FALSE),
                  message = "Necessary python modules not available")
test_that("create_bert_model", {
  example_data<-data.frame(
    id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
    label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
  example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

  expect_no_error(
    train_tune_bert_model(output_dir="tmp",
                          bert_model_dir_path="tmp",
                          raw_texts= example_data$text[1:25],
                          aug_vocab_by=0,
                          p_mask=0.30,
                          whole_word=TRUE,
                          val_size=0.1,
                          n_epoch=1,
                          batch_size=2,
                          chunk_size=250,
                          n_workers=1,
                          multi_process=FALSE,
                          trace=TRUE))
  expect_no_error(
    train_tune_bert_model(output_dir="tmp",
                          bert_model_dir_path="tmp",
                          raw_texts= example_data$text[1:25],
                          aug_vocab_by=100,
                          p_mask=0.30,
                          whole_word=TRUE,
                          val_size=0.1,
                          n_epoch=1,
                          batch_size=2,
                          chunk_size=250,
                          n_workers=1,
                          multi_process=FALSE,
                          trace=TRUE))

  expect_no_error(
    train_tune_bert_model(output_dir="tmp",
                          bert_model_dir_path="tmp",
                          raw_texts= example_data$text[1:25],
                          aug_vocab_by=0,
                          p_mask=0.30,
                          whole_word=FALSE,
                          val_size=0.1,
                          n_epoch=1,
                          batch_size=2,
                          chunk_size=250,
                          n_workers=1,
                          multi_process=FALSE,
                          trace=TRUE))
})



