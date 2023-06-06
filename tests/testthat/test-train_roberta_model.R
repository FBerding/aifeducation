testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace=FALSE),
                  message = "Necessary python modules not available")
testthat::skip_if_not(condition = dir.exists(testthat::test_path("test_data/roberta")),
                      message = "Necessary bert model not available")
aifeducation::set_config_gpu_low_memory()

test_that("train_tune_roberta_model", {
  example_data<-data.frame(
    id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
    label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
  example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

  expect_no_error(
    train_tune_roberta_model(output_dir=testthat::test_path("test_data/roberta"),
                          model_dir_path=testthat::test_path("test_data/roberta"),
                          raw_texts= example_data$text[1:5],
                          p_mask=0.30,
                          val_size=0.1,
                          n_epoch=1,
                          batch_size=1,
                          chunk_size=100,
                          n_workers=1,
                          multi_process=FALSE,
                          trace=FALSE))
})



