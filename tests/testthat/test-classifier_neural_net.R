testthat::skip_if_not(condition=check_aif_py_modules(),
                      message  = "Necessary python modules not available")

#-------------------------------------------------------------------------------
example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id2,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)
example_data$label[c(1:500,1001:1750)]=NA
example_targets<-as.factor(example_data$label)
names(example_targets)=example_data$id

current_embeddings<-bert_embeddings$clone(deep = TRUE)
#-------------------------------------------------------------------------------

classifier<-TextEmbeddingClassifierNeuralNet$new(
  name="movie_review_classifier",
  label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings=current_embeddings,
  targets=example_targets,
  hidden=NULL,
  rec=c(28,28),
  dropout=0.2,
  recurrent_dropout=0.4,
  l2_regularizer=0.001,
  optimizer="adam",
  act_fct="gelu",
  rec_act_fct="tanh")

test_that("creation_classifier_neural_net", {
  expect_s3_class(classifier,
                  class="TextEmbeddingClassifierNeuralNet")
})

test_that("training_baseline_only", {
  expect_no_error(
    classifier$train(
      data_embeddings = current_embeddings,
      data_targets = example_targets,
      data_n_valid_samples=2,
      use_baseline=TRUE,
      bsl_val_size=0.25,
      use_bsc=FALSE,
      bsc_methods=c("dbsmote"),
      bsc_max_k=5,
      use_bpl=FALSE,
      bpl_max_steps=2,
      bpl_inc_ratio=0.25,
      bpl_anchor=0.75,
      bpl_valid_size=0.33,
      bpl_model_reset=FALSE,
      epochs=9,
      batch_size=32,
      dir_checkpoint="tmp/checkpoints_classifier",
      trace=FALSE,
      view_metrics=FALSE,
      keras_trace=0,
      n_cores=1)
  )
})

test_that("training_bsc_only", {
  expect_no_error(
    classifier$train(
      data_embeddings = current_embeddings,
      data_targets = example_targets,
      data_n_valid_samples=2,
      use_baseline=FALSE,
      bsl_val_size=0.25,
      use_bsc=TRUE,
      bsc_methods=c("dbsmote"),
      bsc_max_k=5,
      use_bpl=FALSE,
      bpl_max_steps=2,
      bpl_inc_ratio=0.25,
      bpl_anchor=0.75,
      bpl_valid_size=0.33,
      bpl_model_reset=FALSE,
      epochs=9,
      batch_size=32,
      dir_checkpoint="tmp/checkpoints_classifier",
      trace=FALSE,
      view_metrics=FALSE,
      keras_trace=0,
      n_cores=1)
  )
})

test_that("training_pbl_only", {
  expect_no_error(
    classifier$train(
      data_embeddings = current_embeddings,
      data_targets = example_targets,
      data_n_valid_samples=2,
      use_baseline=FALSE,
      bsl_val_size=0.25,
      use_bsc=FALSE,
      bsc_methods=c("dbsmote"),
      bsc_max_k=5,
      use_bpl=TRUE,
      bpl_max_steps=2,
      bpl_inc_ratio=0.25,
      bpl_anchor=0.75,
      bpl_valid_size=0.33,
      bpl_model_reset=FALSE,
      epochs=30,
      batch_size=32,
      dir_checkpoint="tmp/checkpoints_classifier",
      trace=FALSE,
      view_metrics=FALSE,
      keras_trace=0,
      n_cores=2)
  )
})




