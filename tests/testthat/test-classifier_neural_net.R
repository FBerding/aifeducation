testthat::skip_if_not(condition=check_aif_py_modules(),
                      message = "Necessary python modules not available")

#-------------------------------------------------------------------------------
example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

example_targets<-as.factor(example_data$label)
names(example_targets)=example_data$id
#-------------------------------------------------------------------------------

classifier<-TextEmbeddingClassifierNeuralNet$new(
  name="movie_review_classifier",
  label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings=bert_embeddings,
  targets=example_target,
  config=list(
    hidden=c(128,64),
    gru=c(128,128),
    dropout=0.2,
    recurrent_dropout=0.4,
    l2_regularizer=0.001,
    optimizer="adam",
    act_fct="gelu",
    act_fct_last="softmax",
    err_fct="categorical_crossentropy"))

test_that("creation_classifier_neural_net", {
  expect_s3_class(classifier,
                  class="TextEmbeddingClassifierNeuralNet")
})

classifier$train(
  data_embeddings = bert_embeddings,
  data_targets = example_targets,
  data_n_valid_samples=5,
  use_baseline=TRUE,
  bsl_val_size=0.25,
  use_bsc=TRUE,
  bsc_methods=c("dbsmote"),
  bsc_max_k=10,
  use_bpl=TRUE,
  bpl_max_steps=10,
  bpl_inc_ratio=0.25,
  bpl_anchor=0.75,
  bpl_valid_size=0.33,
  opt_model_reset=TRUE,
  epochs=2,
  batch_size=32,
  dir_checkpoint="tmp/checkpoints_classifier",
  trace=TRUE,
  view_metrics=FALSE,
  keras_trace=2,
  n_cores=2)
