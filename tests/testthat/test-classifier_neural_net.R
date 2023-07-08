testthat::skip()

testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                      message  = "Necessary python modules not available")

path="test_data/classifier/bert_embeddings.rda"
testthat::skip_if_not(condition=file.exists(testthat::test_path(path)),
                      message  = "Necessary dataset not available")

#-------------------------------------------------------------------------------
aifeducation::set_config_gpu_low_memory()

example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id2,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)
example_data$label[c(1:500,1001:1750)]=NA
example_data<-example_data[c(501:600,1001:1100,1751:1850),]
example_targets<-as.factor(example_data$label)
names(example_targets)=example_data$id

load(testthat::test_path(path))
current_embeddings<-bert_embeddings$clone(deep = TRUE)
#-------------------------------------------------------------------------------

test_that("creation_classifier_neural_net", {
  classifier<-NULL
  classifier<-TextEmbeddingClassifierNeuralNet$new(
    name="movie_review_classifier",
    label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
    text_embeddings=current_embeddings,
    targets=example_targets,
    hidden=NULL,
    rec=c(28,28),
    self_attention_heads = 0,
    dropout=0.2,
    recurrent_dropout=0.4,
    l2_regularizer=0.01,
    optimizer="adam",
    act_fct="gelu",
    rec_act_fct="tanh")
  expect_s3_class(classifier,
                  class="TextEmbeddingClassifierNeuralNet")

  classifier<-NULL
  classifier<-TextEmbeddingClassifierNeuralNet$new(
    name="movie_review_classifier",
    label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
    text_embeddings=current_embeddings,
    targets=example_targets,
    hidden=c(28,28),
    rec=NULL,
    dropout=0.2,
    recurrent_dropout=0.4,
    l2_regularizer=0.01,
    optimizer="adam",
    act_fct="gelu",
    rec_act_fct="tanh")
  expect_s3_class(classifier,
                  class="TextEmbeddingClassifierNeuralNet")

  classifier<-NULL
  classifier<-TextEmbeddingClassifierNeuralNet$new(
    name="movie_review_classifier",
    label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
    text_embeddings=current_embeddings,
    targets=example_targets,
    hidden=c(28,28),
    rec=c(28,28),
    self_attention_heads=2,
    dropout=0.2,
    recurrent_dropout=0.4,
    l2_regularizer=0.01,
    optimizer="adam",
    act_fct="gelu",
    rec_act_fct="tanh")
  expect_s3_class(classifier,
                  class="TextEmbeddingClassifierNeuralNet")

  classifier<-NULL
  classifier<-TextEmbeddingClassifierNeuralNet$new(
    name="movie_review_classifier",
    label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
    text_embeddings=current_embeddings,
    targets=example_targets,
    hidden=c(28,28),
    rec=NULL,
    self_attention_heads=2,
    dropout=0.2,
    recurrent_dropout=0.4,
    l2_regularizer=0.01,
    optimizer="adam",
    act_fct="gelu",
    rec_act_fct="tanh")
  expect_s3_class(classifier,
                  class="TextEmbeddingClassifierNeuralNet")

  classifier<-NULL
  classifier<-TextEmbeddingClassifierNeuralNet$new(
    name="movie_review_classifier",
    label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
    text_embeddings=current_embeddings,
    targets=example_targets,
    hidden=NULL,
    rec=NULL,
    self_attention_heads=2,
    dropout=0.2,
    recurrent_dropout=0.4,
    l2_regularizer=0.01,
    optimizer="adam",
    act_fct="gelu",
    rec_act_fct="tanh")
  expect_s3_class(classifier,
                  class="TextEmbeddingClassifierNeuralNet")
})


#-------------------------------------------------------------------------------
classifier<-TextEmbeddingClassifierNeuralNet$new(
  name="movie_review_classifier",
  label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings=current_embeddings,
  targets=example_targets,
  hidden=NULL,
  rec=c(28,28),
  self_attention_heads = 0,
  dropout=0.2,
  recurrent_dropout=0.4,
  l2_regularizer=0.01,
  optimizer="adam",
  act_fct="gelu",
  rec_act_fct="tanh")

test_that("training_baseline_only", {
  expect_no_error(
    classifier$train(
      data_embeddings = current_embeddings,
      data_targets = example_targets,
      data_n_test_samples=2,
      use_baseline=TRUE,
      bsl_val_size=0.25,
      use_bsc=FALSE,
      bsc_methods=c("dbsmote"),
      bsc_max_k=10,
      bsc_val_size=0.25,
      use_bpl=FALSE,
      bpl_max_steps=2,
      bpl_epochs_per_step=1,
      bpl_dynamic_inc=FALSE,
      bpl_balance=TRUE,
      bpl_max=1.00,
      bpl_anchor=1.00,
      bpl_min=0.00,
      bpl_weight_inc=0.02,
      bpl_weight_start=0.00,
      bpl_model_reset=FALSE,
      epochs=2,
      batch_size=32,
      dir_checkpoint=testthat::test_path("test_data/tmp"),
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
      data_n_test_samples=2,
      use_baseline=FALSE,
      bsl_val_size=0.25,
      use_bsc=TRUE,
      bsc_methods=c("dbsmote"),
      bsc_max_k=10,
      bsc_val_size=0.25,
      use_bpl=FALSE,
      bpl_max_steps=2,
      bpl_epochs_per_step=1,
      bpl_dynamic_inc=FALSE,
      bpl_balance=FALSE,
      bpl_max=1.00,
      bpl_anchor=1.00,
      bpl_min=0.00,
      bpl_weight_inc=0.02,
      bpl_weight_start=0.00,
      bpl_model_reset=FALSE,
      epochs=2,
      batch_size=32,
      dir_checkpoint=testthat::test_path("test_data/tmp"),
      trace=FALSE,
      view_metrics=FALSE,
      keras_trace=0,
      n_cores=1)
  )
})

test_that("training_pbl_baseline", {
  expect_no_error(
    classifier$train(
      data_embeddings = current_embeddings,
      data_targets = example_targets,
      data_n_test_samples=2,
      use_baseline=TRUE,
      bsl_val_size=0.25,
      use_bsc=FALSE,
      bsc_methods=c("dbsmote"),
      bsc_max_k=10,
      bsc_val_size=0.25,
      use_bpl=TRUE,
      bpl_max_steps=2,
      bpl_epochs_per_step=1,
      bpl_dynamic_inc=FALSE,
      bpl_balance=TRUE,
      bpl_max=1.00,
      bpl_anchor=1.00,
      bpl_min=0.00,
      bpl_weight_inc=0.02,
      bpl_weight_start=0.00,
      bpl_model_reset=FALSE,
      epochs=2,
      batch_size=32,
      dir_checkpoint=testthat::test_path("test_data/tmp"),
      trace=FALSE,
      view_metrics=FALSE,
      keras_trace=0,
      n_cores=1)
  )
})

test_that("training_pbl_bsc", {
  expect_no_error(
    classifier$train(
      data_embeddings = current_embeddings,
      data_targets = example_targets,
      data_n_test_samples=2,
      use_baseline=FALSE,
      bsl_val_size=0.25,
      use_bsc=TRUE,
      bsc_methods=c("dbsmote"),
      bsc_max_k=10,
      bsc_val_size=0.25,
      use_bpl=TRUE,
      bpl_max_steps=2,
      bpl_epochs_per_step=1,
      bpl_dynamic_inc=FALSE,
      bpl_balance=TRUE,
      bpl_max=1.00,
      bpl_anchor=1.00,
      bpl_min=0.00,
      bpl_weight_inc=0.02,
      bpl_weight_start=0.00,
      bpl_model_reset=FALSE,
      epochs=2,
      batch_size=32,
      dir_checkpoint=testthat::test_path("test_data/tmp"),
      trace=FALSE,
      view_metrics=FALSE,
      keras_trace=0,
      n_cores=1)
  )
})

test_that("prediction", {
  prediction<-classifier$predict(newdata = current_embeddings,
                                 batch_size = 2,
                                 verbose = 0)
  expect_equal(object=nrow(prediction),
               expected = dim(current_embeddings$embeddings)[[1]])

})

test_that("prediction_single_case", {
  single_embedding<-current_embeddings$clone(deep = TRUE)
  single_embedding$embeddings<-single_embedding$embeddings[1,,,drop=FALSE]
  prediction<-classifier$predict(newdata = single_embedding,
                                 batch_size = 2,
                                 verbose = 0)
  expect_equal(object=nrow(prediction),
               expected = 1)
})

test_that("descriptions", {
  classifier$set_model_description(
    eng = "Description",
    native = "Beschreibung",
    abstract_eng = "Abstract",
    abstract_native = "Zusammenfassung",
    keywords_eng = c("Test","Neural Net"),
    keywords_native = c("Test","Neuronales Netz")
  )
  desc<-classifier$get_model_description()
  expect_equal(
    object=desc$eng,
    expected="Description"
  )
  expect_equal(
    object=desc$native,
    expected="Beschreibung"
  )
  expect_equal(
    object=desc$abstract_eng,
    expected="Abstract"
  )
  expect_equal(
    object=desc$abstract_native,
    expected="Zusammenfassung"
  )
  expect_equal(
    object=desc$keywords_eng,
    expected=c("Test","Neural Net")
  )
  expect_equal(
    object=desc$keywords_native,
    expected=c("Test","Neuronales Netz")
  )
})

test_that("license", {
  classifier$set_license("test_license")
  expect_equal(
    object=classifier$get_license(),
    expected=c("test_license")
  )
})

test_that("publication_info",{
  classifier$set_publication_info(
    authors = personList(
      person(given="Max",family="Mustermann")
    ),
    citation="Test Classifier",
    url="https://Test.html"
  )
  pub_info=classifier$get_publication_info()
  expect_equal(
    object=pub_info$developed_by$authors,
    expected=personList(
      person(given="Max",family="Mustermann")
    )
  )

  expect_equal(
    object=pub_info$developed_by$citation,
    expected="Test Classifier"
  )

  expect_equal(
    object=pub_info$developed_by$url,
    expected="https://Test.html"
  )
})
