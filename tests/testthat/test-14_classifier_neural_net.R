testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                      message  = "Necessary python modules not available")

path="test_data/classifier/bert_embeddings.rda"
testthat::skip_if_not(condition=file.exists(testthat::test_path(path)),
                      message  = "Necessary dataset not available")

if(dir.exists(testthat::test_path("test_artefacts"))==FALSE){
  dir.create(testthat::test_path("test_artefacts"))
}

if(dir.exists(testthat::test_path("test_artefacts/tmp_full_models"))==FALSE){
  dir.create(testthat::test_path("test_artefacts/tmp_full_models"))
}

if(dir.exists(testthat::test_path("test_artefacts/classifier"))==FALSE){
  dir.create(testthat::test_path("test_artefacts/classifier"))
}

if(dir.exists(testthat::test_path("test_artefacts/tmp"))==FALSE){
  dir.create(testthat::test_path("test_artefacts/tmp"))
}



#-------------------------------------------------------------------------------
aifeducation::set_config_gpu_low_memory()

load(testthat::test_path(path))
current_embeddings<-bert_embeddings$clone(deep = TRUE)
for (n_classes in 2:3){
  example_data<-data.frame(
    id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id2,
    label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
  example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)
  example_data$label<-as.character(example_data$label)

  rownames(example_data)<-example_data$id
  example_data<-example_data[intersect(
    rownames(example_data),rownames(current_embeddings$embeddings)),]

  example_data$label[c(201:300)]=NA
  if(n_classes>2){
    example_data$label[c(201:250)]<-"medium"
  }
  example_targets<-as.factor(example_data$label)
  names(example_targets)=example_data$id


  #-------------------------------------------------------------------------------

  test_that(paste("creation_classifier_neural_net","n_classes",n_classes), {
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

  test_that(paste("training_baseline_only","n_classes",n_classes), {
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
        sustain_track=TRUE,
        sustain_iso_code = "DEU",
        epochs=2,
        batch_size=32,
        dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
        trace=FALSE,
        keras_trace=0,
        n_cores=1)
    )
  })

  test_that(paste("training_bsc_only","n_classes",n_classes), {
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
        dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
        sustain_track=FALSE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace=FALSE,
        keras_trace=0,
        n_cores=1)
    )
  })

  test_that(paste("training_pbl_baseline","n_classes",n_classes), {
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
        dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
        sustain_track=FALSE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace=FALSE,
        keras_trace=0,
        n_cores=1)
    )
  })

  test_that(paste("training_pbl_bsc","n_classes",n_classes), {
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
        dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
        sustain_track=FALSE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        trace=FALSE,
        keras_trace=0,
        n_cores=1)
    )
  })
}

test_that("Saving Classifier H5",{
  expect_no_error(classifier$save_model(
    testthat::test_path("test_artefacts/tmp"),
    save_format = "h5")
  )
})

test_that("Loading Classifier H5",{
  expect_no_error(
    classifier$load_model(
      testthat::test_path("test_artefacts/tmp"))
  )
})

test_that("Saving Classifier TF",{
  expect_no_error(classifier$save_model(
    testthat::test_path("test_artefacts/tmp"),
    save_format = "tf")
  )
})

test_that("Loading Classifier TF",{
  expect_no_error(
    classifier$load_model(
      testthat::test_path("test_artefacts/tmp"))
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

test_that("software_license", {
  classifier$set_software_license("test_license")
  expect_equal(
    object=classifier$get_software_license(),
    expected=c("test_license")
  )
})

test_that("documentation_license", {
  classifier$set_documentation_license("test_license")
  expect_equal(
    object=classifier$get_documentation_license(),
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

test_that("Classifier Save Total Model H5", {
  expect_no_error(
    save_ai_model(model=classifier,
                  model_dir = testthat::test_path("test_artefacts/tmp_full_models"),
                  save_format = "H5")
  )
})

test_that("Classifier Load Total Model H5", {
  new_classifier<-NULL
  new_classifier<-load_ai_model(
    model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models/",classifier$get_model_info()$model_name))
  )
  expect_s3_class(new_classifier,
                  class="TextEmbeddingClassifierNeuralNet")
})

test_that("Classifier Save Total Model TF with ID", {
  expect_no_error(
    save_ai_model(model=classifier,
                  model_dir = testthat::test_path("test_artefacts/tmp_full_models"),
                  save_format = "tf")
  )
})

test_that("Classifier Load Total Model TF with ID", {
  new_classifier<-NULL
  new_classifier<-load_ai_model(
    model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models/",classifier$get_model_info()$model_name))
  )
  expect_s3_class(new_classifier,
                  class="TextEmbeddingClassifierNeuralNet")
})

test_that("Classifier Save Total Model TF without ID", {
  expect_no_error(
    save_ai_model(model=classifier,
                  model_dir = testthat::test_path("test_artefacts/tmp_full_models"),
                  save_format = "tf",
                  append_ID=FALSE)
  )
})

test_that("Classifier Load Total Model TF without ID", {
  new_classifier<-NULL
  new_classifier<-load_ai_model(
    model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models/",classifier$get_model_info()$model_name_root))
  )
  expect_s3_class(new_classifier,
                  class="TextEmbeddingClassifierNeuralNet")
})

test_that("Classifier Predict", {
  pred<-NULL
  pred<-classifier$predict(
    newdata = current_embeddings,
    batch_size = 2,
    verbose = 0
  )
  expect_equal(nrow(pred),nrow(current_embeddings$embeddings))

  pred<-NULL
  pred<-classifier$predict(
    newdata = current_embeddings$embeddings,
    batch_size = 2,
    verbose = 0
  )
  expect_equal(nrow(pred),nrow(current_embeddings$embeddings))

  pred<-NULL
  pred<-classifier$predict(
    newdata = current_embeddings$embeddings[1,,,drop=FALSE],
    batch_size = 2,
    verbose = 0
  )
  expect_equal(nrow(pred),1)

})
