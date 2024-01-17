testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                      message  = "Necessary python modules not available")

#if(aifeducation_config$global_framework_set()==FALSE){
#  aifeducation_config$set_global_ml_backend("tensorflow")
#}

aifeducation::set_config_gpu_low_memory()
set_config_tf_logger("ERROR")
set_config_os_environ_logger("ERROR")

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

if(dir.exists(testthat::test_path("test_artefacts/tmp/2_classes"))==FALSE){
  dir.create(testthat::test_path("test_artefacts/tmp/2_classes"))
}

if(dir.exists(testthat::test_path("test_artefacts/tmp/3_classes"))==FALSE){
  dir.create(testthat::test_path("test_artefacts/tmp/3_classes"))
}

folder_list=c("keras","pytorch")

for (folder in folder_list){
  if(dir.exists(testthat::test_path(paste0("test_artefacts/tmp_full_models_",folder)))==FALSE){
    dir.create(testthat::test_path(paste0("test_artefacts/tmp_full_models_",folder)))
  }

  if(dir.exists(testthat::test_path(paste0("test_artefacts/tmp_",folder)))==FALSE){
    dir.create(testthat::test_path(paste0("test_artefacts/tmp_",folder)))
  }

  if(dir.exists(testthat::test_path(paste0("test_artefacts/tmp_",folder,"/2_classes")))==FALSE){
    dir.create(testthat::test_path(paste0("test_artefacts/tmp_",folder,"/2_classes")))
  }

  if(dir.exists(testthat::test_path(paste0("test_artefacts/tmp_",folder,"/3_classes")))==FALSE){
    dir.create(testthat::test_path(paste0("test_artefacts/tmp_",folder,"/3_classes")))
  }
}

ml_frameworks=c("tensorflow","pytorch")
#ml_frameworks=c("pytorch")

#-------------------------------------------------------------------------------
aifeducation::set_config_gpu_low_memory()
load(testthat::test_path(path))
current_embeddings<-bert_embeddings$clone(deep = TRUE)

for(framework in ml_frameworks){
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
    #Test Creation of the models

    rec_list=list(NULL,c(28),c(28,14))
    hidden_list=list(NULL,c(27),c(27,13))
    r_encoder_list=list(0,1,2)
    attention_list=list("fourier","multihead")
    pos_embedding_list=list(TRUE,FALSE)


      for(rec in rec_list){
        for(hidden in hidden_list){
          for(r in r_encoder_list){
            for(attention in attention_list){
              for(pos_embedding in pos_embedding_list){
                test_that(paste(framework,"creation_classifier_neural_net","n_classes",n_classes,
                                "rec",paste(rec,collapse = "_"),"hidden",paste(hidden,collapse = "_"),"encoder",r,"attention",attention,"pos",pos_embedding), {
                  classifier<-NULL
                  classifier<-TextEmbeddingClassifierNeuralNet$new(
                    ml_framework = framework,
                    name="movie_review_classifier",
                    label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
                    text_embeddings=current_embeddings,
                    targets=example_targets,
                    hidden=hidden,
                    rec=rec,
                    self_attention_heads = 2,
                    add_pos_embedding = pos_embedding,
                    attention_type =  attention,
                    encoder_dropout = 0.1,
                    repeat_encoder = r,
                    recurrent_dropout=0.4)

                  expect_s3_class(classifier,
                                  class="TextEmbeddingClassifierNeuralNet")

                  predictions=classifier$predict(
                    newdata = current_embeddings$embeddings[1:3,,],
                    batch_size = 2,
                    verbose = 0)
                  expect_equal(object = length(predictions$expected_category),expected = 3)

                  expect_false(classifier$get_sustainability_data()$sustainability_tracked)
                })
              }
            }
          }
        }
      }

    #-------------------------------------------------------------------------------
    classifier<-NULL
    classifier<-TextEmbeddingClassifierNeuralNet$new(
      ml_framework = framework,
      name=paste0("movie_review_classifier_","classes_",n_classes),
      label="Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
      text_embeddings=current_embeddings,
      targets=example_targets,
      hidden=NULL,
      rec=c(3,3),
      self_attention_heads = 0,
      attention_type = "fourier",
      recurrent_dropout=0.4,
      optimizer="adam")

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"training_baseline_only","n_classes",n_classes), {
      expect_no_error(
        classifier$train(
          data_embeddings = current_embeddings,
          data_targets = example_targets,
          data_n_test_samples=2,
          balance_class_weights=TRUE,
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
          batch_size=4,
          dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
          trace=FALSE,
          keras_trace=0,
          pytorch_trace=0,
          n_cores=1)
      )

      expect_true(classifier$get_sustainability_data()$sustainability_tracked)

    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"training_bsc_only","n_classes",n_classes), {
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
          batch_size=4,
          dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
          sustain_track=FALSE,
          sustain_iso_code = "DEU",
          sustain_region = NULL,
          sustain_interval = 15,
          trace=FALSE,
          keras_trace=0,
          pytorch_trace=0,
          n_cores=1)
      )

      expect_false(classifier$get_sustainability_data()$sustainability_tracked)
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"training_pbl_baseline","n_classes",n_classes), {
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
          batch_size=4,
          dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
          sustain_track=FALSE,
          sustain_iso_code = "DEU",
          sustain_region = NULL,
          sustain_interval = 15,
          trace=FALSE,
          keras_trace=0,
          pytorch_trace=0,
          n_cores=1)
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"training_pbl_bsc","n_classes",n_classes), {
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
          batch_size=4,
          dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
          sustain_track=FALSE,
          sustain_iso_code = "DEU",
          sustain_region = NULL,
          sustain_interval = 15,
          trace=FALSE,
          keras_trace=0,
          pytorch_trace=0,
          n_cores=1)
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"training_pbl_bsc","n_classes",n_classes), {
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
          bpl_balance=FALSE,
          bpl_max=1.00,
          bpl_anchor=1.00,
          bpl_min=0.00,
          bpl_weight_inc=0.02,
          bpl_weight_start=0.00,
          bpl_model_reset=FALSE,
          epochs=2,
          batch_size=4,
          dir_checkpoint=testthat::test_path("test_artefacts/classifier"),
          sustain_track=FALSE,
          sustain_iso_code = "DEU",
          sustain_region = NULL,
          sustain_interval = 15,
          trace=FALSE,
          keras_trace=0,
          pytorch_trace=0,
          n_cores=1)
      )
    })
  }

  if(framework=="tensorflow"){
    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Saving Classifier Keras_V3"),{
      expect_no_error(classifier$save_model(
        testthat::test_path(paste0("test_artefacts/tmp_keras/",n_classes,"_classes")),
        save_format = "keras")
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Loading Classifier Keras_V3"),{
      expect_no_error(
        classifier$load_model(
          ml_framework="tensorflow",
          dir_path=testthat::test_path(paste0("test_artefacts/tmp_keras/",n_classes,"_classes")))
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Saving Classifier H5"),{
      expect_no_error(classifier$save_model(
        testthat::test_path(paste0("test_artefacts/tmp/",n_classes,"_classes")),
        save_format = "h5")
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Loading Classifier H5"),{
      expect_no_error(
        classifier$load_model(
          ml_framework="tensorflow",
          dir_path=testthat::test_path(paste0("test_artefacts/tmp/",n_classes,"_classes")))
      )
    })

    #------------------------------------------------------------------------------
    base::gc(verbose = FALSE,full = TRUE)

    test_that(paste(framework,"Saving Classifier TF"),{
      expect_no_error(classifier$save_model(
        testthat::test_path(paste0("test_artefacts/tmp/",n_classes,"_classes")),
        save_format = "tf")
      )
    })

    base::gc(verbose = FALSE,full = TRUE)

    test_that(paste(framework,"Loading Classifier TF"),{
      expect_no_error(
        classifier$load_model(
          ml_framework="tensorflow",
          dir_path=testthat::test_path(paste0("test_artefacts/tmp/",n_classes,"_classes")))
      )
    })
  } else if (framework=="pytorch"){
    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Saving Classifier pytorch"),{
      expect_no_error(classifier$save_model(
        testthat::test_path(paste0("test_artefacts/tmp_pytorch/",n_classes,"_classes")),
        save_format = "default")
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Loading Classifier pytorch"),{
      expect_no_error(
        classifier$load_model(
          ml_framework="pytorch",
          dir_path=testthat::test_path(paste0("test_artefacts/tmp_pytorch/",n_classes,"_classes")))
      )
    })
  }

  #------------------------------------------------------------------------------

  base::gc(verbose = FALSE,full = TRUE)
  test_that(paste(framework,"prediction"), {
    prediction<-classifier$predict(newdata = current_embeddings,
                                   batch_size = 2,
                                   verbose = 0)
    expect_equal(object=nrow(prediction),
                 expected = dim(current_embeddings$embeddings)[[1]])

  })

  base::gc(verbose = FALSE,full = TRUE)
  test_that(paste(framework,"prediction_single_case"), {
    single_embedding<-current_embeddings$clone(deep = TRUE)
    single_embedding$embeddings<-single_embedding$embeddings[1,,,drop=FALSE]
    prediction<-classifier$predict(newdata = single_embedding,
                                   batch_size = 2,
                                   verbose = 0)
    expect_equal(object=nrow(prediction),
                 expected = 1)
  })

  test_that(paste(framework,"descriptions"), {
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

  test_that(paste(framework,"software_license"), {
    classifier$set_software_license("test_license")
    expect_equal(
      object=classifier$get_software_license(),
      expected=c("test_license")
    )
  })

  test_that(paste(framework,"documentation_license"), {
    classifier$set_documentation_license("test_license")
    expect_equal(
      object=classifier$get_documentation_license(),
      expected=c("test_license")
    )
  })

  test_that(paste(framework,"publication_info"),{
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


#-------------------------------------------------------------------------------
  if(framework=="tensorflow"){
    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Save Total Model keras_V3 without ID"), {
      expect_no_error(
        save_ai_model(model=classifier,
                      model_dir = testthat::test_path("test_artefacts/tmp_full_models_keras"),
                      save_format = "keras",
                      append_ID = FALSE)
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Load Total Model keras_V3 without ID"), {
      new_classifier<-NULL
      new_classifier<-load_ai_model(
        ml_framework="tensorflow",
        model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models_keras/",classifier$get_model_info()$model_name_root))
      )
      expect_s3_class(new_classifier,
                      class="TextEmbeddingClassifierNeuralNet")
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Save Total Model keras_V3"), {
      expect_no_error(
        save_ai_model(model=classifier,
                      model_dir = testthat::test_path("test_artefacts/tmp_full_models_keras"),
                      save_format = "keras")
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Load Total Model keras_V3 with ID"), {
      new_classifier<-NULL
      new_classifier<-load_ai_model(
        ml_framework="tensorflow",
        model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models_keras/",classifier$get_model_info()$model_name))
      )
      expect_s3_class(new_classifier,
                      class="TextEmbeddingClassifierNeuralNet")
    })

    #------------------------------------------------------------------------------
    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Save Total Model H5"), {
      expect_no_error(
        save_ai_model(model=classifier,
                      model_dir = testthat::test_path("test_artefacts/tmp_full_models"),
                      save_format = "H5")
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Load Total Model H5"), {
      new_classifier<-NULL
      new_classifier<-load_ai_model(
        ml_framework="tensorflow",
        model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models/",classifier$get_model_info()$model_name))
      )
      expect_s3_class(new_classifier,
                      class="TextEmbeddingClassifierNeuralNet")
    })

    #----------------------------------------------------------------------------------
    base::gc(verbose = FALSE,full = TRUE)

    test_that(paste(framework,"Classifier Save Total Model TF with ID"), {
      testthat::skip_on_os("linux")
      expect_no_error(
        save_ai_model(model=classifier,
                      model_dir = testthat::test_path("test_artefacts/tmp_full_models"),
                      save_format = "tf")
      )
    })
    base::gc(verbose = FALSE,full = TRUE)

    test_that(paste(framework,"Classifier Load Total Model TF with ID"), {
      testthat::skip_on_os("linux")
      new_classifier<-NULL
      new_classifier<-load_ai_model(
        ml_framework="tensorflow",
        model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models/",classifier$get_model_info()$model_name))
      )
      expect_s3_class(new_classifier,
                      class="TextEmbeddingClassifierNeuralNet")
    })
    #-------------------------------------------------------------------------------
    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Save Total Model TF without ID"), {
      testthat::skip_on_os(os="linux")
      expect_no_error(
        save_ai_model(model=classifier,
                      model_dir = testthat::test_path("test_artefacts/tmp_full_models"),
                      save_format = "tf",
                      append_ID=FALSE)
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Load Total Model TF without ID"), {
      testthat::skip_on_os(os="linux")
      new_classifier<-NULL
      new_classifier<-load_ai_model(
        ml_framework="tensorflow",
        model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models/",classifier$get_model_info()$model_name_root))
      )
      expect_s3_class(new_classifier,
                      class="TextEmbeddingClassifierNeuralNet")
    })
    #------------------------------------------------------------------------------
    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Predict"), {
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
  }
  else if (framework=="pytorch"){
    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Save Total Model pytorch without ID"), {
      expect_no_error(
        save_ai_model(model=classifier,
                      model_dir = testthat::test_path("test_artefacts/tmp_full_models_pytorch"),
                      save_format = "default",
                      append_ID = FALSE)
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Load Total Model pytorch without ID"), {
      new_classifier<-NULL
      new_classifier<-load_ai_model(
        ml_framework="pytorch",
        model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models_pytorch/",classifier$get_model_info()$model_name_root))
      )
      expect_s3_class(new_classifier,
                      class="TextEmbeddingClassifierNeuralNet")
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Save Total Model pytorch  with ID"), {
      expect_no_error(
        save_ai_model(model=classifier,
                      model_dir = testthat::test_path("test_artefacts/tmp_full_models_pytorch"),
                      save_format = "default")
      )
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Load Total Model pytorch  with ID"), {
      new_classifier<-NULL
      new_classifier<-load_ai_model(
        ml_framework="pytorch",
        model_dir = testthat::test_path(paste0("test_artefacts/tmp_full_models_pytorch/",classifier$get_model_info()$model_name))
      )
      expect_s3_class(new_classifier,
                      class="TextEmbeddingClassifierNeuralNet")
    })

    base::gc(verbose = FALSE,full = TRUE)
    test_that(paste(framework,"Classifier Predict"), {
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

  }
}


