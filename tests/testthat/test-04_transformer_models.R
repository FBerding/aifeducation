testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                  message = "Necessary python modules not available")

if(aifeducation_config$global_framework_set()==FALSE){
  aifeducation_config$set_global_ml_backend("tensorflow")
}

aifeducation::set_config_gpu_low_memory()
#transformers$utils$logging$set_verbosity_warning()
transformers$utils$logging$set_verbosity_error()
os$environ$setdefault("TOKENIZERS_PARALLELISM","false")
set_config_tf_logger("ERROR")
set_config_os_environ_logger("ERROR")

if(dir.exists(testthat::test_path("test_artefacts"))==FALSE){
  dir.create(testthat::test_path("test_artefacts"))
}

if(dir.exists(testthat::test_path("test_artefacts/tmp"))==FALSE){
  dir.create(testthat::test_path("test_artefacts/tmp"))
}

ml_frameworks<-c("tensorflow",
                 "pytorch")

ai_methods=c("bert",
             "roberta",
             "longformer",
             "funnel",
             "deberta_v2"
             )
ai_framework_matrix<-matrix(
  ncol=2,
  nrow=length(ai_methods),
  data=c(1,1,
         1,1,
         1,1,
         1,1,
         1,1),
  byrow=TRUE)
colnames(ai_framework_matrix)<-c("tensorflow","pytorch")
rownames(ai_framework_matrix)<-ai_methods

rows_susatainability<-vector(length = length(ai_methods))

names(rows_susatainability)<-ai_methods
rows_susatainability["bert"]=3
rows_susatainability["funnel"]=2
rows_susatainability["roberta"]=2
rows_susatainability["longformer"]=2
rows_susatainability["deberta_v2"]=2


example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

print(check_aif_py_modules())

for(framework in ml_frameworks){

for(ai_method in ai_methods){
  base::gc(verbose = FALSE,full = TRUE)
  path_01=paste0("test_artefacts/",ai_method)
  if(dir.exists(testthat::test_path(path_01))==FALSE){
    dir.create(testthat::test_path(path_01))
  }

  path_02=paste0("test_artefacts/tmp/",ai_method)
  path_03=paste0("test_artefacts/tmp_full_models")

  if(dir.exists(testthat::test_path(path_02))==FALSE){
    dir.create(testthat::test_path(path_02))
  }

  if(dir.exists(testthat::test_path(path_03))==FALSE){
    dir.create(testthat::test_path(path_03))
  }
  if(dir.exists(testthat::test_path(paste0(path_03,"/tensorflow")))==FALSE){
    dir.create(testthat::test_path(paste0(path_03,"/tensorflow")))
  }
  if(dir.exists(testthat::test_path(paste0(path_03,"/pytorch")))==FALSE){
    dir.create(testthat::test_path(paste0(path_03,"/pytorch")))
  }

    if(ai_framework_matrix[ai_method,framework]==1){

      #Creation of the Model--------------------------------------------------------

      test_that(paste0(ai_method,"create_model","_",framework), {
        #BERT---------------------------------------------------------------------
        if(ai_method=="bert"){
        expect_no_error(
          create_bert_model(
          ml_framework = framework,
          model_dir=testthat::test_path(paste0(path_01,"/",framework)),
          vocab_raw_texts=example_data$text,
          vocab_size=50000,
          vocab_do_lower_case=FALSE,
          max_position_embeddings=512,
          hidden_size=256,
          num_hidden_layer=2,
          num_attention_heads=2,
          intermediate_size=256,
          hidden_act="gelu",
          hidden_dropout_prob=0.1,
          sustain_track=TRUE,
          sustain_iso_code = "DEU",
          sustain_region = NULL,
          sustain_interval = 15,
          trace=FALSE))

        expect_no_error(
          create_bert_model(
            ml_framework = framework,
            model_dir=testthat::test_path(paste0(path_01,"/",framework)),
            vocab_raw_texts=example_data$text,
            vocab_size=50000,
            vocab_do_lower_case=TRUE,
            max_position_embeddings=512,
            hidden_size=256,
            num_hidden_layer=2,
            num_attention_heads=2,
            intermediate_size=256,
            hidden_act="gelu",
            hidden_dropout_prob=0.1,
            sustain_track=TRUE,
            sustain_iso_code = "DEU",
            sustain_region = NULL,
            sustain_interval = 15,
            trace=FALSE))

      } else if (ai_method=="roberta"){
          expect_no_error(
            create_roberta_model(
              ml_framework = framework,
              model_dir=testthat::test_path(paste0(path_01,"/",framework)),
              vocab_raw_texts=example_data$text,
              vocab_size=10000,
              add_prefix_space=FALSE,
              max_position_embeddings=512,
              hidden_size=16,
              num_hidden_layer=2,
              num_attention_heads=2,
              intermediate_size=128,
              hidden_act="gelu",
              hidden_dropout_prob=0.1,
              sustain_track=TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace=FALSE))

          expect_no_error(
            create_roberta_model(
              ml_framework = framework,
              model_dir=testthat::test_path(paste0(path_01,"/",framework)),
              vocab_raw_texts=example_data$text,
              vocab_size=10000,
              add_prefix_space=TRUE,
              max_position_embeddings=512,
              hidden_size=32,
              num_hidden_layer=2,
              num_attention_heads=2,
              intermediate_size=128,
              hidden_act="gelu",
              hidden_dropout_prob=0.1,
              sustain_track=FALSE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace=FALSE))

      } else if(ai_method=="longformer"){
          expect_no_error(
            create_longformer_model(
              ml_framework = framework,
              model_dir=testthat::test_path(paste0(path_01,"/",framework)),
              vocab_raw_texts=example_data$text[1:500],
              vocab_size=10000,
              add_prefix_space=FALSE,
              max_position_embeddings=512,
              hidden_size=32,
              num_hidden_layer=2,
              num_attention_heads=2,
              intermediate_size=128,
              hidden_act="gelu",
              hidden_dropout_prob=0.1,
              attention_window = 40,
              sustain_track=TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace=FALSE))

          expect_no_error(
            create_longformer_model(
              ml_framework = framework,
              model_dir=testthat::test_path(paste0(path_01,"/",framework)),
              vocab_raw_texts=example_data$text[1:500],
              vocab_size=10000,
              add_prefix_space=TRUE,
              max_position_embeddings=512,
              hidden_size=32,
              num_hidden_layer=2,
              num_attention_heads=2,
              intermediate_size=128,
              hidden_act="gelu",
              hidden_dropout_prob=0.1,
              attention_window = 40,
              sustain_track=FALSE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace=FALSE))
      }  else if (ai_method=="funnel"){
        expect_no_error(
          create_funnel_model(
            ml_framework = framework,
            model_dir=testthat::test_path(paste0(path_01,"/",framework)),
            vocab_raw_texts=example_data$text,
            vocab_size=10000,
            max_position_embeddings=512,
            hidden_size=32,
            block_sizes = c(2,2,2),
            num_decoder_layers = 1,
            num_attention_heads=2,
            intermediate_size=128,
            hidden_act="gelu",
            hidden_dropout_prob=0.1,
            sustain_track=TRUE,
            sustain_iso_code = "DEU",
            sustain_region = NULL,
            sustain_interval = 15,
            trace=FALSE))

        expect_no_error(
          create_funnel_model(
            ml_framework = framework,
            model_dir=testthat::test_path(paste0(path_01,"/",framework)),
            vocab_raw_texts=example_data$text,
            vocab_size=10000,
            max_position_embeddings=512,
            hidden_size=32,
            block_sizes = c(2,2,2),
            num_attention_heads=2,
            intermediate_size=128,
            hidden_act="gelu",
            hidden_dropout_prob=0.1,
            sustain_track=FALSE,
            sustain_iso_code = "DEU",
            sustain_region = NULL,
            sustain_interval = 15,
            trace=FALSE))

      } else if (ai_method=="deberta_v2"){
        expect_no_error(
          create_deberta_v2_model(
            ml_framework = framework,
            model_dir=testthat::test_path(paste0(path_01,"/",framework)),
            vocab_raw_texts=example_data$text,
            vocab_size=10000,
            add_prefix_space=FALSE,
            max_position_embeddings=512,
            hidden_size=32,
            num_hidden_layer=2,
            num_attention_heads=2,
            intermediate_size=128,
            hidden_act="gelu",
            hidden_dropout_prob=0.1,
            sustain_track=TRUE,
            sustain_iso_code = "DEU",
            sustain_region = NULL,
            sustain_interval = 15,
            trace=FALSE))

        expect_no_error(
          create_deberta_v2_model(
            ml_framework = framework,
            model_dir=testthat::test_path(paste0(path_01,"/",framework)),
            vocab_raw_texts=example_data$text,
            vocab_size=10000,
            add_prefix_space=TRUE,
            max_position_embeddings=512,
            hidden_size=32,
            num_hidden_layer=2,
            num_attention_heads=2,
            intermediate_size=128,
            hidden_act="gelu",
            hidden_dropout_prob=0.1,
            sustain_track=FALSE,
            sustain_iso_code = "DEU",
            sustain_region = NULL,
            sustain_interval = 15,
            trace=FALSE))
      } else if(ai_method=="rwkv"){
        expect_no_error(
        create_rwkv_model(
          ml_framework=framework,
          model_dir=testthat::test_path(paste0(path_01,"/",framework)),
          vocab_raw_texts=example_data$text,
          vocab_size=50277,
          context_length=256,
          add_prefix_space=FALSE,
          trim_offsets=TRUE,
          do_lower_case=FALSE,
          max_position_embeddings=512,
          hidden_size=256,
          num_hidden_layer=2,
          attention_hidden_size=256,
          intermediate_size=256,
          hidden_act="gelu_new",
          hidden_dropout_prob=0.1,
          sustain_track=TRUE,
          sustain_iso_code = "DEU",
          sustain_region = NULL,
          sustain_interval = 15,
          trace=TRUE)
        )

        expect_no_error(
        create_rwkv_model(
    ml_framework=framework,
    model_dir=testthat::test_path(paste0(path_01,"/",framework)),
    vocab_raw_texts=example_data$text,
    vocab_size=50277,
    context_length=256,
    add_prefix_space=FALSE,
    trim_offsets=TRUE,
    do_lower_case=FALSE,
    max_position_embeddings=512,
    hidden_size=256,
    num_hidden_layer=2,
    attention_hidden_size=256,
    intermediate_size=4*256,
    hidden_act="gelu_new",
    hidden_dropout_prob=0.1,
    sustain_track=FALSE,
    sustain_iso_code = "DEU",
    sustain_region = NULL,
    sustain_interval = 15,
    trace=TRUE)
        )
      }
        })

      #Training of the Model------------------------------------------------------
      test_that(paste0(ai_method,"train_tune","_",framework), {
        if(ai_method=="bert"){
        expect_no_error(
          train_tune_bert_model(ml_framework = framework,
                                output_dir=testthat::test_path(paste0(path_01,"/",framework)),
                                model_dir_path=testthat::test_path(paste0(path_01,"/",framework)),
                                raw_texts= example_data$text[1:10],
                                p_mask=0.15,
                                whole_word=TRUE,
                                full_sequences_only = TRUE,
                                val_size=0.25,
                                n_epoch=2,
                                batch_size=2,
                                chunk_size=100,
                                n_workers=1,
                                multi_process=FALSE,
                                sustain_track=TRUE,
                                sustain_iso_code = "DEU",
                                sustain_region = NULL,
                                sustain_interval = 15,
                                trace=FALSE,
                                keras_trace = 0))

        expect_no_error(
          train_tune_bert_model(ml_framework = framework,
                                output_dir=testthat::test_path(paste0(path_01,"/",framework)),
                                model_dir_path=testthat::test_path(paste0(path_01,"/",framework)),
                                raw_texts= example_data$text[1:10],
                                p_mask=0.30,
                                whole_word=FALSE,
                                full_sequences_only = TRUE,
                                val_size=0.1,
                                n_epoch=2,
                                batch_size=1,
                                chunk_size=100,
                                n_workers=1,
                                multi_process=FALSE,
                                sustain_track=TRUE,
                                sustain_iso_code = "DEU",
                                sustain_region = NULL,
                                sustain_interval = 15,
                                trace=FALSE,
                                keras_trace = 0))

        } else if(ai_method=="roberta"){
          expect_no_error(
            train_tune_roberta_model(
              ml_framework = framework,
              output_dir=testthat::test_path(paste0(path_01,"/",framework)),
              model_dir_path=testthat::test_path(paste0(path_01,"/",framework)),
              raw_texts= example_data$text[1:5],
              p_mask=0.30,
              val_size=0.1,
              n_epoch=2,
              batch_size=1,
              chunk_size=70,
              full_sequences_only = TRUE,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              keras_trace = 0,
              trace=FALSE))

        } else if(ai_method=="longformer"){
          expect_no_error(
            train_tune_longformer_model(
              ml_framework = framework,
              output_dir=testthat::test_path(paste0(path_01,"/",framework)),
              model_dir_path=testthat::test_path(paste0(path_01,"/",framework)),
              raw_texts= example_data$text[1:5],
              p_mask=0.30,
              val_size=0.1,
              n_epoch=2,
              batch_size=1,
              chunk_size=512,
              full_sequences_only = FALSE,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              keras_trace = 0,
              trace=FALSE))
        } else if(ai_method=="funnel"){
          expect_no_error(
            train_tune_funnel_model(ml_framework = framework,
                                  output_dir=testthat::test_path(paste0(path_01,"/",framework)),
                                  model_dir_path=testthat::test_path(paste0(path_01,"/",framework)),
                                  raw_texts= example_data$text[1:20],
                                  p_mask=0.15,
                                  val_size=0.1,
                                  n_epoch=2,
                                  batch_size=2,
                                  min_seq_len = 50,
                                  full_sequences_only = TRUE,
                                  chunk_size=250,
                                  n_workers=1,
                                  multi_process=FALSE,
                                  sustain_track=TRUE,
                                  sustain_iso_code = "DEU",
                                  sustain_region = NULL,
                                  sustain_interval = 15,
                                  trace=FALSE,
                                  keras_trace = 0))

        }  else if(ai_method=="deberta_v2"){
          expect_no_error(
            train_tune_deberta_v2_model(
              ml_framework = framework,
              output_dir=testthat::test_path(paste0(path_01,"/",framework)),
              model_dir_path=testthat::test_path(paste0(path_01,"/",framework)),
              raw_texts= example_data$text[1:5],
              p_mask=0.15,
              val_size=0.1,
              n_epoch=2,
              batch_size=2,
              chunk_size=100,
              full_sequences_only = FALSE,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              keras_trace = 0,
              trace=FALSE))
        }
      })

      #Embedding of the Model-------------------------------------------------------

      bert_modeling<-TextEmbeddingModel$new(
        model_name=paste0(ai_method,"_embedding"),
        model_label=paste0("Text Embedding via",ai_method),
        model_version="0.0.1",
        model_language="english",
        method = ai_method,
        ml_framework=framework,
        max_length = 256,
        chunks=4,
        overlap=10,
        aggregation="last",
        model_dir=testthat::test_path(paste0(path_01,"/",framework))
      )

      model_name=bert_modeling$get_model_info()$model_name
      model_name_root=bert_modeling$get_model_info()$model_name_root

      test_that(paste0(ai_method,"creation",framework), {
        expect_s3_class(bert_modeling,
                        class="TextEmbeddingModel")
      })

      test_that(paste0(ai_method,"Saving Model",framework), {
        expect_no_error(
          bert_modeling$save_model(testthat::test_path(paste0(path_02,"/",framework)))
        )
      })

      test_that(paste0(ai_method,"Loading Model",framework), {
        expect_no_error(
          bert_modeling$load_model(
            model_dir=testthat::test_path(paste0(path_02,"/",framework)),
            ml_framework=framework)
        )
      })

      test_that(paste0(ai_method,"embedding",framework), {
        embeddings<-bert_modeling$embed(raw_text = example_data$text[1:10],
                                        doc_id = example_data$id[1:10])
        expect_s3_class(embeddings, class="EmbeddedText")

        embeddings<-NULL
        embeddings<-bert_modeling$embed(raw_text = example_data$text[1:1],
                                        doc_id = example_data$id[1:1])
        expect_s3_class(embeddings, class="EmbeddedText")

      })

      test_that(paste0(ai_method,"encoding",framework), {
        encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                        token_encodings_only = TRUE,
                                        to_int=TRUE)
        expect_length(encodings,10)
        expect_type(encodings,type="list")

        encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                        token_encodings_only = TRUE,
                                        to_int=FALSE)
        expect_length(encodings,10)
        expect_type(encodings,type="list")
      })

      test_that(paste0(ai_method,"decoding_bert",framework), {
        encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                        token_encodings_only = TRUE,
                                        to_int=TRUE)
        decodings<-bert_modeling$decode(encodings,
                                        to_token = FALSE)
        expect_length(decodings,10)
        expect_type(decodings,type="list")

        decodings<-bert_modeling$decode(encodings,
                                        to_token = TRUE)
        expect_length(decodings,10)
        expect_type(decodings,type="list")
      })

      test_that(paste0(ai_method,"get_special_tokens",framework), {
        tokens<-bert_modeling$get_special_tokens()
        expect_equal(nrow(tokens),7)
        expect_equal(ncol(tokens),3)
      })

      test_that(paste0(ai_method,"fill_mask",framework), {
        tokens<-bert_modeling$get_special_tokens()
        mask_token<-tokens[which(tokens[,1]=="mask_token"),2]

        first_solution<-bert_modeling$fill_mask(text=paste("This is a",mask_token,"."),
                                n_solutions = 5)
        expect_equal(length(first_solution),1)
        expect_true(is.data.frame(first_solution[[1]]))
        expect_equal(nrow(first_solution[[1]]),5)
        expect_equal(ncol(first_solution[[1]]),3)

        second_solution<-bert_modeling$fill_mask(text=paste("This is a",mask_token,"."),
                                                n_solutions = 1)
        expect_equal(length(second_solution),1)
        expect_true(is.data.frame(second_solution[[1]]))
        expect_equal(nrow(second_solution[[1]]),1)
        expect_equal(ncol(second_solution[[1]]),3)

        third_solution<-bert_modeling$fill_mask(text=paste("This is a",mask_token,".",
                                                           "The weather is",mask_token,"."),
                                                 n_solutions = 5)
        expect_equal(length(third_solution),2)
        for(i in 1:2){

          expect_true(is.data.frame(third_solution[[i]]))
          expect_equal(nrow(third_solution[[i]]),5)
          expect_equal(ncol(third_solution[[i]]),3)
        }
      })

      test_that(paste0(ai_method,"descriptions",framework), {
        bert_modeling$set_model_description(
          eng = "Description",
          native = "Beschreibung",
          abstract_eng = "Abstract",
          abstract_native = "Zusammenfassung",
          keywords_eng = c("Test","Neural Net"),
          keywords_native = c("Test","Neuronales Netz")
        )
        desc<-bert_modeling$get_model_description()
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

      test_that(paste0(ai_method,"software_license",framework), {
        bert_modeling$set_software_license("test_license")
        expect_equal(
          object=bert_modeling$get_software_license(),
          expected=c("test_license")
        )
      })

      test_that(paste0(ai_method,"documentation_license",framework), {
        bert_modeling$set_documentation_license("test_license")
        expect_equal(
          object=bert_modeling$get_documentation_license(),
          expected=c("test_license")
        )
      })

      test_that(paste0(ai_method,"publication_info",framework),{
        bert_modeling$set_publication_info(
          type="developer",
          authors = personList(
            person(given="Max",family="Mustermann")
          ),
          citation="Test Classifier",
          url="https://Test.html"
        )

        bert_modeling$set_publication_info(
          type="modifier",
          authors = personList(
            person(given="Nico",family="Meyer")
          ),
          citation="Test Classifier Revisited",
          url="https://Test_revisited.html"
        )


        pub_info=bert_modeling$get_publication_info()

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

        expect_equal(
          object=pub_info$modified_by$authors,
          expected=personList(
            person(given="Nico",family="Meyer")
          )
        )
        expect_equal(
          object=pub_info$modified_by$citation,
          expected="Test Classifier Revisited"
        )
        expect_equal(
          object=pub_info$modified_by$url,
          expected="https://Test_revisited.html"
        )
      })

      test_that(paste0(ai_method,"get_methods",framework), {
        expect_no_error(bert_modeling$get_transformer_components())
        expect_no_error(bert_modeling$get_basic_components())
        expect_no_error(bert_modeling$get_bow_components())
        })

      #------------------------------------------------------------------------
      if(framework=="tensorflow"){
        test_that(paste0(ai_method,"Save Total Model h5",framework), {
          expect_no_error(
            save_ai_model(model=bert_modeling,
                          model_dir = testthat::test_path(paste0(path_03,"/",framework)),
                          save_format = "h5")
          )
        })

        test_that(paste0(ai_method,"Load Total Model h5",framework), {
          bert_modeling<-NULL
          bert_modeling<-load_ai_model(
            ml_framework = framework,
            model_dir = testthat::test_path(paste0(path_03,"/",framework,"/",model_name))
          )
          expect_s3_class(bert_modeling,
                          class="TextEmbeddingModel")
        })
      } else {
        test_that(paste0(ai_method,"Save Total Model safetensors",framework), {
          expect_no_error(
            save_ai_model(model=bert_modeling,
                          model_dir = testthat::test_path(paste0(path_03,"/",framework)),
                          save_format = "safetensors")
          )
          if(reticulate::py_module_available("safetensors")){
            expect_true(file.exists(testthat::test_path(paste0(path_03,"/",framework,"/",model_name,"/model_data/model.safetensors"))))
          } else {
            expect_true(file.exists(testthat::test_path(paste0(path_03,"/",framework,"/",model_name,"/model_data/pytorch_model.bin"))))
          }
        })

        test_that(paste0(ai_method,"Load Total Model safetensors",framework), {
          bert_modeling<-NULL
          bert_modeling<-load_ai_model(
            ml_framework = framework,
            model_dir = testthat::test_path(paste0(path_03,"/",framework,"/",model_name))
          )
          expect_s3_class(bert_modeling,
                          class="TextEmbeddingModel")
        })
      }


      #------------------------------------------------------------------------
      test_that(paste0(ai_method,"Save Total Model default with ID",framework), {
        expect_no_error(
          save_ai_model(model=bert_modeling,
                        model_dir = testthat::test_path(paste0(path_03,"/",framework)),
                        save_format = "default")
        )
        if(framework=="pytorch"){
          if(reticulate::py_module_available("safetensors")){
            expect_true(file.exists(testthat::test_path(paste0(path_03,"/",framework,"/",model_name,"/model_data/model.safetensors"))))
          } else {
            expect_true(file.exists(testthat::test_path(paste0(path_03,"/",framework,"/",model_name,"/model_data/pytorch_model.bin"))))
          }
        }

      })

      test_that(paste0(ai_method,"Load Total Model default with ID",framework), {
        bert_modeling<-NULL
        bert_modeling<-load_ai_model(
          ml_framework = framework,
          model_dir = testthat::test_path(paste0(path_03,"/",framework,"/",model_name))
        )
        expect_s3_class(bert_modeling,
                        class="TextEmbeddingModel")
      })

      #-------------------------------------------------------------------------
      test_that(paste0(ai_method,"Save Total Model default without ID",framework), {
        expect_no_error(
          save_ai_model(model=bert_modeling,
                        model_dir = testthat::test_path(paste0(path_03,"/",framework)),
                        save_format = "default",
                        append_ID = FALSE)
        )
      })

      test_that(paste0("Load Total Model default without ID",framework), {
        bert_modeling<-NULL
        bert_modeling<-load_ai_model(
          ml_framework = framework,
          model_dir = testthat::test_path(paste0(path_03,"/",framework,"/",model_name_root))
        )
        expect_s3_class(bert_modeling,
                        class="TextEmbeddingModel")
      })

      #------------------------------------------------------------------------
      test_that(paste0(ai_method,"Sustainability Data Loaded",framework), {
        bert_modeling<-NULL
        bert_modeling<-load_ai_model(
          ml_framework = framework,
          model_dir = testthat::test_path(paste0(path_03,"/",framework,"/",model_name_root))
        )
        sustain_data<-bert_modeling$get_sustainability_data()
        expect_equal(nrow(sustain_data),rows_susatainability[[ai_method]])
      })
    } else {
      cat(paste(framework,"not supported for",ai_method,"\n"))
    }
    #---------------------------------------------------------------------------
  }
}


for(ai_method in ai_methods){
  for(framework in ml_frameworks){
    base::gc(verbose = FALSE,full = TRUE)
    if(framework=="tensorflow"){
      other_framework="pytorch"
    } else {
      other_framework="tensorflow"
    }

    tmp_path=testthat::test_path(
      paste0(
        "test_artefacts/tmp_full_models/",
        other_framework,"/",
        ai_method,"_embedding")
    )

    test_that(paste(ai_method,"load from",other_framework,"to",framework),{
      expect_no_error(
        test<-load_ai_model(
          model_dir = tmp_path,
          ml_framework = framework
        )
      )

      tmp<-test$get_transformer_components()[[4]]

      expect_equal(tmp,framework)

    }
    )
  }
}

