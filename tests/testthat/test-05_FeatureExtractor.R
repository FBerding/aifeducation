testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                      message  = "Necessary python modules not available")

#SetUp-------------------------------------------------------------------------
#Set paths
root_path_data=testthat::test_path("test_data/FeatureExtractor")
if(dir.exists(testthat::test_path("test_artefacts"))==FALSE){
  dir.create(testthat::test_path("test_artefacts"))
}
root_path_results=testthat::test_path("test_artefacts/FeatureExtractor")
if(dir.exists(root_path_results)==FALSE){
  dir.create(root_path_results)
}

#SetUp tensorflow
aifeducation::set_config_gpu_low_memory()
set_config_tf_logger("ERROR")
set_config_os_environ_logger("ERROR")

#SetUp datasets
#Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

#load data for test
#object is imdb_embeddings
load(testthat::test_path("test_data/classifier/imdb_embeddings.rda"))

dataset_list=list("EmbeddedText"=imdb_embeddings,
             "LargeDataSetForTextEmbeddings"=imdb_embeddings$convert_to_LargeDataSetForTextEmbeddings())

#config
ml_frameworks=c("tensorflow","pytorch")

method_list=list(
  "tensorflow"=c("lstm"),
  "pytorch"=c("lstm","dense","conv"))

#Start tests--------------------------------------------------------------------
for(framework in ml_frameworks){
  for(data_type in names(dataset_list)){
    for(method in method_list[[framework]]){
        #Create----------------------------------------------------------------
        extractor<-TEFeatureExtractor$new(
          ml_framework = framework,
          name="Test_extractor",
          label="Test Extractor",
          text_embeddings=dataset_list[[data_type]],
          features=128,
          method=method,
          noise_factor=0.2,
          optimizer="adam"
        )

        #Train-----------------------------------------------------------------
        test_that(paste(framework,method, data_type,"train"),{
        train_path=paste0(root_path_results,"/","train_",generate_id())
        if(dir.exists(train_path)==FALSE){
          dir.create(train_path)
        }
        expect_no_error(
          extractor$train(
            data_embeddings=dataset_list[[data_type]],
            data_val_size=0.25,
            sustain_track=TRUE,
            sustain_iso_code="DEU",
            sustain_region=NULL,
            sustain_interval=15,
            epochs=2,
            batch_size=100,
            dir_checkpoint=train_path,
            trace=FALSE,
            keras_trace=0,
            pytorch_trace=0
          )
        )
        })
        gc()

        #Predict---------------------------------------------------------------
        test_that(paste(framework,method, data_type,"predict"),{
        if(data_type=="EmbeddedText"){
          predictions_ET=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                    batch_size = 50)
          expect_equal(predictions_ET$get_features(),128)
          expect_equal(dataset_list[[data_type]]$n_rows(),predictions_ET$n_rows())
          expect_true(predictions_ET$is_compressed())

          predictions_large=extractor$extract_features_large(
            data_embeddings = dataset_list[[data_type]]$convert_to_LargeDataSetForTextEmbeddings(),
            batch_size = 50,
            trace = FALSE)
          expect_equal(predictions_large$get_features(),128)
          expect_equal(dataset_list[[data_type]]$convert_to_LargeDataSetForTextEmbeddings()$n_rows(),predictions_ET$n_rows())
          expect_true(predictions_large$is_compressed())

          #R index is one based, python index is zero based
          i=sample(x=seq.int(from=1,to=predictions_ET$n_rows()),size = 1)
          expect_equal(unname(predictions_ET$embeddings[i,,,drop=FALSE]),
                       predictions_large$select((i-1))["input"],
                       tolerance = 1e-6)

        } else if(data_type=="LargeDataSetForTextEmbeddings"){
          predictions_ET=extractor$extract_features(data_embeddings = dataset_list[[data_type]]$convert_to_EmbeddedText(),
                                                    batch_size = 50)
          expect_equal(predictions_ET$get_features(),128)
          expect_equal(dataset_list[[data_type]]$n_rows(),predictions_ET$n_rows())
          expect_true(predictions_ET$is_compressed())

          predictions_large=extractor$extract_features_large(
            data_embeddings = dataset_list[[data_type]],
            batch_size = 50,
            trace = FALSE)
          expect_equal(predictions_large$get_features(),128)
          expect_equal(dataset_list[[data_type]]$convert_to_EmbeddedText()$n_rows(),predictions_ET$n_rows())
          expect_true(predictions_large$is_compressed())

          #R index is one based, python index is zero based
          i=sample(x=seq.int(from=1,to=predictions_ET$n_rows()),size = 1)
          expect_equal(unname(predictions_ET$embeddings[i,,,drop=FALSE]),
                       predictions_large$select((i-1))["input"],
                       tolerance = 1e-6)
        }



      })
        gc()

        #Method for loading and saving models-----------------------------------
          test_that(paste(framework,method, data_type,"method save and load"),{

            #Predictions before saving and loading
            if(data_type=="EmbeddedText"){
              predictions=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                     batch_size = 50)
            } else {
              predictions=extractor$extract_features_large(
                data_embeddings = dataset_list[[data_type]],
                batch_size = 50,
                trace = FALSE)
            }

            #Save and load
            folder_name=paste0("method_save_load_",generate_id())
            model_dir=paste0(root_path_results,"/",folder_name)
            extractor$save(model_dir=root_path_results,
                                 folder_name=folder_name)
            extractor$load(model_dir=model_dir)

            #Predict after loading
            if(data_type=="EmbeddedText"){
              predictions_2=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                       batch_size = 50)
            } else {
              predictions_2=extractor$extract_features_large(
                data_embeddings = dataset_list[[data_type]],
                batch_size = 50,
                trace = FALSE)
            }

            #Compare predictions
            i=sample(x=seq.int(from=1,to=predictions$n_rows()),size = 1)
            expect_equal(predictions$embeddings[i,,,drop=FALSE],
                         predictions_2$embeddings[i,,,drop=FALSE],
                         tolerance = 1e-6)

          })
          gc()


        #Function for loading and saving models-----------------------------------

          test_that(paste(framework,method, data_type,"function save and load"),{

            #Predictions before saving and loading
            if(data_type=="EmbeddedText"){
              predictions=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                     batch_size = 50)
            } else {
              predictions=extractor$extract_features_large(
                data_embeddings = dataset_list[[data_type]],
                batch_size = 50,
                trace = FALSE)
            }

            #Save and load
            folder_name=paste0("function_save_load_",generate_id())
            model_dir=paste0(root_path_results,"/",folder_name)
            save_to_disk(object=extractor,
                         model_dir=root_path_results,
                         folder_name=folder_name)
            extractor=NULL
            extractor<-load_from_disk(model_dir=model_dir)

            #Predict after loading
            if(data_type=="EmbeddedText"){
              predictions_2=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                       batch_size = 50)
            } else {
              predictions_2=extractor$extract_features_large(
                data_embeddings = dataset_list[[data_type]],
                batch_size = 50,
                trace = FALSE)
            }

            #Compare predictions
            i=sample(x=seq.int(from=1,to=predictions$n_rows()),size = 1)
            expect_equal(predictions$embeddings[i,,,drop=FALSE],
                         predictions_2$embeddings[i,,,drop=FALSE],
                         tolerance = 1e-6)

          })
          gc()

    }
  }
}















