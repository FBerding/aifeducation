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
ml_frameworks=c("pytorch")

method_list=list(
  "pytorch"=c("lstm","dense","conv")
  )

#Start tests--------------------------------------------------------------------
for(framework in ml_frameworks){
  for(data_type in names(dataset_list)){
    for(method in method_list[[framework]]){
        #Create----------------------------------------------------------------
      extractor<-TEFeatureExtractor$new()
      extractor$configure(
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
        test_that(paste(framework,method, data_type,"train without log"),{
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

            ml_trace=0
          )
        )
        })
        gc()

        test_that(paste(framework,method, data_type,"train with log"),{
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
              log_dir=train_path,
              trace=FALSE,

              ml_trace=0
            )
          )

          state_log_exists=file.exists(paste0(train_path,"/aifeducation_state.log"))
          expect_true(state_log_exists)
          if(state_log_exists){
            log_state=read.csv(paste0(train_path,"/aifeducation_state.log"))
            expect_equal(nrow(log_state),3)
            expect_equal(ncol(log_state),3)
            expect_equal(colnames(log_state),c("value","total","message"))
          }

          loss_log_exists=file.exists(paste0(train_path,"/aifeducation_loss.log"))
          expect_true(loss_log_exists)
          if(loss_log_exists==TRUE){
            log_loss=read.csv(paste0(train_path,"/aifeducation_loss.log"),header = FALSE)
            expect_gte(ncol(log_loss),2)
            expect_gte(nrow(log_loss),2)
          }
        })

        #Predict---------------------------------------------------------------
        test_that(paste(framework,method, data_type,"predict - basic"),{
        if(data_type=="EmbeddedText"){
          predictions=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                    batch_size = 50)
        } else {
          predictions=extractor$extract_features_large(data_embeddings = dataset_list[[data_type]],
                                                          batch_size = 50)
        }
          expect_equal(predictions$get_features(),128)
          expect_equal(dataset_list[[data_type]]$n_rows(),predictions$n_rows())
          expect_true(predictions$is_compressed())
      })

        test_that(paste(framework,method, data_type,"predict - randomness"),{
          if(data_type=="EmbeddedText"){
            predictions=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                   batch_size = 50)
            predictions_2=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                   batch_size = 50)
            expect_equal(predictions,predictions_2)
          } else {
            predictions=extractor$extract_features_large(data_embeddings = dataset_list[[data_type]],
                                                         batch_size = 50)
            predictions_2=extractor$extract_features_large(data_embeddings = dataset_list[[data_type]],
                                                         batch_size = 50)

            i=sample(seq.int(from = 1,to=predictions$n_rows()))
            expect_equal(predictions$select((i-1))["input"],predictions_2$select((i-1))["input"])

          }
        })

        test_that(paste(framework,method, data_type,"predict - order invariance"),{
          embeddings_ET_perm=dataset_list[["EmbeddedText"]]$clone(deep=TRUE)
          perm=sample(x=seq.int(from = 1,to=nrow(embeddings_ET_perm$embeddings)),replace = FALSE)
          embeddings_ET_perm$embeddings=embeddings_ET_perm$embeddings[perm,,,drop=FALSE]

          if(data_type=="EmbeddedText"){
            predictions=extractor$extract_features(data_embeddings = dataset_list[[data_type]],
                                                   batch_size = 50)
            predictions_Perm<-extractor$extract_features(data_embeddings = embeddings_ET_perm,
                                                         batch_size = 50)
            i=sample(seq.int(from = 1,to=predictions$n_rows()),size = 1)

            expect_equal(predictions$embeddings[i,,,drop=FALSE],
                         predictions_Perm$embeddings[which(perm==i),,,drop=FALSE])


          } else {
            predictions=extractor$extract_features_large(data_embeddings = dataset_list[[data_type]],
                                                         batch_size = 50)
            predictions_Perm=extractor$extract_features_large(data_embeddings = embeddings_ET_perm$convert_to_LargeDataSetForTextEmbeddings(),
                                                           batch_size = 50)
            i=sample(seq.int(from = 1,to=predictions$n_rows()),size = 1)
            expect_equal(predictions$select((i-1))["input"],
                         predictions_Perm$select(which(perm==i)-1)["input"])

          }
        })

        test_that(paste(framework,method, "predict - data source invariance"),{
          if(data_type=="EmbeddedText"){
            predictions_ET=extractor$extract_features(data_embeddings = dataset_list[["EmbeddedText"]],
                                                                              batch_size = 50)
            predictions_LD=extractor$extract_features_large(data_embeddings = dataset_list[["LargeDataSetForTextEmbeddings"]],
                                                      batch_size = 50)
            i=sample(seq.int(from = 1,to=predictions_ET$n_rows()),size = 1)
            expect_equal(unname(predictions_ET$embeddings[i,,,drop=FALSE]),
                         predictions_LD$select(i-1)["input"],
                         tolerance = 1e-7)

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
            dir_path=paste0(root_path_results,"/",folder_name)
            extractor$save(dir_path=root_path_results,
                                 folder_name=folder_name)
            extractor$load(dir_path=dir_path)

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
            dir_path=paste0(root_path_results,"/",folder_name)
            save_to_disk(object=extractor,
                         dir_path=root_path_results,
                         folder_name=folder_name)
            extractor=NULL
            extractor<-load_from_disk(dir_path=dir_path)

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















