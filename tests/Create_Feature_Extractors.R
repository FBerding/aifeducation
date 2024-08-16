#This file does not contain any tests. It is used for creating FeatureExtractors
#that can be used for testing Classifiers

#Config-------------------------------------------------------------------------
root_path_data=testthat::test_path("test_data/classifier")

if(dir.exists(root_path_data)==FALSE){
  dir.create(root_path_data)
}

ml_frameworks<-c("pytorch")

method_list="lstm"

load(testthat::test_path("test_data/classifier/imdb_embeddings.rda"))

dataset_list=list("EmbeddedText"=imdb_embeddings,
                  "LargeDataSetForTextEmbeddings"=imdb_embeddings$convert_to_LargeDataSetForTextEmbeddings())

trace=TRUE

#Start creation and training---------------------------------------------------
for(framework in ml_frameworks){
  for(method in method_list){

    train_path=paste0(root_path_data)
          extractor<-TEFeatureExtractor$new()
          extractor$configure(
            ml_framework = framework,
            name="Test_extractor",
            label="Test Extractor",
            text_embeddings=dataset_list[["LargeDataSetForTextEmbeddings"]],
            features=128,
            method=method,
            noise_factor=0.02,
            optimizer="adam"
          )



              extractor$train(
                data_embeddings=dataset_list[["LargeDataSetForTextEmbeddings"]],
                data_val_size=0.25,
                sustain_track=TRUE,
                sustain_iso_code="DEU",
                sustain_region=NULL,
                sustain_interval=15,
                epochs=500,
                batch_size=100,
                dir_checkpoint=train_path,
                trace=trace,
                keras_trace=1,
                pytorch_trace=1
              )
              save_to_disk(object = extractor,
                           dir_path =  root_path_data,
                           folder_name = paste0("feature_extractor_",framework))


  }
}


