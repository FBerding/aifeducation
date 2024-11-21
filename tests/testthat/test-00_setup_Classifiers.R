# This file does not contain any tests. It is used for creating FeatureExtractors
# that can be used for testing Classifiers

testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE, check = "pytorch"),
  message = "Necessary python modules not available"
)

test_that("Setup Classifier Data", {
  # Config-------------------------------------------------------------------------
  root_path_data <- testthat::test_path("test_data/Embeddings")
  create_dir(root_path_data, FALSE)

  root_path_results<- testthat::test_path("test_data_tmp")
  create_dir(root_path_results, FALSE)
  root_path_results<- testthat::test_path("test_data_tmp/classifier")
  create_dir(root_path_results, FALSE)

  ml_frameworks <- c("pytorch")
  trace <- FALSE

  method_list <- "lstm"

  imdb_embeddings<-load_from_disk(paste0(root_path_data,"/imdb_embeddings"))

  dataset_list <- list(
    "EmbeddedText" = imdb_embeddings,
    "LargeDataSetForTextEmbeddings" = imdb_embeddings$convert_to_LargeDataSetForTextEmbeddings()
  )



  # Start creation and training---------------------------------------------------
  for (framework in ml_frameworks) {
    for (method in method_list) {
      train_path <- paste0(root_path_data)
      extractor <- TEFeatureExtractor$new()
      extractor$configure(
        ml_framework = framework,
        name = "Test_extractor",
        label = "Test Extractor",
        text_embeddings = dataset_list[["LargeDataSetForTextEmbeddings"]],
        features = 128,
        method = method,
        noise_factor = 0.002,
        optimizer = "adam"
      )
      extractor$train(
        data_embeddings = dataset_list[["LargeDataSetForTextEmbeddings"]],
        data_val_size = 0.25,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        epochs = 75,
        batch_size = 100,
        dir_checkpoint = train_path,
        trace = trace,
        ml_trace = as.numeric(trace)
      )
      save_to_disk(
        object = extractor,
        dir_path = root_path_results,
        folder_name = paste0("feature_extractor_", framework)
      )
    }
  }

  expect_true(
    file.exists(paste0(root_path_results,"/","feature_extractor_",framework,"/","model_data.safetensors"))
  )
})
