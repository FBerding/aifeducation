testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

# SetUp Test---------------------------------------------------------------------
root_path_data <- testthat::test_path("test_data/LargeDataSetForTextEmbeddings")
create_dir(testthat::test_path("test_artefacts"), FALSE)
root_path_results <- testthat::test_path("test_artefacts/LargeDataSetForTextEmbeddings")
create_dir(root_path_results, FALSE)

# object is imdb_embeddings
load(testthat::test_path("test_data/classifier/imdb_embeddings.rda"))


# Start test---------------------------------------------------------------------
test_that("LargeDataSetForTextEmbeddings - Create", {
  expect_no_error(LargeDataSetForTextEmbeddings$new())

  data_set <- LargeDataSetForTextEmbeddings$new()

  expect_no_error(data_set$configure(
    model_name = imdb_embeddings$get_model_info()$model_name,
    model_label = imdb_embeddings$get_model_info()$model_label,
    model_date = imdb_embeddings$get_model_info()$model_date,
    model_method = imdb_embeddings$get_model_info()$model_method,
    model_version = imdb_embeddings$get_model_info()$model_version,
    model_language = imdb_embeddings$get_model_info()$model_language,
    param_seq_length = imdb_embeddings$get_model_info()$param_seq_length,
    param_chunks = imdb_embeddings$get_model_info()$param_chunks,
    param_features = imdb_embeddings$get_model_info()$param_features,
    param_overlap = imdb_embeddings$get_model_info()$param_overlap,
    param_emb_layer_min = imdb_embeddings$get_model_info()$param_emb_layer_min,
    param_emb_layer_max = imdb_embeddings$get_model_info()$param_emb_layer_max,
    param_emb_pool_type = imdb_embeddings$get_model_info()$param_emb_pool_type,
    param_aggregation = imdb_embeddings$get_model_info()$param_aggregation
  ))
})

# Test basic parameters--------------------------------------------------------
test_that("LargeDataSetForTextEmbeddings - No FeatureExtractor", {
  new_dataset <- LargeDataSetForTextEmbeddings$new()
  new_dataset$configure(
    model_name = imdb_embeddings$get_model_info()$model_name,
    model_label = imdb_embeddings$get_model_info()$model_label,
    model_date = imdb_embeddings$get_model_info()$model_date,
    model_method = imdb_embeddings$get_model_info()$model_method,
    model_version = imdb_embeddings$get_model_info()$model_version,
    model_language = imdb_embeddings$get_model_info()$model_language,
    param_seq_length = imdb_embeddings$get_model_info()$param_seq_length,
    param_chunks = imdb_embeddings$get_model_info()$param_chunks,
    param_features = imdb_embeddings$get_features(),
    param_overlap = imdb_embeddings$get_model_info()$param_overlap,
    param_emb_layer_min = imdb_embeddings$get_model_info()$param_emb_layer_min,
    param_emb_layer_max = imdb_embeddings$get_model_info()$param_emb_layer_max,
    param_emb_pool_type = imdb_embeddings$get_model_info()$param_emb_pool_type,
    param_aggregation = imdb_embeddings$get_model_info()$param_aggregation
  )

  # Correct Features
  expect_equal(new_dataset$get_features(), imdb_embeddings$get_features())

  # Correct original features
  expect_equal(new_dataset$get_original_features(), imdb_embeddings$get_features())

  # Correct Times
  expect_equal(new_dataset$get_times(), imdb_embeddings$get_times())

  # Check model information
  for (entry in names(new_dataset$get_model_info())) {
    expect_equal(
      new_dataset$get_model_info()[entry],
      imdb_embeddings$get_model_info()[entry]
    )
  }

  # Add embeddings from array
  expect_no_error(
    new_dataset$add_embeddings_from_array(imdb_embeddings$embeddings)
  )
  expect_equal(new_dataset$n_rows(), nrow(imdb_embeddings$embeddings))

  # add from EbbeddedText
  expect_no_error(
    new_dataset$add_embeddings_from_EmbeddedText(imdb_embeddings)
  )
  expect_equal(new_dataset$n_rows(), 2 * nrow(imdb_embeddings$embeddings))

  # Compression test
  expect_false(new_dataset$is_compressed())

  # Conversation
  new_dataset$reduce_to_unique_ids()
  new_data_set_converted <- new_dataset$convert_to_EmbeddedText()
  expect_equal(nrow(new_data_set_converted$embeddings), nrow(imdb_embeddings$embeddings))
  for (entry in names(new_data_set_converted$get_model_info())) {
    expect_equal(
      new_data_set_converted$get_model_info()[entry],
      new_dataset$get_model_info()[entry]
    )
  }

  # Selection test
  one_case <- new_dataset$select(3)
  expect_equal(one_case$num_rows, 1)
  multiple_cases <- new_dataset$select(c(0, 1, 2, 3))
  expect_equal(multiple_cases$num_rows, 4)
})

#-----------------------------------------------------------------------------
test_that("LargeDataSetForTextEmbeddings - Method Save and Load", {
  save_path <- paste0(root_path_results, "/dataset")
  folder_name <- generate_id()
  load_path <- paste0(save_path, "/", folder_name)

  new_dataset <- LargeDataSetForTextEmbeddings$new()
  new_dataset$configure(
    model_name = imdb_embeddings$get_model_info()$model_name,
    model_label = imdb_embeddings$get_model_info()$model_label,
    model_date = imdb_embeddings$get_model_info()$model_date,
    model_method = imdb_embeddings$get_model_info()$model_method,
    model_version = imdb_embeddings$get_model_info()$model_version,
    model_language = imdb_embeddings$get_model_info()$model_language,
    param_seq_length = imdb_embeddings$get_model_info()$param_seq_length,
    param_chunks = imdb_embeddings$get_model_info()$param_chunks,
    param_features = imdb_embeddings$get_features(),
    param_overlap = imdb_embeddings$get_model_info()$param_overlap,
    param_emb_layer_min = imdb_embeddings$get_model_info()$param_emb_layer_min,
    param_emb_layer_max = imdb_embeddings$get_model_info()$param_emb_layer_max,
    param_emb_pool_type = imdb_embeddings$get_model_info()$param_emb_pool_type,
    param_aggregation = imdb_embeddings$get_model_info()$param_aggregation
  )

  new_dataset$add_embeddings_from_array(imdb_embeddings$embeddings)
  expect_no_error(new_dataset$save(save_path, folder_name = folder_name))

  new_dataset$load(load_path)

  # Number of cases
  expect_equal(new_dataset$n_rows(), nrow(imdb_embeddings$embeddings))
})
#-----------------------------------------------------------------------------
test_that("LargeDataSetForTextEmbeddings - Function Save and Load", {
  save_path <- paste0(root_path_results, "/dataset")
  folder_name <- generate_id()
  load_path <- paste0(save_path, "/", folder_name)

  new_dataset <- LargeDataSetForTextEmbeddings$new()
  new_dataset$configure(
    model_name = imdb_embeddings$get_model_info()$model_name,
    model_label = imdb_embeddings$get_model_info()$model_label,
    model_date = imdb_embeddings$get_model_info()$model_date,
    model_method = imdb_embeddings$get_model_info()$model_method,
    model_version = imdb_embeddings$get_model_info()$model_version,
    model_language = imdb_embeddings$get_model_info()$model_language,
    param_seq_length = imdb_embeddings$get_model_info()$param_seq_length,
    param_chunks = imdb_embeddings$get_model_info()$param_chunks,
    param_features = imdb_embeddings$get_features(),
    param_overlap = imdb_embeddings$get_model_info()$param_overlap,
    param_emb_layer_min = imdb_embeddings$get_model_info()$param_emb_layer_min,
    param_emb_layer_max = imdb_embeddings$get_model_info()$param_emb_layer_max,
    param_emb_pool_type = imdb_embeddings$get_model_info()$param_emb_pool_type,
    param_aggregation = imdb_embeddings$get_model_info()$param_aggregation
  )

  new_dataset$add_embeddings_from_array(imdb_embeddings$embeddings)

  # Save
  expect_no_error(save_to_disk(
    object = new_dataset,
    dir_path = save_path,
    folder_name = folder_name
  ))
  # Load
  new_dataset <- NULL
  new_dataset <- load_from_disk(dir_path = load_path)

  # Number of cases
  expect_equal(new_dataset$n_rows(), nrow(imdb_embeddings$embeddings))

  # Correct Features
  expect_equal(new_dataset$get_features(), imdb_embeddings$get_features())

  # Correct original features
  expect_equal(new_dataset$get_original_features(), imdb_embeddings$get_features())

  # Correct Times
  expect_equal(new_dataset$get_times(), imdb_embeddings$get_times())

  # Check model information
  for (entry in names(new_dataset$get_model_info())) {
    expect_equal(
      new_dataset$get_model_info()[entry],
      imdb_embeddings$get_model_info()[entry]
    )
  }
})
