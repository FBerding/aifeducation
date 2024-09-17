testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

# SetUp-------------------------------------------------------------------------
# Set paths
root_path_data <- testthat::test_path("test_data/TextEmbeddingModel")
if (dir.exists(testthat::test_path("test_artefacts")) == FALSE) {
  dir.create(testthat::test_path("test_artefacts"))
}
root_path_results <- testthat::test_path("test_artefacts/TextEmbeddingModel")
if (dir.exists(root_path_results) == FALSE) {
  dir.create(root_path_results)
}

# SetUp tensorflow
aifeducation::set_config_gpu_low_memory()
set_config_tf_logger("ERROR")
set_config_os_environ_logger("ERROR")

# SetUp datasets
# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

# load data for test
# Use internal sample data
example_data <- imdb_movie_reviews
# Create LargeDataSet
example_data_for_large <- example_data
empty_vector <- vector(length = nrow(example_data))
empty_vector[] <- NA
example_data_for_large$citation <- empty_vector
example_data_for_large$bib_entry <- empty_vector
example_data_for_large$license <- empty_vector
example_data_for_large$url_license <- empty_vector
example_data_for_large$text_license <- empty_vector
example_data_for_large$url_source <- empty_vector

example_data_large <- LargeDataSetForText$new()
example_data_large$add_from_data.frame(example_data_for_large)

example_data_large_single <- LargeDataSetForText$new()
example_data_large_single$add_from_data.frame(example_data_for_large[1, ])


# config
ml_frameworks <- c(
  "tensorflow",
  "pytorch"
)
base_model_list <- list(
  # "tensorflow"=c("bert",
  #               "roberta",
  #               "longformer",
  #               "funnel",
  #               "deberta_v2"
  #               ),
  "pytorch" = c( # "bert",
    # "roberta",
    # "longformer",
    # "funnel",
    # "deberta_v2",
    "mpnet"
  )
)
save_format_list <- list(
  "tensorflow" = c("auto"),
  "pytorch" = c("auto", "pt", "safetensors")
)

pooling_type_list <- list(
  "funnel" = c("cls"),
  "bert" = c("cls", "average"),
  "roberta" = c("cls", "average"),
  "longformer" = c("cls", "average"),
  "deberta_v2" = c("cls", "average"),
  "mpnet" = c("cls", "average")
)

max_layers <- 1:2

# Start tests--------------------------------------------------------------------

for (framework in ml_frameworks) {
  for (base_model in base_model_list[[framework]]) {
    # Set path to the base model
    model_path <- paste0(
      root_path_data, "/",
      framework, "/",
      base_model
    )

    for (pooling_type in pooling_type_list[[base_model]]) {
      for (max_layer in max_layers) {
        for (min_layer in 1:max_layer) {
          # Create Model
          text_embedding_model <- TextEmbeddingModel$new()
          text_embedding_model$configure(
            model_name = paste0(base_model, "_embedding"),
            model_label = paste0("Text Embedding via", base_model),
            model_language = "english",
            method = base_model,
            ml_framework = framework,
            max_length = 20,
            chunks = 4,
            overlap = 10,
            emb_layer_min = min_layer,
            emb_layer_max = max_layer,
            emb_pool_type = pooling_type,
            model_dir = model_path
          )

          # Central methods--------------------------------------------------------
          # Check history
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "history"), {
            history <- text_embedding_model$last_training$history
            expect_equal(nrow(history), 2)
            expect_equal(ncol(history), 3)
            expect_true("epoch" %in% colnames(history))
            expect_true("loss" %in% colnames(history))
            expect_true("val_loss" %in% colnames(history))
          })

          # Check Configuration of the model
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "configuration"), {
            expect_equal(text_embedding_model$get_transformer_components()$emb_layer_min, min_layer)
            expect_equal(text_embedding_model$get_transformer_components()$emb_layer_max, max_layer)
            expect_equal(text_embedding_model$get_transformer_components()$emb_pool_type, pooling_type)
          })

          # Method embed
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "embed"), {
            # general
            embeddings <- text_embedding_model$embed(
              raw_text = example_data$text[1:10],
              doc_id = example_data$id[1:10]
            )
            expect_s3_class(embeddings, class = "EmbeddedText")
            expect_false(embeddings$is_compressed())
            expect_equal(embeddings$n_rows(), 10)

            # Check if embeddings are array with 3 dimensions
            expect_equal(length(dim(embeddings$embeddings)), 3)

            # check case order invariance
            perm <- sample(x = 1:10, size = 10, replace = FALSE)
            embeddings_perm <- text_embedding_model$embed(
              raw_text = example_data$text[perm],
              doc_id = example_data$id[perm]
            )
            for (i in 1:10) {
              expect_equal(embeddings$embeddings[i, , ],
                embeddings_perm$embeddings[which(perm == i), , ],
                tolerance = 1e-5
              )
            }
          })
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "embed single case"), {
            embeddings <- text_embedding_model$embed(
              raw_text = example_data$text[1:1],
              doc_id = example_data$id[1:1]
            )
            expect_s3_class(embeddings, class = "EmbeddedText")
            expect_false(embeddings$is_compressed())
            expect_equal(embeddings$n_rows(), 1)
          })

          # Method embed_large
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "embed_large"), {
            # general
            embeddings <- text_embedding_model$embed_large(example_data_large)
            expect_s3_class(embeddings, class = "LargeDataSetForTextEmbeddings")
            expect_false(embeddings$is_compressed())
            expect_equal(embeddings$n_rows(), nrow(example_data))
          })

          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "embed_large with log"), {
            # general
            log_dir <- paste0(root_path_results, "/", generate_id())
            if (dir.exists(log_dir) == FALSE) {
              dir.create(log_dir)
            }
            log_file <- paste0(log_dir, "aifeducation_state.log")
            embeddings <- text_embedding_model$embed_large(example_data_large,
              log_file = log_file,
              log_write_interval = 2
            )
            expect_s3_class(embeddings, class = "LargeDataSetForTextEmbeddings")
            expect_false(embeddings$is_compressed())
            expect_equal(embeddings$n_rows(), nrow(example_data))

            state_log_exists <- file.exists(log_file)
            expect_true(state_log_exists)
            if (state_log_exists) {
              log_state <- read.csv(log_file)
              expect_equal(nrow(log_state), 3)
              expect_equal(ncol(log_state), 3)
              expect_equal(colnames(log_state), c("value", "total", "message"))
            }
          })

          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "embed_large single case"), {
            embeddings <- text_embedding_model$embed_large(example_data_large_single)
            expect_s3_class(embeddings, class = "LargeDataSetForTextEmbeddings")
            expect_false(embeddings$is_compressed())
            expect_equal(embeddings$n_rows(), 1)
          })

          # encoding
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "encoding"), {
            encodings <- text_embedding_model$encode(
              raw_text = example_data$text[1:10],
              token_encodings_only = TRUE,
              to_int = TRUE
            )
            expect_length(encodings, 10)
            expect_type(encodings, type = "list")

            encodings <- text_embedding_model$encode(
              raw_text = example_data$text[1:10],
              token_encodings_only = TRUE,
              to_int = FALSE
            )
            expect_length(encodings, 10)
            expect_type(encodings, type = "list")
          })

          # Decoding
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "decoding"), {
            encodings <- text_embedding_model$encode(
              raw_text = example_data$text[1:10],
              token_encodings_only = TRUE,
              to_int = TRUE
            )
            decodings <- text_embedding_model$decode(encodings,
              to_token = FALSE
            )
            expect_length(decodings, 10)
            expect_type(decodings, type = "list")

            decodings <- text_embedding_model$decode(encodings,
              to_token = TRUE
            )
            expect_length(decodings, 10)
            expect_type(decodings, type = "list")
          })

          # Method get_special_tokens
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "get_special_tokens"), {
            tokens <- text_embedding_model$get_special_tokens()
            expect_equal(nrow(tokens), 7)
            expect_equal(ncol(tokens), 3)
          })

          # Method fill_mask
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "fill_mask"), {
            tokens <- text_embedding_model$get_special_tokens()
            mask_token <- tokens[which(tokens[, 1] == "mask_token"), 2]

            first_solution <- text_embedding_model$fill_mask(
              text = paste("This is a", mask_token, "."),
              n_solutions = 5
            )

            expect_equal(length(first_solution), 1)
            expect_true(is.data.frame(first_solution[[1]]))
            expect_equal(nrow(first_solution[[1]]), 5)
            expect_equal(ncol(first_solution[[1]]), 3)

            second_solution <- text_embedding_model$fill_mask(
              text = paste("This is a", mask_token, "."),
              n_solutions = 1
            )
            expect_equal(length(second_solution), 1)
            expect_true(is.data.frame(second_solution[[1]]))
            expect_equal(nrow(second_solution[[1]]), 1)
            expect_equal(ncol(second_solution[[1]]), 3)

            third_solution <- text_embedding_model$fill_mask(
              text = paste(
                "This is a", mask_token, ".",
                "The weather is", mask_token, "."
              ),
              n_solutions = 5
            )
            expect_equal(length(third_solution), 2)
            for (i in 1:2) {
              expect_true(is.data.frame(third_solution[[i]]))
              expect_equal(nrow(third_solution[[i]]), 5)
              expect_equal(ncol(third_solution[[i]]), 3)
            }
          })

          # Method Saving and Loading-----------------------------------------------------
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "method_save_load"), {
            folder_name <- paste0(
              "method_save_load_",
              framework, "_",
              base_model, "_",
              pooling_type, "_",
              max_layer, "_",
              min_layer
            )
            save_location <- paste0(root_path_results, "/", folder_name)
            if (dir.exists(save_location) == FALSE) {
              dir.create(save_location)
            }

            # embeddings for saving
            embeddings <- text_embedding_model$embed(
              raw_text = example_data$text[1:10],
              doc_id = example_data$id[1:10]
            )

            # Save Model
            expect_no_error(text_embedding_model$save(
              dir_path = root_path_results,
              folder_name = folder_name
            ))
            # Load Model
            expect_no_error(text_embedding_model$load(dir_path = save_location))

            # embeddings after loading saving
            embeddings_2 <- text_embedding_model$embed(
              raw_text = example_data$text[1:10],
              doc_id = example_data$id[1:10]
            )
            # compare embeddings
            i <- sample(x = seq.int(from = 1, to = embeddings$n_rows()), size = 1)
            expect_equal(embeddings$embeddings[i, , , drop = FALSE],
              embeddings_2$embeddings[i, , , drop = FALSE],
              tolerance = 1e-6
            )
          })

          # Function Saving and Loading
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "function_save_load"), {
            folder_name <- paste0(
              "function_save_load_",
              framework, "_",
              base_model, "_",
              pooling_type, "_",
              max_layer, "_",
              min_layer
            )
            save_location <- paste0(root_path_results, "/", folder_name)
            if (dir.exists(save_location) == FALSE) {
              dir.create(save_location)
            }

            # embeddings for saving
            embeddings <- text_embedding_model$embed(
              raw_text = example_data$text[1:10],
              doc_id = example_data$id[1:10]
            )

            # Save Model
            expect_no_error(save_to_disk(
              object = text_embedding_model,
              dir_path = root_path_results,
              folder_name = folder_name
            ))
            # Load Model
            text_embedding_model_reloaded <- load_from_disk(dir_path = save_location)

            # embeddings after loading saving
            embeddings_2 <- text_embedding_model_reloaded$embed(
              raw_text = example_data$text[1:10],
              doc_id = example_data$id[1:10]
            )
            # compare embeddings
            i <- sample(x = seq.int(from = 1, to = embeddings$n_rows()), size = 1)
            expect_equal(embeddings$embeddings[i, , , drop = FALSE],
              embeddings_2$embeddings[i, , , drop = FALSE],
              tolerance = 1e-6
            )

            # Check loaded history
            expect_s3_class(text_embedding_model_reloaded,
              class = "TextEmbeddingModel"
            )
            history <- text_embedding_model_reloaded$last_training$history
            expect_equal(nrow(history), 2)
            expect_equal(ncol(history), 3)
            expect_true("epoch" %in% colnames(history))
            expect_true("loss" %in% colnames(history))
            expect_true("val_loss" %in% colnames(history))

            # Check loaded sustainability data
            sustain_data <- text_embedding_model_reloaded$get_sustainability_data()

            # One row for creation and one row for training
            expect_equal(nrow(sustain_data), 2)

            # Check tokenizer statistics
            expect_equal(nrow(text_embedding_model_reloaded$tokenizer_statistics), 2)
          })

          # Documentation----------------------------------------------------------
          # Description
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "description"), {
            text_embedding_model$set_model_description(
              eng = "Description",
              native = "Beschreibung",
              abstract_eng = "Abstract",
              abstract_native = "Zusammenfassung",
              keywords_eng = c("Test", "Neural Net"),
              keywords_native = c("Test", "Neuronales Netz")
            )
            desc <- text_embedding_model$get_model_description()
            expect_equal(
              object = desc$eng,
              expected = "Description"
            )
            expect_equal(
              object = desc$native,
              expected = "Beschreibung"
            )
            expect_equal(
              object = desc$abstract_eng,
              expected = "Abstract"
            )
            expect_equal(
              object = desc$abstract_native,
              expected = "Zusammenfassung"
            )
            expect_equal(
              object = desc$keywords_eng,
              expected = c("Test", "Neural Net")
            )
            expect_equal(
              object = desc$keywords_native,
              expected = c("Test", "Neuronales Netz")
            )
          })

          # Software License
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "software license"), {
            text_embedding_model$set_software_license("test_license")
            expect_equal(
              object = text_embedding_model$get_software_license(),
              expected = c("test_license")
            )
          })

          # Documentation License
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "documentation license"), {
            text_embedding_model$set_documentation_license("test_license")
            expect_equal(
              object = text_embedding_model$get_documentation_license(),
              expected = c("test_license")
            )
          })

          # Publication information
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "Publication information"), {
            text_embedding_model$set_publication_info(
              type = "developer",
              authors = personList(
                person(given = "Max", family = "Mustermann")
              ),
              citation = "Test Classifier",
              url = "https://Test.html"
            )

            text_embedding_model$set_publication_info(
              type = "modifier",
              authors = personList(
                person(given = "Nico", family = "Meyer")
              ),
              citation = "Test Classifier Revisited",
              url = "https://Test_revisited.html"
            )


            pub_info <- text_embedding_model$get_publication_info()

            expect_equal(
              object = pub_info$developed_by$authors,
              expected = personList(
                person(given = "Max", family = "Mustermann")
              )
            )
            expect_equal(
              object = pub_info$developed_by$citation,
              expected = "Test Classifier"
            )
            expect_equal(
              object = pub_info$developed_by$url,
              expected = "https://Test.html"
            )

            expect_equal(
              object = pub_info$modified_by$authors,
              expected = personList(
                person(given = "Nico", family = "Meyer")
              )
            )
            expect_equal(
              object = pub_info$modified_by$citation,
              expected = "Test Classifier Revisited"
            )
            expect_equal(
              object = pub_info$modified_by$url,
              expected = "https://Test_revisited.html"
            )
          })

          # Method get_components
          test_that(paste(framework, base_model, pooling_type, max_layer, min_layer, "get_components"), {
            expect_no_error(text_embedding_model$get_transformer_components())
            expect_no_error(text_embedding_model$get_basic_components())
          })
        }
      }
    }
  }
}
