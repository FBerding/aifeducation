testthat::skip_on_cran()
if (Sys.getenv("CI") == "true") {
  testthat::skip_if_not(
    condition = check_aif_py_modules(trace = FALSE, check = "pytorch"),
    message = "Necessary python modules not available"
  )
} else {
  testthat::skip_if_not(
    condition = check_aif_py_modules(trace = FALSE, check = "all"),
    message = "Necessary python modules not available"
  )
  # SetUp tensorflow
  aifeducation::set_config_gpu_low_memory()
  set_config_tf_logger("ERROR")
  set_config_os_environ_logger("ERROR")
}

# Skip Tests
skip_creation_test <- FALSE
skip_method_save_load <- FALSE
skip_function_save_load <- FALSE
skip_training_test <- FALSE
skip_documentation <- FALSE
class_range <- c(2, 3)

# Can be set to "all"
local_samples <- 50

# Git Hub specific
n_git_samples <- 50

prob_precision <- 1e-4
prob_precision_fourier <- 1e-3

if (Sys.getenv("CI") == "true") {
  include_tensorflow <- FALSE
  skip_overfitting_test <- TRUE
} else {
  include_tensorflow <- FALSE
  skip_overfitting_test <- FALSE
}


# SetUp-------------------------------------------------------------------------
# Set paths
root_path_general_data <- testthat::test_path("test_data/Embeddings")
root_path_data <- testthat::test_path("test_data/classifier")
root_path_general_data <- testthat::test_path("test_data/Embeddings")
create_dir(testthat::test_path("test_artefacts"), FALSE)
root_path_results <- testthat::test_path("test_artefacts/TeClassifierRegular")
create_dir(root_path_results, FALSE)
root_path_feature_extractor <- testthat::test_path("test_data_tmp/classifier/feature_extractor_pytorch")

# SetUp datasets
# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

# Load Embeddings
imdb_embeddings <- load_from_disk(paste0(root_path_general_data, "/imdb_embeddings"))

test_embeddings_large <- imdb_embeddings$convert_to_LargeDataSetForTextEmbeddings()
test_embeddings <- test_embeddings_large$convert_to_EmbeddedText()

test_embeddings_reduced <- test_embeddings$clone(deep = TRUE)
test_embeddings_reduced$embeddings <- test_embeddings_reduced$embeddings[1:5, , ]
test_embeddings_reduced_LD <- test_embeddings_reduced$convert_to_LargeDataSetForTextEmbeddings()

test_embeddings_single_case <- test_embeddings$clone(deep = TRUE)
test_embeddings_single_case$embeddings <- test_embeddings_single_case$embeddings[1, , , drop = FALSE]
test_embeddings_single_case_LD <- test_embeddings_single_case$convert_to_LargeDataSetForTextEmbeddings()

# Config
if (include_tensorflow == FALSE) {
  ml_frameworks <- c("pytorch")
} else {
  ml_frameworks <- c("pytorch", "tensorflow")
}

rec_list_layers <- list(0, 1, 2)
rec_list_size <- list(2, 8)
rec_type_list <- list("gru", "lstm")
rec_bidirectiona_list <- list(TRUE, FALSE)
dense_list_layers <- list(0, 1, 1)
dense_list_size <- list(3, 5)
r_encoder_list <- list(0, 1, 2)
attention_list <- list("fourier", "multihead")
pos_embedding_list <- list(TRUE, FALSE)

sc_list <- list(FALSE, TRUE)
pl_list <- list(FALSE, TRUE)

# Load feature extractors
feature_extractor_list <- NULL
feature_extractor_list["tensorflow"] <- list(list(NULL))

if (file.exists(root_path_feature_extractor)) {
  feature_extractor_list["pytorch"] <- list(
    list(
      load_from_disk(root_path_feature_extractor),
      NULL
    )
  )
} else {
  feature_extractor_list["pytorch"] <- list(
    list(
      NULL
    )
  )
}

# Prepare data for different classification types---------------------------
target_data <- NULL
target_levels <- NULL
for (n_classes in class_range) {
  example_data <- imdb_movie_reviews

  rownames(example_data) <- rownames(test_embeddings$embeddings)
  example_data$id <- rownames(test_embeddings$embeddings)
  example_data <- example_data[intersect(
    rownames(example_data), rownames(test_embeddings$embeddings)
  ), ]

  example_data$label <- as.character(example_data$label)
  example_data$label[c(201:300)] <- NA
  if (n_classes > 2) {
    example_data$label[c(201:250)] <- "medium"
    tmp_target_levels <- c("neg", "medium", "pos")
  } else {
    tmp_target_levels <- c("neg", "pos")
  }
  example_targets <- as.factor(example_data$label)
  names(example_targets) <- example_data$id

  target_data[n_classes] <- list(example_targets)
  target_levels[n_classes] <- list(tmp_target_levels)
}

for (framework in ml_frameworks) {
  # Start Tests-------------------------------------------------------------------------------
  # Test creation and prediction of the classifier----------------------------
  if (!skip_creation_test) {
    all_test_combinations <- NULL
    for (feature_extractor in feature_extractor_list[[framework]]) {
      for (rec_layers in rec_list_layers) {
        for (dense_layers in dense_list_layers) {
          for (r in r_encoder_list) {
            for (attention in attention_list) {
              for (pos_embedding in pos_embedding_list) {
                for (rec_type in rec_type_list) {
                  for (rec_bidirectional in rec_bidirectiona_list) {
                    all_test_combinations[length(all_test_combinations) + 1] <- list(
                      list(
                        feature_extractor = feature_extractor,
                        rec_layers = rec_layers,
                        dense_layers = dense_layers,
                        r = r,
                        attention = attention,
                        pos_embedding = pos_embedding,
                        rec_type = rec_type,
                        rec_bidirectional = rec_bidirectional
                      )
                    )
                  }
                }
              }
            }
          }
        }
      }
    }

    if (local_samples == "all") {
      n_local_samples <- length(all_test_combinations)
    } else {
      n_local_samples <- local_samples
    }

    # If on github use only a small random sample
    if (Sys.getenv("CI") != "true") {
      test_combinations <- all_test_combinations[sample(
        x = seq.int(
          from = 1,
          to = length(all_test_combinations)
        ),
        size = n_local_samples,
        replace = FALSE
      )]
    } else {
      test_combinations <- all_test_combinations[sample(
        x = seq.int(
          from = 1,
          to = length(all_test_combinations)
        ),
        size = n_git_samples,
        replace = FALSE
      )]
    }

    for (i in 1:length(test_combinations)) {
      classifier <- NULL
      n_classes <- sample(x = class_range, size = 1, replace = FALSE)
      gc()

      dense_size <- dense_list_size[[sample(x = seq.int(from = 1, to = length(dense_list_size)), size = 1)]]
      rec_size <- rec_list_size[[sample(x = seq.int(from = 1, to = length(rec_list_size)), size = 1)]]

      classifier <- TEClassifierRegular$new()
      classifier$configure(
        ml_framework = framework,
        name = "movie_review_classifier",
        label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
        text_embeddings = test_embeddings,
        feature_extractor = test_combinations[[i]]$feature_extractor,
        target_levels = target_levels[[n_classes]],
        dense_layers = test_combinations[[i]]$dense_layers,
        dense_size = dense_size,
        rec_layers = test_combinations[[i]]$rec_layers,
        rec_size = rec_size,
        rec_type = test_combinations[[i]]$rec_type,
        rec_bidirectional = test_combinations[[i]]$rec_bidirectional,
        self_attention_heads = 2,
        add_pos_embedding = test_combinations[[i]]$pos_embedding,
        attention_type = test_combinations[[i]]$attention,
        rec_dropout = 0.5,
        encoder_dropout = 0.1,
        repeat_encoder = test_combinations[[i]]$r,
        recurrent_dropout = 0.4
      )

      test_that(paste(
        "no sustainability tracking", framework,
        "n_classes", n_classes,
        "features_extractor", !is.null(feature_extractor),
        "rec_layers", paste(test_combinations[[i]]$rec_layers, rec_size),
        "rec_type", test_combinations[[i]]$rec_type,
        "rec_bidirectional", test_combinations[[i]]$rec_bidirectional,
        "dense_layers", paste(test_combinations[[i]]$dense_layers, dense_size),
        "encoder", test_combinations[[i]]$r,
        "attention", test_combinations[[i]]$attention,
        "pos", test_combinations[[i]]$pos_embedding
      ), {
        expect_false(classifier$get_sustainability_data()$sustainability_tracked)
      })


      test_that(paste(
        "predict - basic", framework,
        "n_classes", n_classes,
        "features_extractor", !is.null(feature_extractor),
        "rec_layers", paste(test_combinations[[i]]$rec_layers, rec_size),
        "rec_type", test_combinations[[i]]$rec_type,
        "rec_bidirectional", test_combinations[[i]]$rec_bidirectional,
        "dense_layers", paste(test_combinations[[i]]$dense_layers, dense_size),
        "encoder", test_combinations[[i]]$r,
        "attention", test_combinations[[i]]$attention,
        "pos", test_combinations[[i]]$pos_embedding
      ), {
        expect_s3_class(classifier,
          class = "TEClassifierRegular"
        )

        predictions <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(
          object = length(predictions$expected_category),
          expected = nrow(test_embeddings_reduced$embeddings)
        )
      })

      test_that(paste(
        "predict - single case", framework,
        "n_classes", n_classes,
        "features_extractor", !is.null(feature_extractor),
        "rec_layers", paste(test_combinations[[i]]$rec_layers, rec_size),
        "rec_type", test_combinations[[i]]$rec_type,
        "rec_bidirectional", test_combinations[[i]]$rec_bidirectional,
        "dense_layers", paste(test_combinations[[i]]$dense_layers, dense_size),
        "encoder", test_combinations[[i]]$r,
        "attention", test_combinations[[i]]$attention,
        "pos", test_combinations[[i]]$pos_embedding
      ), {
        prediction <- classifier$predict(
          newdata = test_embeddings_single_case,
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(
          object = nrow(prediction),
          expected = 1
        )

        prediction_LD <- classifier$predict(
          newdata = test_embeddings_single_case_LD,
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(
          object = nrow(prediction_LD),
          expected = 1
        )
      })

      test_that(paste(
        "predict - randomness", framework,
        "n_classes", n_classes,
        "features_extractor", !is.null(feature_extractor),
        "rec_layers", paste(test_combinations[[i]]$rec_layers, rec_size),
        "rec_type", test_combinations[[i]]$rec_type,
        "rec_bidirectional", test_combinations[[i]]$rec_bidirectional,
        "dense_layers", paste(test_combinations[[i]]$dense_layers, dense_size),
        "encoder", test_combinations[[i]]$r,
        "attention", test_combinations[[i]]$attention,
        "pos", test_combinations[[i]]$pos_embedding
      ), {
        # EmbeddedText
        predictions <- NULL
        predictions_2 <- NULL
        predictions <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )
        predictions_2 <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(predictions[, 1:(ncol(predictions) - 1)], predictions_2[, 1:(ncol(predictions_2) - 1)],
          tolerance = 1e-6
        )

        # LargeDataSetForTextEmbeddings
        predictions <- NULL
        predictions_2 <- NULL
        predictions <- classifier$predict(
          newdata = test_embeddings_reduced_LD,
          batch_size = 2,
          ml_trace = 0
        )
        predictions_2 <- classifier$predict(
          newdata = test_embeddings_reduced_LD,
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(predictions[, 1:(ncol(predictions) - 1)], predictions_2[, 1:(ncol(predictions_2) - 1)],
          tolerance = 1e-6
        )
      })


      test_that(paste(
        "predict - order invariance", framework,
        "n_classes", n_classes,
        "features_extractor", !is.null(feature_extractor),
        "rec_layers", paste(test_combinations[[i]]$rec_layers, rec_size),
        "rec_type", test_combinations[[i]]$rec_type,
        "rec_bidirectional", test_combinations[[i]]$rec_bidirectional,
        "dense_layers", paste(test_combinations[[i]]$dense_layers, dense_size),
        "encoder", test_combinations[[i]]$r,
        "attention", test_combinations[[i]]$attention,
        "pos", test_combinations[[i]]$pos_embedding
      ), {
        embeddings_ET_perm <- test_embeddings_reduced$clone(deep = TRUE)
        perm <- sample(x = seq.int(from = 1, to = nrow(embeddings_ET_perm$embeddings)), replace = FALSE)
        embeddings_ET_perm$embeddings <- embeddings_ET_perm$embeddings[perm, , , drop = FALSE]

        ids <- rownames(test_embeddings_reduced$embeddings)

        # EmbeddedText
        predictions <- NULL
        predictions_Perm <- NULL
        predictions <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 50,
          ml_trace = 0
        )
        predictions_Perm <- classifier$predict(
          newdata = embeddings_ET_perm,
          batch_size = 50,
          ml_trace = 0
        )

        if (test_combinations[[i]]$attention != "fourier") {
          expect_equal(predictions[ids, 1:(ncol(predictions) - 1)], predictions_Perm[ids, 1:(ncol(predictions_Perm) - 1)],
            tolerance = prob_precision
          )
        } else {
          expect_equal(predictions[ids, 1:(ncol(predictions) - 1)], predictions_Perm[ids, 1:(ncol(predictions_Perm) - 1)],
            tolerance = prob_precision_fourier
          )
        }

        # LargeDataSetForTextEmbeddings
        predictions <- NULL
        predictions_Perm <- NULL
        predictions <- classifier$predict(
          newdata = test_embeddings_reduced_LD,
          batch_size = 50,
          ml_trace = 0
        )
        predictions_Perm <- classifier$predict(
          newdata = embeddings_ET_perm$convert_to_LargeDataSetForTextEmbeddings(),
          batch_size = 50,
          ml_trace = 0
        )

        if (test_combinations[[i]]$attention != "fourier") {
          expect_equal(predictions[ids, 1:(ncol(predictions) - 1)], predictions_Perm[ids, 1:(ncol(predictions_Perm) - 1)],
            tolerance = prob_precision
          )
        } else {
          expect_equal(predictions[ids, 1:(ncol(predictions) - 1)], predictions_Perm[ids, 1:(ncol(predictions_Perm) - 1)],
            tolerance = prob_precision_fourier
          )
        }
      })

      test_that(paste(
        "predict - data source invariance", framework,
        "n_classes", n_classes,
        "features_extractor", !is.null(feature_extractor),
        "rec_layers", paste(test_combinations[[i]]$rec_layers, rec_size),
        "rec_type", test_combinations[[i]]$rec_type,
        "rec_bidirectional", test_combinations[[i]]$rec_bidirectional,
        "dense_layers", paste(test_combinations[[i]]$dense_layers, dense_size),
        "encoder", test_combinations[[i]]$r,
        "attention", test_combinations[[i]]$attention,
        "pos", test_combinations[[i]]$pos_embedding
      ), {
        predictions_ET <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )
        predictions_LD <- classifier$predict(
          newdata = test_embeddings_reduced_LD,
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(predictions_ET[, 1:(ncol(predictions_ET) - 1)], predictions_LD[, 1:(ncol(predictions_LD) - 1)],
          tolerance = 1e-6
        )
      })
      gc()
    }
  }


  # Test training of the classifier-------------------------------------------
  if (!skip_training_test) {
    for (feature_extractor in feature_extractor_list[[framework]]) {
      for (use_sc in sc_list) {
        for (use_pl in pl_list) {
          # Randomly select a configuration for training
          n_classes <- sample(x = class_range, size = 1, replace = FALSE)

          rec_layers <- rec_list_layers[[sample(x = seq.int(from = 1, to = length(rec_list_layers)), size = 1)]]
          dense_layers <- dense_list_layers[[sample(x = seq.int(from = 1, to = length(dense_list_layers)), size = 1)]]
          dense_size <- dense_list_size[[sample(x = seq.int(from = 1, to = length(dense_list_size)), size = 1)]]
          rec_size <- rec_list_size[[sample(x = seq.int(from = 1, to = length(rec_list_size)), size = 1)]]

          rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
          rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]

          repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
          attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
          add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

          # Create directory for saving checkpoint for every training
          train_path <- paste0(root_path_results, "/", "train_", generate_id())
          if (dir.exists(train_path) == FALSE) {
            dir.create(train_path)
          }
          classifier <- TEClassifierRegular$new()
          classifier$configure(
            ml_framework = framework,
            name = paste0("movie_review_classifier_", "classes_", n_classes),
            label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
            text_embeddings = test_embeddings,
            target_levels = target_levels[[n_classes]],
            feature_extractor = feature_extractor,
            dense_layers = dense_layers,
            dense_size = dense_size,
            rec_layers = rec_layers,
            rec_size = rec_size,
            rec_type = rec_type,
            rec_bidirectional = rec_bidirectional,
            self_attention_heads = 1,
            intermediate_size = NULL,
            attention_type = attention_type,
            add_pos_embedding = add_pos_embedding,
            rec_dropout = 0.1,
            repeat_encoder = repeat_encoder,
            dense_dropout = 0.4,
            recurrent_dropout = 0.4,
            encoder_dropout = 0.1,
            optimizer = "adam"
          )

          test_that(paste(
            framework, "training",
            "n_classes", n_classes,
            "sc", use_sc,
            "pl", use_pl,
            "features_extractor", !is.null(feature_extractor),
            "rec_layers", paste(rec_layers, rec_size),
            "rec_type", rec_type,
            "rec_bidirectional", rec_bidirectional,
            "dense_layers", paste(dense_layers, dense_size),
            "encoder", repeat_encoder,
            "attention", attention_type,
            "pos", add_pos_embedding
          ), {
            expect_no_error(
              classifier$train(
                data_embeddings = test_embeddings,
                data_targets = target_data[[n_classes]],
                data_folds = 2,
                balance_class_weights = TRUE,
                balance_sequence_length = TRUE,
                use_sc = use_sc,
                sc_method = "dbsmote",
                sc_min_k = 1,
                sc_max_k = 2,
                use_pl = use_pl,
                pl_max_steps = 2,
                pl_max = 1.00,
                pl_anchor = 1.00,
                pl_min = 0.00,
                sustain_track = TRUE,
                sustain_iso_code = "DEU",
                sustain_region = NULL,
                sustain_interval = 15,
                epochs = 20,
                batch_size = 32,
                dir_checkpoint = train_path,
                trace = FALSE,
                ml_trace = 0,
                n_cores = 2
              )
            )
            expect_true(classifier$get_sustainability_data()$sustainability_tracked)
          })

          # Clean Directory
          unlink(
            x = train_path,
            recursive = TRUE
          )
          gc()
        }
      }
    }
  }



  # Method save and load------------------------------------------------------
  if (!skip_method_save_load) {
    for (feature_extractor in feature_extractor_list[[framework]]) {
      test_that(paste(framework, !is.null(feature_extractor), "method save and load"), {
        # Randomly select a configuration for training
        n_classes <- sample(x = class_range, size = 1, replace = FALSE)

        rec_layers <- rec_list_layers[[sample(x = seq.int(from = 1, to = length(rec_list_layers)), size = 1)]]
        dense_layers <- dense_list_layers[[sample(x = seq.int(from = 1, to = length(dense_list_layers)), size = 1)]]
        dense_size <- dense_list_size[[sample(x = seq.int(from = 1, to = length(dense_list_size)), size = 1)]]
        rec_size <- rec_list_size[[sample(x = seq.int(from = 1, to = length(rec_list_size)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier <- TEClassifierRegular$new()
        classifier$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels[[n_classes]],
          feature_extractor = feature_extractor,
          dense_layers = dense_layers,
          dense_size = dense_size,
          rec_layers = rec_layers,
          rec_size = rec_size,
          rec_type = rec_type,
          rec_bidirectional = rec_bidirectional,
          self_attention_heads = 1,
          intermediate_size = NULL,
          attention_type = attention_type,
          add_pos_embedding = add_pos_embedding,
          rec_dropout = 0.1,
          repeat_encoder = 1,
          dense_dropout = 0.4,
          recurrent_dropout = 0.4,
          encoder_dropout = 0.1,
          optimizer = "adam"
        )

        # Predictions before saving and loading
        predictions <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )

        # Save and load
        folder_name <- paste0("method_save_load_", generate_id())
        dir_path <- paste0(root_path_results, "/", folder_name)
        classifier$save(
          dir_path = root_path_results,
          folder_name = folder_name
        )
        classifier$load(dir_path = dir_path)

        # Predict after loading
        predictions_2 <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )

        # Compare predictions
        i <- sample(x = seq.int(from = 1, to = nrow(predictions)), size = 1)
        expect_equal(predictions[i, , drop = FALSE],
          predictions_2[i, , drop = FALSE],
          tolerance = 1e-6
        )

        # Clean Directory
        unlink(
          x = dir_path,
          recursive = TRUE
        )
      })
      gc()
    }
  }

  # Function for loading and saving models-----------------------------------
  if (!skip_function_save_load) {
    for (feature_extractor in feature_extractor_list[[framework]]) {
      test_that(paste(framework, !is.null(feature_extractor), "function save and load"), {
        # Randomly select a configuration for training
        n_classes <- sample(x = class_range, size = 1, replace = FALSE)

        rec_layers <- rec_list_layers[[sample(x = seq.int(from = 1, to = length(rec_list_layers)), size = 1)]]
        dense_layers <- dense_list_layers[[sample(x = seq.int(from = 1, to = length(dense_list_layers)), size = 1)]]
        dense_size <- dense_list_size[[sample(x = seq.int(from = 1, to = length(dense_list_size)), size = 1)]]
        rec_size <- rec_list_size[[sample(x = seq.int(from = 1, to = length(rec_list_size)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier <- TEClassifierRegular$new()
        classifier$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels[[n_classes]],
          feature_extractor = feature_extractor,
          dense_layers = dense_layers,
          dense_size = dense_size,
          rec_layers = rec_layers,
          rec_size = rec_size,
          rec_type = rec_type,
          rec_bidirectional = rec_bidirectional,
          self_attention_heads = 1,
          intermediate_size = NULL,
          attention_type = attention_type,
          add_pos_embedding = add_pos_embedding,
          rec_dropout = 0.1,
          repeat_encoder = repeat_encoder,
          dense_dropout = 0.4,
          recurrent_dropout = 0.4,
          encoder_dropout = 0.1,
          optimizer = "adam"
        )

        # Predictions before saving and loading
        predictions <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )

        # Save and load
        folder_name <- paste0("function_save_load_", generate_id())
        dir_path <- paste0(root_path_results, "/", folder_name)
        save_to_disk(
          object = classifier,
          dir_path = root_path_results,
          folder_name = folder_name
        )
        classifier <- NULL
        classifier <- load_from_disk(dir_path = dir_path)

        # Predict after loading
        predictions_2 <- classifier$predict(
          newdata = test_embeddings_reduced,
          batch_size = 2,
          ml_trace = 0
        )

        # Compare predictions
        i <- sample(x = seq.int(from = 1, to = nrow(predictions)), size = 1)
        expect_equal(predictions[i, , drop = FALSE],
          predictions_2[i, , drop = FALSE],
          tolerance = 1e-6
        )

        # Clean Directory
        unlink(
          x = dir_path,
          recursive = TRUE
        )
      })
      gc()
    }
  }

  # Overfitting test----------------------------------------------------------
  if (!skip_overfitting_test) {
    test_that(paste(framework, n_classes, "overfitting test with log"), {
      # Create directory for saving checkpoint for every training
      train_path <- paste0(root_path_results, "/", "train_", generate_id())
      create_dir(train_path, FALSE)

      # Randomly select a configuration for training
      n_classes <- sample(x = class_range, size = 1, replace = FALSE)

      rec_layers <- rec_list_layers[[sample(x = seq.int(from = 1, to = length(rec_list_layers)), size = 1)]]
      dense_layers <- dense_list_layers[[sample(x = seq.int(from = 1, to = length(dense_list_layers)), size = 1)]]
      # dense_size=dense_list_size[[sample(x=seq.int(from = 1,to=length(dense_list_size)),size = 1)]]
      # rec_size=rec_list_size[[sample(x=seq.int(from = 1,to=length(rec_list_size)),size = 1)]]
      rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
      rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
      # repeat_encoder=r_encoder_list[[sample(x=seq.int(from = 1,to=length(r_encoder_list)),size = 1)]]
      attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
      add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

      classifier_overfitting <- TEClassifierRegular$new()
      classifier_overfitting$configure(
        ml_framework = framework,
        name = paste0("movie_review_classifier_", "classes_", n_classes),
        label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
        text_embeddings = test_embeddings,
        target_levels = target_levels[[n_classes]],
        feature_extractor = NULL,
        dense_layers = 0,
        dense_size = 10,
        rec_size = c(10),
        rec_layers = 1,
        rec_type = rec_type,
        rec_bidirectional = rec_bidirectional,
        self_attention_heads = 1,
        intermediate_size = NULL,
        attention_type = attention_type,
        add_pos_embedding = add_pos_embedding,
        rec_dropout = 0.0,
        repeat_encoder = 0,
        dense_dropout = 0.0,
        recurrent_dropout = 0.0,
        encoder_dropout = 0.0,
        optimizer = "adam"
      )

      if (n_classes < 3) {
        epochs <- 100
      } else {
        epochs <- 300
      }

      classifier_overfitting$train(
        data_embeddings = test_embeddings,
        data_targets = target_data[[n_classes]],
        data_folds = 3,
        balance_class_weights = TRUE,
        balance_sequence_length = TRUE,
        use_sc = FALSE,
        sc_method = "dbsmote",
        sc_min_k = 1,
        sc_max_k = 2,
        use_pl = FALSE,
        pl_max_steps = 2,
        pl_max = 1.00,
        pl_anchor = 1.00,
        pl_min = 0.00,
        sustain_track = TRUE,
        sustain_iso_code = "DEU",
        sustain_region = NULL,
        sustain_interval = 15,
        epochs = epochs,
        batch_size = 32,
        dir_checkpoint = train_path,
        log_dir = train_path,
        trace = FALSE,
        ml_trace = 0,
        n_cores = 2
      )

      history <- classifier_overfitting$last_training$history[[1]]$accuracy["train", ]
      expect_gte(object = max(history), expected = 0.95)

      state_log_exists <- file.exists(paste0(train_path, "/aifeducation_state.log"))
      if (framework == "pytorch") {
        expect_true(state_log_exists)
      }
      if (state_log_exists) {
        log_state <- read.csv(paste0(train_path, "/aifeducation_state.log"))
        expect_equal(nrow(log_state), 3)
        expect_equal(ncol(log_state), 3)
        expect_equal(colnames(log_state), c("value", "total", "message"))
      }

      loss_log_exists <- file.exists(paste0(train_path, "/aifeducation_loss.log"))
      if (framework == "pytorch") {
        expect_true(loss_log_exists)
      }
      if (loss_log_exists == TRUE) {
        log_loss <- read.csv(paste0(train_path, "/aifeducation_loss.log"), header = FALSE)
        expect_gte(ncol(log_loss), 2)
        expect_gte(nrow(log_loss), 2)
      }

      # Clean Directory
      unlink(
        x = train_path,
        recursive = TRUE
      )
    })
  }

  # Documentation--------------------------------------------------------------
  if (skip_documentation) {
    test_that(paste(framework, n_classes, "descriptions"), {
      # Randomly select a configuration for training
      n_classes <- sample(x = class_range, size = 1, replace = FALSE)

      rec_layers <- rec_list_layers[[sample(x = seq.int(from = 1, to = length(rec_list_layers)), size = 1)]]
      dense_layers <- dense_list_layers[[sample(x = seq.int(from = 1, to = length(dense_list_layers)), size = 1)]]
      dense_size <- dense_list_size[[sample(x = seq.int(from = 1, to = length(dense_list_size)), size = 1)]]
      rec_size <- rec_list_size[[sample(x = seq.int(from = 1, to = length(rec_list_size)), size = 1)]]
      rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
      rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
      repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
      attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
      add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

      classifier <- TEClassifierRegular$new()
      classifier$configure(
        ml_framework = framework,
        name = paste0("movie_review_classifier_", "classes_", n_classes),
        label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
        text_embeddings = test_embeddings,
        target_levels = target_levels[[n_classes]],
        feature_extractor = feature_extractor,
        dense_layers = dense_layers,
        dense_size = dense_size,
        rec_layers = rec_layers,
        rec_size = rec_size,
        rec_type = rec_type,
        rec_bidirectional = rec_bidirectional,
        self_attention_heads = 1,
        intermediate_size = NULL,
        attention_type = attention_type,
        add_pos_embedding = add_pos_embedding,
        rec_dropout = 0.1,
        repeat_encoder = repeat_encoder,
        dense_dropout = 0.4,
        recurrent_dropout = 0.4,
        encoder_dropout = 0.1,
        optimizer = "adam"
      )

      classifier$set_model_description(
        eng = "Description",
        native = "Beschreibung",
        abstract_eng = "Abstract",
        abstract_native = "Zusammenfassung",
        keywords_eng = c("Test", "Neural Net"),
        keywords_native = c("Test", "Neuronales Netz")
      )
      desc <- classifier$get_model_description()
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



      classifier$set_software_license("test_license")
      expect_equal(
        object = classifier$get_software_license(),
        expected = c("test_license")
      )



      classifier$set_documentation_license("test_license")
      expect_equal(
        object = classifier$get_documentation_license(),
        expected = c("test_license")
      )



      classifier$set_publication_info(
        authors = personList(
          person(given = "Max", family = "Mustermann")
        ),
        citation = "Test Classifier",
        url = "https://Test.html"
      )
      pub_info <- classifier$get_publication_info()
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
    })
  }
}

# Clean Directory
if (dir.exists(root_path_results)) {
  unlink(
    x = root_path_results,
    recursive = TRUE
  )
}
