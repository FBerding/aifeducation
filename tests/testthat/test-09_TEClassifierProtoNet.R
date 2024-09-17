testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

# Skip Tests
skip_creation_test <- TRUE
skip_training_test <- TRUE
skip_overfitting_test <- FALSE
skip_3_classes <- FALSE

# SetUp-------------------------------------------------------------------------
# Set paths
root_path_data <- testthat::test_path("test_data/classifier")
if (dir.exists(testthat::test_path("test_artefacts")) == FALSE) {
  dir.create(testthat::test_path("test_artefacts"))
}
root_path_results <- testthat::test_path("test_artefacts/TeClassifierProtoNet")
if (dir.exists(root_path_results) == FALSE) {
  dir.create(root_path_results)
}

# SetUp datasets
# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

# SetUp tensorflow
aifeducation::set_config_gpu_low_memory()
set_config_tf_logger("ERROR")
set_config_os_environ_logger("ERROR")

# Load Embeddings
# object is imdb_embeddings
load(paste0(root_path_data, "/imdb_embeddings.rda"))
test_embeddings_large <- imdb_embeddings$convert_to_LargeDataSetForTextEmbeddings()
test_embeddings <- test_embeddings_large$convert_to_EmbeddedText()

test_embeddings_reduced <- test_embeddings$clone(deep = TRUE)
test_embeddings_reduced$embeddings <- test_embeddings_reduced$embeddings[1:5, , ]
test_embeddings_reduced_LD <- test_embeddings_reduced$convert_to_LargeDataSetForTextEmbeddings()

# case=sample(x=seq.int(from = 1,to=nrow(test_embeddings$embeddings)))
test_embeddings_single_case <- test_embeddings$clone(deep = TRUE)
test_embeddings_single_case$embeddings <- test_embeddings_single_case$embeddings[1, , , drop = FALSE]
test_embeddings_single_case_LD <- test_embeddings_single_case$convert_to_LargeDataSetForTextEmbeddings()

# Config
ml_frameworks <- c("pytorch")

rec_list <- list(NULL, c(4), c(4, 3))
rec_type_list <- list("gru", "lstm")
rec_bidirectiona_list <- list(TRUE, FALSE)
hidden_list <- list(NULL, c(4), c(4, 3))
r_encoder_list <- list(0, 1, 2)
attention_list <- list("fourier", "multihead")
pos_embedding_list <- list(TRUE, FALSE)

sc_list <- list(FALSE, TRUE)
pl_list <- list(FALSE, TRUE)

# Load feature extractors
feature_extractor_list <- NULL
feature_extractor_list["pytorch"] <- list(list(
  load_from_disk(paste0(root_path_data, "/feature_extractor_pytorch")),
  NULL
))

if (skip_3_classes == TRUE) {
  max_classes <- 2
} else {
  max_classes <- 3
}

for (framework in ml_frameworks) {
  for (n_classes in 2:max_classes) {
    # Prepare data for different classification types---------------------------
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
      target_levels <- c("neg", "medium", "pos")
    } else {
      target_levels <- c("neg", "pos")
    }
    example_targets <- as.factor(example_data$label)
    names(example_targets) <- example_data$id


    # Start Tests-------------------------------------------------------------------------------
    # Test creation and prediction of the classifier----------------------------
    if (!skip_creation_test) {
      for (feature_extractor in feature_extractor_list[[framework]]) {
        for (rec in rec_list) {
          for (hidden in hidden_list) {
            for (r in r_encoder_list) {
              for (attention in attention_list) {
                for (pos_embedding in pos_embedding_list) {
                  for (rec_type in rec_type_list) {
                    for (rec_bidirectional in rec_bidirectiona_list) {
                      classifier <- NULL
                      gc()
                      classifier <- TEClassifierProtoNet$new()
                      classifier$configure(
                        ml_framework = framework,
                        name = "movie_review_classifier",
                        label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
                        text_embeddings = test_embeddings,
                        feature_extractor = feature_extractor,
                        embedding_dim = 3,
                        target_levels = target_levels,
                        hidden = hidden,
                        rec = rec,
                        rec_type = rec_type,
                        rec_bidirectional = rec_bidirectional,
                        self_attention_heads = 2,
                        add_pos_embedding = pos_embedding,
                        attention_type = attention,
                        encoder_dropout = 0.1,
                        repeat_encoder = r,
                        recurrent_dropout = 0.4
                      )

                      test_that(paste(
                        "no sustainability tracking", framework,
                        "n_classes", n_classes,
                        "features_extractor", !is.null(feature_extractor),
                        "rec", paste(rec, collapse = "_"),
                        "rec_type", rec_type,
                        "rec_bidirectional", rec_bidirectional,
                        "hidden", paste(hidden, collapse = "_"),
                        "encoder", r,
                        "attention", attention,
                        "pos", pos_embedding
                      ), {
                        expect_false(classifier$get_sustainability_data()$sustainability_tracked)
                      })


                      test_that(paste(
                        "predict - basic", framework,
                        "n_classes", n_classes,
                        "features_extractor", !is.null(feature_extractor),
                        "rec", paste(rec, collapse = "_"),
                        "rec_type", rec_type,
                        "rec_bidirectional", rec_bidirectional,
                        "hidden", paste(hidden, collapse = "_"),
                        "encoder", r,
                        "attention", attention,
                        "pos", pos_embedding
                      ), {
                        expect_s3_class(classifier,
                          class = "TEClassifierProtoNet"
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
                        "rec", paste(rec, collapse = "_"),
                        "rec_type", rec_type,
                        "rec_bidirectional", rec_bidirectional,
                        "hidden", paste(hidden, collapse = "_"),
                        "encoder", r,
                        "attention", attention,
                        "pos", pos_embedding
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
                        "rec", paste(rec, collapse = "_"),
                        "rec_type", rec_type,
                        "rec_bidirectional", rec_bidirectional,
                        "hidden", paste(hidden, collapse = "_"),
                        "encoder", r,
                        "attention", attention,
                        "pos", pos_embedding
                      ), {
                        # EmbeddedText
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
                        expect_equal(predictions, predictions_2)

                        # LargeDataSetForTextEmbeddings
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
                        expect_equal(predictions, predictions_2)
                      })

                      test_that(paste(
                        "predict - order invariance", framework,
                        "n_classes", n_classes,
                        "features_extractor", !is.null(feature_extractor),
                        "rec", paste(rec, collapse = "_"),
                        "rec_type", rec_type,
                        "rec_bidirectional", rec_bidirectional,
                        "hidden", paste(hidden, collapse = "_"),
                        "encoder", r,
                        "attention", attention,
                        "pos", pos_embedding
                      ), {
                        embeddings_ET_perm <- test_embeddings_reduced$clone(deep = TRUE)
                        perm <- sample(x = seq.int(from = 1, to = nrow(embeddings_ET_perm$embeddings)), replace = FALSE)
                        embeddings_ET_perm$embeddings <- embeddings_ET_perm$embeddings[perm, , , drop = FALSE]

                        # EmbeddedText
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
                        predictions_Perm <- predictions_Perm[rownames(predictions), ]

                        expect_equal(predictions$expected_category, predictions_Perm$expected_category)

                        # LargeDataSetForTextEmbeddings
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
                        predictions_Perm <- predictions_Perm[rownames(predictions), ]
                        expect_equal(predictions$expected_category, predictions_Perm$expected_category)
                      })

                      test_that(paste(
                        "predict - data source invariance", framework,
                        "n_classes", n_classes,
                        "features_extractor", !is.null(feature_extractor),
                        "rec", paste(rec, collapse = "_"),
                        "rec_type", rec_type,
                        "rec_bidirectional", rec_bidirectional,
                        "hidden", paste(hidden, collapse = "_"),
                        "encoder", r,
                        "attention", attention,
                        "pos", pos_embedding
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
                        i <- sample(seq.int(from = 1, to = nrow(predictions_ET)), size = 1)
                        expect_equal(predictions_ET$expected_category, predictions_LD$expected_category)
                      })
                      gc()
                    }
                  }
                }
              }
            }
          }
        }
      }
    }


    # Test training of the classifier-------------------------------------------
    if (!skip_training_test) {
      for (feature_extractor in feature_extractor_list[[framework]]) {
        for (use_sc in sc_list) {
          for (use_pl in pl_list) {
            # Randomly select a configuration for training
            rec <- rec_list[[sample(x = seq.int(from = 1, to = length(rec_list)), size = 1)]]
            rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
            rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
            hidden <- hidden_list[[sample(x = seq.int(from = 1, to = length(hidden_list)), size = 1)]]
            repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
            attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
            add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

            # Create directory for saving checkpoint for every training
            train_path <- paste0(root_path_results, "/", "train_", generate_id())
            if (dir.exists(train_path) == FALSE) {
              dir.create(train_path)
            }

            classifier <- TEClassifierProtoNet$new()
            classifier$configure(
              ml_framework = framework,
              name = paste0("movie_review_classifier_", "classes_", n_classes),
              label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
              text_embeddings = test_embeddings,
              target_levels = target_levels,
              feature_extractor = feature_extractor,
              embedding_dim = 3,
              hidden = hidden,
              rec = rec,
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
              framework, !is.null(feature_extractor), "training",
              "fe", !is.null(feature_extractor),
              "n_classes", n_classes,
              "sc", use_sc,
              "pl", use_pl,
              "features_extractor", !is.null(feature_extractor),
              "rec", paste(rec, collapse = "_"),
              "rec_type", rec_type,
              "rec_bidirectional", rec_bidirectional,
              "hidden", paste(hidden, collapse = "_"),
              "encoder", repeat_encoder,
              "attention", attention_type,
              "pos", add_pos_embedding
            ), {
              expect_no_error(
                classifier$train(
                  data_embeddings = test_embeddings,
                  data_targets = example_targets,
                  data_folds = 2,
                  use_sc = use_sc,
                  sc_method = "dbsmote",
                  sc_min_k = 1,
                  sc_max_k = 2,
                  use_pl = use_pl,
                  loss_alpha = 0.5,
                  loss_margin = 0.5,
                  pl_max_steps = 2,
                  pl_max = 1.00,
                  pl_anchor = 1.00,
                  pl_min = 0.00,
                  sustain_track = TRUE,
                  sustain_iso_code = "DEU",
                  sustain_region = NULL,
                  sustain_interval = 15,
                  epochs = 2,
                  batch_size = 32,
                  dir_checkpoint = train_path,
                  trace = FALSE,
                  ml_trace = 0
                )
              )
              expect_true(classifier$get_sustainability_data()$sustainability_tracked)
            })
            gc()
          }
        }
      }
    }

    # Method save and load------------------------------------------------------
    for (feature_extractor in feature_extractor_list[[framework]]) {
      test_that(paste(framework, !is.null(feature_extractor), "method save and load"), {
        # Randomly select a configuration for training
        rec <- rec_list[[sample(x = seq.int(from = 1, to = length(rec_list)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        hidden <- hidden_list[[sample(x = seq.int(from = 1, to = length(hidden_list)), size = 1)]]
        repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier <- TEClassifierProtoNet$new()
        classifier$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels,
          feature_extractor = feature_extractor,
          hidden = hidden,
          rec = rec,
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
      })
      gc()
    }

    # Function for loading and saving models-----------------------------------
    for (feature_extractor in feature_extractor_list[[framework]]) {
      test_that(paste(framework, !is.null(feature_extractor), "function save and load"), {
        # Randomly select a configuration for training
        rec <- rec_list[[sample(x = seq.int(from = 1, to = length(rec_list)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        hidden <- hidden_list[[sample(x = seq.int(from = 1, to = length(hidden_list)), size = 1)]]
        repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier <- TEClassifierProtoNet$new()
        classifier$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels,
          feature_extractor = feature_extractor,
          embedding_dim = 3,
          hidden = hidden,
          rec = rec,
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
      })
      gc()
    }

    # Overfitting test----------------------------------------------------------
    if (!skip_overfitting_test) {
      test_that(paste(framework, n_classes, "overfitting test"), {
        # Create directory for saving checkpoint for every training
        train_path <- paste0(root_path_results, "/", "train_", generate_id())
        if (dir.exists(train_path) == FALSE) {
          dir.create(train_path)
        }

        # Randomly select a configuration for training
        rec <- rec_list[[sample(x = seq.int(from = 1, to = length(rec_list)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        hidden <- hidden_list[[sample(x = seq.int(from = 1, to = length(hidden_list)), size = 1)]]
        # repeat_encoder=r_encoder_list[[sample(x=seq.int(from = 1,to=length(r_encoder_list)),size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier_overfitting <- TEClassifierProtoNet$new()
        classifier_overfitting$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels,
          feature_extractor = NULL,
          embedding_dim = 5,
          hidden = hidden,
          rec = rec,
          rec_type = rec_type,
          rec_bidirectional = rec_bidirectional,
          self_attention_heads = 1,
          intermediate_size = NULL,
          attention_type = attention_type,
          add_pos_embedding = add_pos_embedding,
          rec_dropout = 0.1,
          repeat_encoder = 0,
          dense_dropout = 0.4,
          recurrent_dropout = 0.4,
          encoder_dropout = 0.1,
          optimizer = "adam"
        )


        classifier_overfitting$train(
          data_embeddings = test_embeddings,
          data_targets = example_targets,
          data_folds = 2,
          loss_alpha = 0.5,
          loss_margin = 0.5,
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
          epochs = 100,
          batch_size = 32,
          dir_checkpoint = train_path,
          log_dir = train_path,
          trace = FALSE,
          ml_trace = 0
        )

        history <- classifier_overfitting$last_training$history[[1]]$accuracy["train", ]
        expect_gte(object = max(history), expected = 0.90)

        state_log_exists <- file.exists(paste0(train_path, "/aifeducation_state.log"))
        expect_true(state_log_exists)
        if (state_log_exists) {
          log_state <- read.csv(paste0(train_path, "/aifeducation_state.log"))
          expect_equal(nrow(log_state), 3)
          expect_equal(ncol(log_state), 3)
          expect_equal(colnames(log_state), c("value", "total", "message"))
        }

        loss_log_exists <- file.exists(paste0(train_path, "/aifeducation_loss.log"))
        expect_true(loss_log_exists)
        if (loss_log_exists == TRUE) {
          log_loss <- read.csv(paste0(train_path, "/aifeducation_loss.log"), header = FALSE)
          expect_gte(ncol(log_loss), 2)
          expect_gte(nrow(log_loss), 2)
        }
      })
    }

    # Embed----------------------------------------------------------------------
    for (feature_extractor in feature_extractor_list[[framework]]) {
      test_that(paste(framework, !is.null(feature_extractor), "embed"), {
        # Randomly select a configuration for training
        rec <- rec_list[[sample(x = seq.int(from = 1, to = length(rec_list)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        hidden <- hidden_list[[sample(x = seq.int(from = 1, to = length(hidden_list)), size = 1)]]
        repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier <- TEClassifierProtoNet$new()
        classifier$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels,
          feature_extractor = feature_extractor,
          embedding_dim = 3,
          hidden = hidden,
          rec = rec,
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
        embeddings <- classifier$embed(
          embeddings_q = test_embeddings_reduced,
          batch_size = 50
        )

        # check case order invariance
        perm <- sample(x = seq.int(from = 1, to = nrow(test_embeddings_reduced$embeddings)))
        test_embeddings_reduced_perm <- test_embeddings_reduced$clone(deep = TRUE)
        test_embeddings_reduced_perm$embeddings <- test_embeddings_reduced_perm$embeddings[perm, , ]
        embeddings_perm <- classifier$embed(
          embeddings_q = test_embeddings_reduced_perm,
          batch_size = 50
        )
        for (i in 1:nrow(embeddings$embeddings_q)) {
          expect_equal(embeddings$embeddings_q[i, ],
            embeddings_perm$embeddings_q[which(perm == i), ],
            tolerance = 1e-5
          )
        }
      })
      gc()
    }
    # Plot-----------------------------------------------------------------------
    for (feature_extractor in feature_extractor_list[[framework]]) {
      test_that(paste(framework, !is.null(feature_extractor), "plot"), {
        # Randomly select a configuration for training
        rec <- rec_list[[sample(x = seq.int(from = 1, to = length(rec_list)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        hidden <- hidden_list[[sample(x = seq.int(from = 1, to = length(hidden_list)), size = 1)]]
        repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier <- TEClassifierProtoNet$new()
        classifier$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels,
          feature_extractor = feature_extractor,
          embedding_dim = 3,
          hidden = hidden,
          rec = rec,
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
        plot <- classifier$plot_embeddings(
          embeddings_q = test_embeddings_reduced,
          classes_q = example_targets,
          batch_size = 50
        )
        expect_s3_class(plot, "ggplot")
      })
      gc()
    }

    plot_embeddings <- function(embeddings_q, classes_q, batch_size) {
      # Documentation--------------------------------------------------------------
      test_that(paste(framework, n_classes, "descriptions"), {
        # Randomly select a configuration for training
        rec <- rec_list[[sample(x = seq.int(from = 1, to = length(rec_list)), size = 1)]]
        rec_type <- rec_type_list[[sample(x = seq.int(from = 1, to = length(rec_type_list)), size = 1)]]
        rec_bidirectional <- rec_bidirectiona_list[[sample(x = seq.int(from = 1, to = length(rec_bidirectiona_list)), size = 1)]]
        hidden <- hidden_list[[sample(x = seq.int(from = 1, to = length(hidden_list)), size = 1)]]
        repeat_encoder <- r_encoder_list[[sample(x = seq.int(from = 1, to = length(r_encoder_list)), size = 1)]]
        attention_type <- attention_list[[sample(x = seq.int(from = 1, to = length(attention_list)), size = 1)]]
        add_pos_embedding <- pos_embedding_list[[sample(x = seq.int(from = 1, to = length(pos_embedding_list)), size = 1)]]

        classifier <- TEClassifierProtoNet$new()
        classifier$configure(
          ml_framework = framework,
          name = paste0("movie_review_classifier_", "classes_", n_classes),
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = test_embeddings,
          target_levels = target_levels,
          feature_extractor = NULL,
          embedding_dim = 3,
          hidden = hidden,
          rec = rec,
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
}
