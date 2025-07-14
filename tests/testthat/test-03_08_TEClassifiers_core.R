testthat::skip_on_cran()
testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)
# config------------------------------------------------------------------------
object_class_names <- get_TEClassifiers_class_names(super_class = "ClassifiersBasedOnTextEmbeddings")
#object_class_names=c("TEClassifierSequential")
#object_class_names="TEClassifierParallelPrototype"
#object_class_names="TEClassifierSequentialPrototype"
#object_class_names="TEClassifierRegular"

max_samples <- 20
max_samples_CI <- 10

max_samples_training <- 2
class_range <- c(2, 3)

prob_precision=1e-6

# Skip Tests-------------------------------------------------------------------
skip_creation_test <- FALSE
skip_method_save_load <- FALSE
skip_function_save_load <- FALSE
skip_training_test <- FALSE
skip_documentation <- FALSE


# SetUp-------------------------------------------------------------------------
# Set paths
root_path_general_data <- testthat::test_path("test_data_tmp/Embeddings")

create_dir(testthat::test_path("test_artefacts"), FALSE)
root_path_results <- testthat::test_path("test_artefacts/TEClassifiers_core")
create_dir(root_path_results, FALSE)
root_path_feature_extractor <- testthat::test_path("test_data_tmp/classifier/feature_extractor_pytorch")

# SetUp datasets
# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

# Load test data-----------------------------------------------------------------
test_data <- get_test_data_for_classifiers(
  class_range = class_range,
  path_test_embeddings = paste0(root_path_general_data, "/imdb_embeddings")
)

target_data <- test_data$target_data
target_levels <- test_data$target_levels
test_embeddings_large <- test_data$test_embeddings_large
test_embeddings <- test_data$test_embeddings
test_embeddings_reduced <- test_data$test_embeddings_reduced
test_embeddings_reduced_LD <- test_data$test_embeddings_reduced_LD
test_embeddings_single_case <- test_data$test_embeddings_single_case
test_embeddings_single_case_LD <- test_data$test_embeddings_single_case_LD

# Load feature extractors-------------------------------------------------------
#devtools::load_all()
feature_extractor <- NULL
if (file.exists(root_path_feature_extractor)) {
  feature_extractor <- load_from_disk(root_path_feature_extractor)
} else {
  feature_extractor <- NULL
}

#feature_extractor$extract_features_large(data_embeddings = test_embeddings$convert_to_LargeDataSetForTextEmbeddings(),batch_size = 2,trace=TRUE)
#feature_extractor$extract_features_large()

# Tests------------------------------------------------------------------------
for (object_class_name in object_class_names) {
  # Test for different number of classes
  for (n_classes in class_range) {
    for (i in 1:check_adjust_n_samples_on_CI(n_samples_requested=max_samples,
                                             n_CI = max_samples_CI)) {
      # Core Tests of the models-------------------------------------------------
      if (!skip_creation_test) {
        # Create a List of all relevant combinations of arguments and reduce the number
        # to the desired sample size.
        # These are available for all tests.
        test_combination <- generate_args_for_tests(
          object_name = object_class_name,
          method = "configure",
          var_objects = list(
            feature_extractor = feature_extractor
          ),
          necessary_objects = list(
            text_embeddings = test_embeddings,
            target_levels = target_levels[[n_classes]]
          ),
          var_override = list(
            name = NULL,
            label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
            sustain_interval = 30,
            act_fct = "elu",
            feat_size=64,
            intermediate_features=10,
            tf_dense_dim=26,
            tf_parametrizations="None",
            dense_parametrizations="None",
            rec_parametrizations="None",
            conv_parametrizations="None",
            tf_num_heads=2,
            ng_conv_ks_min=2,
            ng_conv_ks_max=3,
            trace = FALSE,
            epochs = 10,
            batch_size = 20,
            ml_trace = 0,
            n_cores = 2,
            data_folds = 2,
            pl_max_steps = 2,
            pl_max = 1,
            pl_anchor = 1,
            pl_min = 0,
            sustain_track = TRUE,
            sustain_iso_code = "DEU",
            data_val_size = 0.25,
            lr_rate = 1e-3,
            dense_size = 5,
            rec_size = 5,
            self_attention_heads = 2,
            intermediate_size = 6,
            lr_warm_up_ratio = 0.01,
            merge_num_heads=2,
            merge_attention_type="multihead"
          )
        )


        classifier <- NULL
        gc()

        # Create test object with a given combination of args
        classifier <- create_object(object_class_name)
        do.call(
          what = classifier$configure,
          args = test_combination
        )


        test_that(paste("Number of Predictions", object_class_name, get_current_args_for_print(test_combination)), {
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

        test_that(paste("predict - single case", object_class_name), {
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

        test_that(paste("predict - randomness", object_class_name, get_current_args_for_print(test_combination)), {
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

        if(!is.null(test_combination$attention)){
          if (test_combination$attention != "fourier") {
            test_that(paste("predict - order invariance", object_class_name, get_current_args_for_print(test_combination)), {
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

              expect_equal(
                predictions[ids, 1:(ncol(predictions) - 1)],
                predictions_Perm[ids, 1:(ncol(predictions_Perm) - 1)],
                tolerance = prob_precision
              )

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

              expect_equal(
                predictions[ids, 1:(ncol(predictions) - 1)],
                predictions_Perm[ids, 1:(ncol(predictions_Perm) - 1)],
                tolerance = prob_precision
              )
            })
          }
        }


        test_that(paste("predict - data source invariance", object_class_name, get_current_args_for_print(test_combination)), {
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

      # Save and load tests-------------------------------------------------------
      if (!skip_method_save_load) {
        test_that(paste("method save and load", object_class_name, get_current_args_for_print(test_combination)), {
          classifier <- create_object(object_class_name)
          do.call(
            what = classifier$configure,
            args = test_combination
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
      }
    }

    # Function for loading and saving models-----------------------------------
    if (!skip_function_save_load) {

      test_that(paste("function save and load", object_class_name, get_current_args_for_print(test_combination)), {
        classifier <- NULL
        gc()
        # Randomly select a configuration for training
        test_combination <- generate_args_for_tests(
          object_name = object_class_name,
          method = "configure",
          var_objects = list(
            feature_extractor = feature_extractor
          ),
          necessary_objects = list(
            text_embeddings = test_embeddings,
            target_levels = target_levels[[n_classes]]
          ),
          var_override = list(
            name = NULL,
            label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
            sustain_interval = 30,
            act_fct = "elu",
            feat_size=64,
            intermediate_features=10,
            tf_dense_dim=26,
            tf_parametrizations="None",
            dense_parametrizations="None",
            rec_parametrizations="None",
            conv_parametrizations="None",
            tf_num_heads=2,
            ng_conv_ks_min=2,
            ng_conv_ks_max=3,
            trace = FALSE,
            epochs = 10,
            batch_size = 20,
            ml_trace = 0,
            n_cores = 2,
            data_folds = 2,
            pl_max_steps = 2,
            pl_max = 1,
            pl_anchor = 1,
            pl_min = 0,
            sustain_track = TRUE,
            sustain_iso_code = "DEU",
            data_val_size = 0.25,
            lr_rate = 1e-3,
            dense_size = 5,
            rec_size = 5,
            self_attention_heads = 2,
            intermediate_size = 6,
            lr_warm_up_ratio = 0.01,
            merge_num_heads=2,
            merge_attention_type="multihead"
          )
        )

        # Create test object with a given combination of args
        classifier <- create_object(object_class_name)
        do.call(
          what = classifier$configure,
          args = test_combination
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

    # Documentation--------------------------------------------------------------
    if (!skip_documentation) {
      test_that(paste("Documentation", object_class_name, get_current_args_for_print(test_combination)), {


        # Randomly select a configuration
        test_combination <- generate_args_for_tests(
          object_name = object_class_name,
          method = "configure",
          var_objects = list(
            feature_extractor = feature_extractor
          ),
          necessary_objects = list(
            text_embeddings = test_embeddings,
            target_levels = target_levels[[n_classes]]
          ),
          var_override = list(
            name = NULL,
            label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
            sustain_interval = 30,
            act_fct = "elu",
            feat_size=64,
            intermediate_features=10,
            tf_dense_dim=26,
            tf_parametrizations="None",
            dense_parametrizations="None",
            rec_parametrizations="None",
            conv_parametrizations="None",
            tf_num_heads=2,
            ng_conv_ks_min=2,
            ng_conv_ks_max=3,
            trace = FALSE,
            epochs = 10,
            batch_size = 20,
            ml_trace = 0,
            n_cores = 2,
            data_folds = 2,
            pl_max_steps = 2,
            pl_max = 1,
            pl_anchor = 1,
            pl_min = 0,
            sustain_track = TRUE,
            sustain_iso_code = "DEU",
            data_val_size = 0.25,
            lr_rate = 1e-3,
            dense_size = 5,
            rec_size = 5,
            self_attention_heads = 2,
            intermediate_size = 6,
            lr_warm_up_ratio = 0.01,
            merge_num_heads=2,
            merge_attention_type="multihead"
          )
        )
        classifier <- NULL
        gc()

        # Create test object with a given combination of args
        classifier <- create_object(object_class_name)
        do.call(
          what = classifier$configure,
          args = test_combination
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



        classifier$set_model_license("test_license")
        expect_equal(
          object = classifier$get_model_license(),
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

    # Test training of the classifier-------------------------------------------
    if (!skip_training_test) {
      # Create combinations for the training configuration
      log_dir <- paste0(root_path_results, "/", generate_id(5))
      create_dir(log_dir, trace = FALSE)

      for (j in 1:max_samples_training) {
        #Config sample
        test_combination <- generate_args_for_tests(
          object_name = object_class_name,
          method = "configure",
          var_objects = list(
            feature_extractor = feature_extractor
          ),
          necessary_objects = list(
            text_embeddings = test_embeddings,
            target_levels = target_levels[[n_classes]]
          ),
          var_override = list(
            name = NULL,
            label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
            sustain_interval = 30,
            act_fct = "elu",
            feat_size=64,
            intermediate_features=10,
            tf_dense_dim=26,
            tf_parametrizations="None",
            dense_parametrizations="None",
            rec_parametrizations="None",
            conv_parametrizations="None",
            tf_num_heads=2,
            ng_conv_ks_min=2,
            ng_conv_ks_max=3,
            trace = FALSE,
            epochs = 10,
            batch_size = 20,
            ml_trace = 0,
            n_cores = 2,
            data_folds = 2,
            pl_max_steps = 2,
            pl_max = 1,
            pl_anchor = 1,
            pl_min = 0,
            sustain_track = TRUE,
            sustain_iso_code = "DEU",
            data_val_size = 0.25,
            lr_rate = 1e-3,
            dense_size = 5,
            rec_size = 5,
            self_attention_heads = 2,
            intermediate_size = 6,
            lr_warm_up_ratio = 0.01,
            merge_num_heads=2,
            merge_attention_type="multihead"
          )
        )

        #traing config sample
        train_args_combinations <- generate_args_for_tests(
          object_name = object_class_name,
          method = "train",
          var_objects = list(),
          necessary_objects = list(
            data_embeddings = test_embeddings,
            data_targets = target_data[[n_classes]]
          ),
          var_override = list(
            name = NULL,
            label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
            sustain_interval = 30,
            act_fct = "elu",
            rec_dropout = 0.1,
            dense_dropout = 0.1,
            encoder_dropout = 0.1,
            trace = FALSE,
            epochs = 20,
            batch_size = 20,
            ml_trace = 0,
            n_cores = 2,
            data_folds = 2,
            use_pl=FALSE,
            pl_max_steps = 2,
            pl_max = 1,
            pl_anchor = 1,
            pl_min = 0,
            use_sc=FALSE,
            sc_min_k = 1,
            sc_max_k = 2,
            sustain_track = TRUE,
            sustain_iso_code = "DEU",
            data_val_size = 0.25,
            lr_rate = 1e-3,
            dense_size = 5,
            rec_size = 5,
            self_attention_heads = 2,
            intermediate_size = 6,
            lr_warm_up_ratio = 0.01,
            log_dir = log_dir
          )
        )

        # Create test object with a given combination of args
        classifier <- NULL
        gc()
        classifier <- create_object(object_class_name)
        do.call(
          what = classifier$configure,
          args = test_combination
        )

        test_that(paste(
          "training", object_class_name,
          get_current_args_for_print(test_combination),
          get_current_args_for_print(train_args_combinations)
        ), {
          expect_no_error(
            do.call(
              what = classifier$train,
              args = train_args_combinations
            )
          )

          expect_true(classifier$get_sustainability_data()$sustainability_tracked)

          state_log_exists <- file.exists(paste0(log_dir, "/aifeducation_state.log"))
          expect_true(state_log_exists)

          if (state_log_exists) {
            log_state <- read.csv(paste0(log_dir, "/aifeducation_state.log"))
            expect_equal(nrow(log_state), 3)
            expect_equal(ncol(log_state), 3)
            expect_equal(colnames(log_state), c("value", "total", "message"))
            unlink(paste0(log_dir, "/aifeducation_state.log"))
          }

          loss_log_exists <- file.exists(paste0(log_dir, "/aifeducation_loss.log"))
          expect_true(loss_log_exists)
          if (loss_log_exists == TRUE) {
            log_loss <- read.csv(paste0(log_dir, "/aifeducation_loss.log"), header = FALSE)
            expect_gte(ncol(log_loss), 2)
            expect_gte(nrow(log_loss), 2)
            unlink(paste0(log_dir, "/aifeducation_loss.log"))
          }
        })
        gc()
      }
    }

    # Clean Directory--------------------------------------------------------------
    if (dir.exists(root_path_results)) {
      unlink(
        x = root_path_results,
        recursive = TRUE
      )
    }
  }
}
