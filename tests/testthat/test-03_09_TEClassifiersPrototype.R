testthat::skip_on_cran()
testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

# config------------------------------------------------------------------------
object_class_names <- get_TEClassifiers_class_names(super_class = "TEClassifiersBasedOnProtoNet")
#Do not use these test for the old ProtoNet Classifier
object_class_names<-setdiff(x=object_class_names,y="TEClassifierProtoNet")
max_samples <- 20
max_samples_CI <- 10

max_samples_training <- 2
class_range <- c(2, 3)

# SetUp-------------------------------------------------------------------------
# Set paths
root_path_general_data <- testthat::test_path("test_data/Embeddings")
create_dir(testthat::test_path("test_artefacts"), FALSE)
root_path_results <- testthat::test_path("test_artefacts/TeClassifierProtoNet")
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
feature_extractor <- NULL

if (file.exists(root_path_feature_extractor)) {
  feature_extractor <- load_from_disk(root_path_feature_extractor)
} else {
  feature_extractor <- NULL
}

for (object_class_name in object_class_names) {
  for (n_classes in class_range) {

    for (i in 1:check_adjust_n_samples_on_CI(n_samples_requested=max_samples,
                                             n_CI = max_samples_CI)) {
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
      #Predict with sample cases-------------------------------------------------
      test_that(paste("Number of Predictions", object_class_name, get_current_args_for_print(test_combination)), {
        predictions <- classifier$predict_with_samples(
          newdata = test_embeddings,
          embeddings_s = test_embeddings_reduced,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(
          object = length(predictions$expected_category),
          expected = nrow(test_embeddings$embeddings)
        )
      })

      test_that(paste(" - single case", object_class_name), {
        prediction <- classifier$predict_with_samples(
          newdata = test_embeddings_single_case,
          embeddings_s = test_embeddings_reduced,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(
          object = nrow(prediction),
          expected = 1
        )

        prediction_LD <- classifier$predict_with_samples(
          newdata = test_embeddings_single_case_LD,
          embeddings_s = test_embeddings_reduced_LD,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(
          object = nrow(prediction_LD),
          expected = 1
        )
      })

      test_that(paste(" - randomness", object_class_name, get_current_args_for_print(test_combination)), {
        # EmbeddedText
        predictions <- NULL
        predictions_2 <- NULL
        predictions <- classifier$predict_with_samples(
          newdata = test_embeddings_reduced,
          embeddings_s = test_embeddings_reduced,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 2,
          ml_trace = 0
        )
        predictions_2 <- classifier$predict_with_samples(
          newdata = test_embeddings_reduced,
          embeddings_s = test_embeddings_reduced,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(predictions[, 1:(ncol(predictions) - 1)], predictions_2[, 1:(ncol(predictions_2) - 1)],
                     tolerance = 1e-6
        )

        # LargeDataSetForTextEmbeddings
        predictions <- NULL
        predictions_2 <- NULL
        predictions <- classifier$predict_with_samples(
          newdata = test_embeddings_reduced_LD,
          embeddings_s = test_embeddings_reduced_LD,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 2,
          ml_trace = 0
        )
        predictions_2 <- classifier$predict_with_samples(
          newdata = test_embeddings_reduced_LD,
          embeddings_s = test_embeddings_reduced_LD,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 2,
          ml_trace = 0
        )
        expect_equal(predictions[, 1:(ncol(predictions) - 1)], predictions_2[, 1:(ncol(predictions_2) - 1)],
                     tolerance = 1e-6
        )
      })

      if(!is.null(test_combination$attention)){
        if (test_combination$attention != "fourier") {
          test_that(paste(" - order invariance", object_class_name, get_current_args_for_print(test_combination)), {
            embeddings_ET_perm <- test_embeddings$clone(deep = TRUE)
            perm <- sample(x = seq.int(from = 1, to = nrow(embeddings_ET_perm$embeddings)), replace = FALSE)
            embeddings_ET_perm$embeddings <- embeddings_ET_perm$embeddings[perm, , , drop = FALSE]

            ids <- rownames(test_embeddings_reduced$embeddings)

            # EmbeddedText
            predictions <- NULL
            predictions_Perm <- NULL
            predictions <- classifier$predict_with_samples(
              newdata = test_embeddings,
              embeddings_s = test_embeddings_reduced,
              classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
              batch_size = 50,
              ml_trace = 0
            )
            predictions_Perm <- classifier$predict_with_samples(
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
            predictions <- classifier$predict_with_samples(
              newdata = test_embeddings_LD,
              embeddings_s = test_embeddings_reduced,
              classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
              batch_size = 50,
              ml_trace = 0
            )

            predictions_Perm <- classifier$predict_with_samples(
              newdata = embeddings_ET_perm$convert_to_LargeDataSetForTextEmbeddings(),
              batch_size = 50,
              ml_trace = 0
            )

            expect_equal(
              predictions[ids, 1:(ncol(predictions) - 1)],
              predictions_Perm[ids, 1:(ncol(predictions_Perm) - 1)],
              embeddings_s = test_embeddings_reduced,
              classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
              tolerance = prob_precision
            )
          })
        }
      }

      # Embed----------------------------------------------------------------------
      test_that(paste("embed without sample cases", object_class_name, get_current_args_for_print(test_combination)), {
        # Predictions
        embeddings <- classifier$embed(
          embeddings_q = test_embeddings_reduced,
          embeddings_s = NULL,
          classes_s = NULL,
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
        for (j in seq_len(nrow(embeddings$embeddings_q))) {
          expect_equal(embeddings$embeddings_q[j, ],
            embeddings_perm$embeddings_q[which(perm == j), ],
            tolerance = 1e-5
          )
        }
      })
      gc()

      test_that(paste("embed with sample cases", object_class_name, get_current_args_for_print(test_combination)), {
        # Predictions
        embeddings <- classifier$embed(
          embeddings_q = test_embeddings,
          embeddings_s = test_embeddings_reduced,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 50
        )

        # check case order invariance
        perm <- sample(x = seq.int(from = 1, to = nrow(test_embeddings$embeddings)))
        test_embeddings_perm <- test_embeddings$clone(deep = TRUE)
        test_embeddings_perm$embeddings <- test_embeddings_perm$embeddings[perm, , ]
        embeddings_perm <- classifier$embed(
          embeddings_q = test_embeddings_perm,
          embeddings_s = test_embeddings_reduced,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 50
        )
        for (j in seq_len(nrow(embeddings$embeddings_q))) {
          expect_equal(embeddings$embeddings_q[j, ],
                       embeddings_perm$embeddings_q[which(perm == j), ],
                       tolerance = 1e-5
          )
        }
      })

      test_that(paste("plot without sample cases", object_class_name, get_current_args_for_print(test_combination)), {
        # plot
        classifier <- create_object(object_class_name)
        do.call(
          what = classifier$configure,
          args = test_combination
        )
        plot <- classifier$plot_embeddings(
          embeddings_q = test_embeddings_reduced,
          classes_q = target_data[[n_classes]],
          embeddings_s = NULL,
          classes_s = NULL,
          batch_size = 50,
          inc_margin = FALSE
        )
        expect_s3_class(plot, "ggplot")
      })

      test_that(paste("plot with sample cases", object_class_name, get_current_args_for_print(test_combination)), {
        # plot
        plot <- classifier$plot_embeddings(
          embeddings_q = test_embeddings,
          classes_q = target_data[[n_classes]],
          embeddings_s = test_embeddings_reduced,
          classes_s = target_data[[n_classes]][rownames(test_embeddings_reduced$embeddings)],
          batch_size = 50,
          inc_margin = FALSE
        )
        expect_s3_class(plot, "ggplot")
      })
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
