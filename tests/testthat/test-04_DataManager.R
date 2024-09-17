testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

# SetUp-------------------------------------------------------------------------
# root_path_data=testthat::test_path("test_data/DataManager")
# if(dir.exists(testthat::test_path("test_artefacts"))==FALSE){
#  dir.create(testthat::test_path("test_artefacts"))
# }
# root_path_results=testthat::test_path("test_artefacts/DataManager")
# if(dir.exists(root_path_results)==FALSE){
#  dir.create(root_path_results)
# }

# SetUp datasets
# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

# Load test data
path <- "test_data/classifier/imdb_embeddings.rda"
load(testthat::test_path(path))
current_embeddings <- imdb_embeddings$clone(deep = TRUE)
example_data <- imdb_movie_reviews

n_classes <- 2

example_data$label <- as.character(example_data$label)
example_data$label[c(201:300)] <- NA
if (n_classes > 2) {
  example_data$label[c(201:250)] <- "medium"
}
example_targets <- as.factor(example_data$label)
names(example_targets) <- example_data$id

table(example_targets)
data_targets <- example_targets
data_embeddings <- current_embeddings

# config test
folds <- c(2, 5)
methods <- c("dbsmote", "smote")
datasets$disable_progress_bars()

# Start Tests-------------------------------------------------------------------
for (method in methods) {
  for (fold in folds) {
    test_datamanager <- DataManagerClassifier$new(
      data_embeddings = data_embeddings,
      data_targets = data_targets,
      folds = fold,
      val_size = 0.25,
      class_levels = levels(data_targets),
      one_hot_encoding = TRUE,
      add_matrix_map = TRUE,
      sc_methods = method,
      sc_min_k = 1,
      sc_max_k = 10,
      trace = FALSE
    )

    for (i in 1:(test_datamanager$get_n_folds() + 1)) {
      sample <- test_datamanager$get_samples()[[i]]
      #-----------------------------------------------------------------------------
      test_that(paste("DataManager - Valid Splits", "Fold:", i), {
        # Test if no case is missing
        expect_equal(length(sample$train) + length(sample$val) + length(sample$test), length(na.omit(example_targets)))

        # Test if the splits are disjunctive
        expect_equal(length(intersect(sample$train, sample$val)), 0)
        expect_equal(length(intersect(sample$train, sample$test)), 0)
        expect_equal(length(intersect(sample$val, sample$test)), 0)
        gc()

        # Test if every class is part of a split
        expect_true(length(table(test_datamanager$datasets$data_labeled[sample$train]["labels"])) == n_classes)
        expect_true(length(table(test_datamanager$datasets$data_labeled[sample$val]["labels"])) == n_classes)
        if (i <= test_datamanager$get_n_folds()) {
          expect_true(length(table(test_datamanager$datasets$data_labeled[sample$test]["labels"])) == n_classes)
        }
        gc()

        # Test if the splits have the minimal absolute frequency
        expect_true(min(table(test_datamanager$datasets$data_labeled[sample$train]["labels"])) > 2)
        expect_true(min(table(test_datamanager$datasets$data_labeled[sample$val]["labels"])) > 1)
        if (i <= test_datamanager$get_n_folds()) {
          expect_true(min(table(test_datamanager$datasets$data_labeled[sample$test]["labels"])) > 1)
        }
        gc()

        # Test if the ratio of the labels is correct (stratified sample)
        expect_identical(
          ignore_attr = TRUE,
          table(test_datamanager$datasets$data_labeled[sample$train]["labels"]) /
            sum(table(test_datamanager$datasets$data_labeled[sample$train]["labels"])),
          table(example_targets) / sum(table(example_targets))
        )
        gc()
        expect_identical(
          ignore_attr = TRUE,
          table(test_datamanager$datasets$data_labeled[sample$val]["labels"]) /
            sum(table(test_datamanager$datasets$data_labeled[sample$val]["labels"])),
          table(example_targets) / sum(table(example_targets))
        )
        gc()
        if (i <= test_datamanager$get_n_folds()) {
          expect_identical(
            ignore_attr = TRUE,
            table(test_datamanager$datasets$data_labeled[sample$test]["labels"]) /
              sum(table(test_datamanager$datasets$data_labeled[sample$test]["labels"])),
            table(example_targets) / sum(table(example_targets))
          )
        }
        gc()
      })

      #----------------------------------------------------------------------------
      test_that(paste("DataManager - Synthetic Cases", method, "Fold:", i), {
        test_datamanager$set_state(iteration = i, step = NULL)
        test_datamanager$create_synthetic(trace = FALSE, inc_pseudo_data = FALSE)
        if (!is.null(test_datamanager$datasets$data_labeled_synthetic)) {
          synthetic_cases_per_seq <- table(
            test_datamanager$datasets$data_labeled_synthetic["length"],
            test_datamanager$datasets$data_labeled_synthetic["labels"]
          )
          original_cases_per_seq <- table(
            test_datamanager$get_dataset()["length"],
            test_datamanager$get_dataset()["labels"]
          )
          for (r in intersect(rownames(original_cases_per_seq), rownames(synthetic_cases_per_seq))) {
            for (c in intersect(colnames(original_cases_per_seq), colnames(synthetic_cases_per_seq))) {
              if (original_cases_per_seq[r, c] > 3) {
                expect_equal(
                  object = original_cases_per_seq[r, c] + synthetic_cases_per_seq[r, c],
                  expected = max(original_cases_per_seq[r, ]),
                  tolerance = 1
                )
              }
            }
          }
        }
      })
      gc()
      #----------------------------------------------------------------------------
      test_that(paste("DataManager - Pseudo Data", "Fold:", i), {
        test_datamanager$add_replace_pseudo_data(
          inputs = data_embeddings$embeddings[1:10, , , drop = FALSE],
          labels = example_targets[1:10]
        )

        expect_equal(
          object = length(test_datamanager$datasets$data_labeled_pseudo),
          expected = 10
        )
      })
      gc()
      #----------------------------------------------------------------------------
      test_that(paste("DataManager - get_dataset()", "Fold:", i), {
        data_test <- test_datamanager$get_dataset(
          inc_labeled = TRUE,
          inc_unlabeled = FALSE,
          inc_synthetic = TRUE,
          inc_pseudo_data = FALSE
        )
        number_of_cases <- sum(table(data_test["length"]))
        true_number_of_cases <- length(test_datamanager$samples[[i]]$train) +
          length(test_datamanager$datasets$data_labeled_synthetic)
        expect_equal(number_of_cases, true_number_of_cases)

        data_test <- test_datamanager$get_dataset(
          inc_labeled = TRUE,
          inc_unlabeled = FALSE,
          inc_synthetic = TRUE,
          inc_pseudo_data = TRUE
        )
        number_of_cases <- sum(table(data_test["length"]))
        true_number_of_cases <- length(test_datamanager$samples[[i]]$train) +
          length(test_datamanager$datasets$data_labeled_synthetic) +
          length(test_datamanager$datasets$data_labeled_pseudo)
        expect_equal(number_of_cases, true_number_of_cases)

        data_test <- test_datamanager$get_dataset(
          inc_labeled = FALSE,
          inc_unlabeled = FALSE,
          inc_synthetic = TRUE,
          inc_pseudo_data = TRUE
        )
        if (!is.null(data_test)) {
          number_of_cases <- sum(table(data_test["length"]))
          true_number_of_cases <- length(test_datamanager$datasets$data_labeled_synthetic) +
            length(test_datamanager$datasets$data_labeled_pseudo)
          expect_equal(number_of_cases, true_number_of_cases)

          data_test <- test_datamanager$get_dataset(
            inc_labeled = FALSE,
            inc_unlabeled = FALSE,
            inc_synthetic = TRUE,
            inc_pseudo_data = FALSE
          )
          number_of_cases <- sum(table(data_test["length"]))
          true_number_of_cases <- length(test_datamanager$datasets$data_labeled_synthetic)
          expect_equal(number_of_cases, true_number_of_cases)
        } else {
          expect_equal(
            object = test_datamanager$datasets$data_labeled_synthetic,
            expected = NULL
          )
        }


        data_test <- test_datamanager$get_dataset(
          inc_labeled = FALSE,
          inc_unlabeled = FALSE,
          inc_synthetic = FALSE,
          inc_pseudo_data = TRUE
        )
        number_of_cases <- sum(table(data_test["length"]))
        true_number_of_cases <- length(test_datamanager$datasets$data_labeled_pseudo)
        expect_equal(number_of_cases, true_number_of_cases)

        data_test <- test_datamanager$get_dataset(
          inc_labeled = FALSE,
          inc_unlabeled = TRUE,
          inc_synthetic = FALSE,
          inc_pseudo_data = FALSE
        )
        number_of_cases <- sum(table(data_test["length"]))
        true_number_of_cases <- length(test_datamanager$datasets$data_unlabeled)
        expect_equal(number_of_cases, true_number_of_cases)
      })
      gc()
    }
  }
}
