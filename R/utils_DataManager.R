# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#-----------------------------------------------------------------------------
#' @title Create synthetic cases for balancing training data
#' @description This function creates synthetic cases for balancing the training with an object of the class
#'   [TextEmbeddingClassifierNeuralNet].
#'
#' @param matrix_form Named `matrix` containing the text embeddings in a matrix form.
#' @param target Named `factor` containing the labels of the corresponding embeddings.
#' @param times `int` for the number of sequences/times.
#' @param features `int` for the number of features within each sequence.
#' @param sequence_length `int` Length of the text embedding sequences.
#' @param method `vector` containing strings of the requested methods for generating new cases. Currently "smote",
#'   "dbsmote", and "adas" from the package smotefamily are available.
#' @param min_k `int` The minimal number of nearest neighbors during sampling process.
#' @param max_k `int` The maximum number of nearest neighbors during sampling process.
#' @return `list` with the following components:
#'   * `syntetic_embeddings`: Named `data.frame` containing the text embeddings of the synthetic cases.
#'   * `syntetic_targets`: Named `factor` containing the labels of the corresponding synthetic cases.
#'   * `n_syntetic_units`: `table` showing the number of synthetic cases for every label/category.
#'
#' @family data_management_utils
#'
#' @export
#' @import foreach
#' @import doParallel
get_synthetic_cases_from_matrix <- function(matrix_form,
                                            times,
                                            features,
                                            target,
                                            sequence_length,
                                            method = c("smote"),
                                            min_k = 1,
                                            max_k = 6) {
  # get possible seq lengthes in order to group the cases by sequence length
  seq_length_categories <- as.numeric(names(table(sequence_length)))

  index <- 1
  input <- NULL

  # Create tasks for every group of sequence lengths
  for (current_seq_length in seq_length_categories) {
    condition <- (sequence_length == current_seq_length)
    idx <- which(condition)
    cat_freq <- table(target[idx])
    categories <- names(cat_freq)
    max_freq <- max(cat_freq)

    for (cat in categories) {
      # Check k and adjust if necessary
      n_neighbors <- cat_freq[cat] - 1

      if (n_neighbors <= max_k) {
        max_k_final <- n_neighbors
        if (min_k > max_k_final) {
          min_k_final <- max_k_final
        } else {
          min_k_final <- min_k
        }
      } else {
        max_k_final <- max_k
        min_k_final <- min_k
      }

      # Check k and adjust according to the difference to the major category
      required_cases <- max_freq - cat_freq[cat]
      n_k <- length(min_k_final:max_k_final)
      if (required_cases < n_k) {
        difference <- n_k - required_cases
        min_k_final <- min_k_final + difference
      }

      if (cat_freq[cat] < max_freq && min_k > 0 && cat_freq[cat] > 3) {
        for (m in seq_len(length(method))) {
          if (method[m] != "dbsmote") {
            for (k in min_k_final:max_k_final) {
              input[[index]] <- list(
                cat = cat,
                required_cases = required_cases,
                k = k,
                method = method[m],
                selected_cases = idx,
                chunks = current_seq_length,
                k_s = length(min_k_final:max_k_final),
                max_k = max_k_final
              )
              index <- index + 1
            }
          } else {
            input[[index]] <- list(
              cat = cat,
              required_cases = required_cases,
              k = 0,
              method = method[m],
              selected_cases = idx,
              chunks = current_seq_length,
              k_s = 1,
              max_k = 0
            )
            index <- index + 1
          }
        }
      }
    }
  }

  result_list <- foreach::foreach(
    index = seq_len(length(input)),
    .export = "create_synthetic_units_from_matrix",
    .errorhandling = "pass"
  ) %dopar% {
    # index=1
    create_synthetic_units_from_matrix(
      matrix_form = matrix_form[
        input[[index]]$selected_cases,
        c(1:(input[[index]]$chunks * features))
      ],
      target = target[input[[index]]$selected_cases],
      required_cases = input[[index]]$required_cases,
      k = input[[index]]$k,
      method = input[[index]]$method,
      cat = input[[index]]$cat,
      k_s = input[[index]]$k_s,
      max_k = input[[index]]$max_k
    )
  }

  # get number of synthetic cases
  n_syn_cases <- 0
  for (i in seq_len(length(result_list))) {
    if (is.null(result_list[[i]]$syntetic_embeddings) == FALSE) {
      n_syn_cases <- n_syn_cases + nrow(result_list[[i]]$syntetic_embeddings)
    }
  }

  syntetic_embeddings <- matrix(
    data = 0,
    nrow = n_syn_cases,
    ncol = times * features
  )
  colnames(syntetic_embeddings) <- colnames(matrix_form)
  syntetic_embeddings <- as.data.frame(syntetic_embeddings)
  syntetic_targets <- NULL

  n_row <- 0
  names_vector <- NULL
  for (i in seq_len(length(result_list))) {
    if (is.null(result_list[[i]]$syntetic_embeddings) == FALSE) {
      syntetic_embeddings[
        (n_row + 1):(n_row + nrow(result_list[[i]]$syntetic_embeddings)),
        c(1:ncol(result_list[[i]]$syntetic_embeddings))
      ] <- result_list[[i]]$syntetic_embeddings[, c(1:ncol(result_list[[i]]$syntetic_embeddings))]
      syntetic_targets <- append(syntetic_targets, values = result_list[[i]]$syntetic_targets)
      n_row <- n_row + nrow(result_list[[i]]$syntetic_embeddings)
      names_vector <- append(
        x = names_vector,
        values = rownames(result_list[[i]]$syntetic_embeddings)
      )
    }
  }

  # Transform matrix back to array
  syntetic_embeddings <- matrix_to_array_c(
    matrix = as.matrix(syntetic_embeddings),
    times = times,
    features = features
  )
  rownames(syntetic_embeddings) <- names_vector
  # dimnames(syntetic_embeddings)[3]<-feature_names

  n_syntetic_units <- table(syntetic_targets)

  results <- NULL
  results["syntetic_embeddings"] <- list(syntetic_embeddings)
  results["syntetic_targets"] <- list(syntetic_targets)
  results["n_syntetic_units"] <- list(n_syntetic_units)

  return(results)
}

#---------------------------------------------
#' @title Create synthetic units
#' @description Function for creating synthetic cases in order to balance the data for training with
#'   [TextEmbeddingClassifierNeuralNet]. This is an auxiliary function for use with [get_synthetic_cases_from_matrix] to allow
#'   parallel computations.
#'
#' @param matrix_form Named `matrix` containing the text embeddings in matrix form. In most cases this object is taken
#'   from [EmbeddedText]$embeddings.
#' @param target Named `factor` containing the labels/categories of the corresponding cases.
#' @param required_cases `int` Number of cases necessary to fill the gab between the frequency of the class under
#'   investigation and the major class.
#' @param k `int` The number of nearest neighbors during sampling process.
#' @param max_k `int` The maximum number of nearest neighbors during sampling process.
#' @param k_s `int` Number of ks in the complete generation process.
#' @param method `vector` containing strings of the requested methods for generating new cases. Currently
#'   "smote","dbsmote", and "adas" from the package smotefamily are available.
#' @param cat `string` The category for which new cases should be created.
#' @return Returns a `list` which contains the text embeddings of the new synthetic cases as a named `data.frame` and
#'   their labels as a named `factor`.
#'
#' @family data_management_utils
#'
#' @export
create_synthetic_units_from_matrix <- function(matrix_form,
                                               target,
                                               required_cases,
                                               k,
                                               method,
                                               cat,
                                               k_s,
                                               max_k) {
  # Transform to a binary problem
  tmp_target <- (target == cat)

  n_minor <- sum(tmp_target)

  # Calculate n of all other cases
  n_major <- sum(!tmp_target)

  # Adept the number of cases for every k in the loop
  min_k <- max_k - k_s + 1

  if (k < (min_k + max_k) / 2) {
    requested_number_cases <- max(1, floor(required_cases / k_s))
  } else {
    requested_number_cases <- ceiling(required_cases / k_s)
  }

  dup_size <- n_major / n_minor

  syn_data <- NULL
  if (method == "smote") {
    syn_data <- try(
      smotefamily::SMOTE(
        X = as.data.frame(matrix_form),
        target = tmp_target,
        K = k,
        dup_size = dup_size
      ),
      silent = TRUE
    )
  } else if (method == "adas") {
    syn_data <- try(
      smotefamily::ADAS(
        X = as.data.frame(matrix_form),
        target = tmp_target,
        K = k
      ),
      silent = TRUE
    )
  } else if (method == "dbsmote") {
    syn_data <- try(
      smotefamily::DBSMOTE(
        X = as.data.frame(matrix_form),
        target = tmp_target,
        dupSize = dup_size,
        MinPts = NULL,
        eps = NULL
      ),
      silent = TRUE
    )
  }

  if (inherits(x = syn_data, what = "try-error") == FALSE && (is.null(syn_data) == FALSE || nrow(syn_data$syn_data) > 0)) {
    n_cols_embedding <- ncol(matrix_form)
    tmp_data <- syn_data$syn_data[1:requested_number_cases, -ncol(syn_data$syn_data)]
    rownames(tmp_data) <- paste0(
      method, "_", cat, "_", k, "_", n_cols_embedding, "_",
      seq(from = 1, to = nrow(tmp_data), by = 1)
    )
    tmp_data <- as.data.frame(tmp_data)
    tmp_target <- rep(cat, times = nrow(tmp_data))
    names(tmp_target) <- rownames(tmp_data)

    results <- list(
      syntetic_embeddings = tmp_data,
      syntetic_targets = tmp_target
    )
  } else {
    results <- list(
      syntetic_embeddings = NULL,
      syntetic_targets = NULL
    )
  }

  return(results)
}

#------------------------------------------------------------------------------
#' @title Function for splitting data into a train and validation sample
#' @description This function creates a train and validation sample based on stratified random sampling. The relative
#'   frequencies of each category in the train and validation sample equal the relative frequencies of the initial data
#'   (proportional stratified sampling).
#'
#' @param embedding Object of class [EmbeddedText].
#' @param target Named `factor` containing the labels of every case.
#' @param val_size `double` Ratio between 0 and 1 indicating the relative frequency of cases which should be used as
#'   validation sample.
#' @return Returns a `list` with the following components:
#'   * `target_train`: Named `factor` containing the labels of the training sample.
#'   * `embeddings_train`: Object of class [EmbeddedText] containing the text embeddings for the training sample.
#'   * `target_test`: Named `factor` containing the labels of the validation sample.
#'   * `embeddings_test`: Object of class [EmbeddedText] containing the text embeddings for the validation sample.
#'
#' @family data_management_utils
#' @keywords internal
#' @noRd
get_train_test_split <- function(embedding = NULL,
                                 target,
                                 val_size) {
  categories <- names(table(target))
  val_sampe <- NULL
  for (cat in categories) {
    tmp <- subset(target, target == cat)
    val_sampe[cat] <- list(
      sample(names(tmp), size = max(1, length(tmp) * val_size))
    )
  }
  val_data <- target[unlist(val_sampe)]
  train_data <- target[setdiff(names(target), names(val_data))]

  if (is.null(embedding) == FALSE) {
    val_embeddings <- embedding$clone(deep = TRUE)
    val_embeddings$embeddings <- val_embeddings$embeddings[names(val_data), ]
    val_embeddings$embeddings <- na.omit(val_embeddings$embeddings)
    train_embeddings <- embedding$clone(deep = TRUE)
    train_embeddings$embeddings <- train_embeddings$embeddings[names(train_data), ]
    train_embeddings$embeddings <- na.omit(train_embeddings$embeddings)

    results <- list(
      target_train = train_data,
      embeddings_train = train_embeddings,
      target_test = val_data,
      embeddings_test = val_embeddings
    )
  } else {
    results <- list(
      target_train = train_data,
      embeddings_train = NA,
      target_test = val_data,
      embeddings_test = NA
    )
  }

  return(results)
}

#-----------------------------------------------------------------------------
#' @title Create cross-validation samples
#' @description Function creates cross-validation samples and ensures that the relative frequency for every
#'   category/label within a fold equals the relative frequency of the category/label within the initial data.
#'
#' @param target Named `factor` containing the relevant labels/categories. Missing cases should be declared with `NA`.
#' @param k_folds `int` number of folds.
#'
#' @return Return a `list` with the following components:
#'   * `val_sample`: `vector` of `strings` containing the names of cases of the validation sample.
#'   * `train_sample`: `vector` of `strings` containing the names of cases of the train sample.
#'   * `n_folds`: int` Number of realized folds.
#'   * `unlabeled_cases`: `vector` of `strings` containing the names of the unlabeled cases.
#'
#' @note The parameter `target` allows cases with missing categories/labels. These should be declared with `NA`. All
#'   these cases are ignored for creating the different folds. Their names are saved within the component
#'   `unlabeled_cases`. These cases can be used for Pseudo Labeling.
#' @note the function checks the absolute frequencies of every category/label. If the absolute frequency is not
#'   sufficient to ensure at least four cases in every fold, the number of folds is adjusted. In these cases, a warning
#'   is printed to the console. At least four cases per fold are necessary to ensure that the training of
#'   [TextEmbeddingClassifierNeuralNet] works well with all options turned on.
#' @family data_management_utils
#' @keywords internal
#' @noRd
get_folds <- function(target,
                      k_folds) {
  sample_target <- na.omit(target)
  freq_cat <- table(sample_target)
  categories <- names(freq_cat)
  min_freq <- min(freq_cat)

  if (min_freq / k_folds < 1) {
    fin_k_folds <- min_freq
    warning(paste("Frequency of the smallest category/label is not sufficent to ensure
                  at least 1 cases per fold. Adjusting number of folds from ", k_folds, "to", fin_k_folds, "."))
    if (fin_k_folds == 0) {
      stop("Frequency of the smallest category/label is to low. Please check your data.
           Consider to remove all categories/labels with a very low absolute frequency.")
    }
  } else {
    fin_k_folds <- k_folds
  }

  final_assignments <- NULL
  for (cat in categories) {
    condition <- (sample_target == cat)
    focused_targets <- subset(
      x = sample_target,
      subset = condition
    )
    n_cases <- length(focused_targets)

    cases_per_fold <- vector(length = fin_k_folds)
    cases_per_fold[] <- ceiling(n_cases / fin_k_folds)

    delta <- sum(cases_per_fold) - n_cases
    if (delta > 0) {
      for (i in 1:delta) {
        cases_per_fold[1 + (i - 1) %% fin_k_folds] <- cases_per_fold[1 + (i - 1) %% fin_k_folds] - 1
      }
    }

    possible_assignments <- NULL
    for (i in seq_len(length(cases_per_fold))) {
      possible_assignments <- append(
        x = possible_assignments,
        values = rep.int(
          x = i,
          times = cases_per_fold[i]
        )
      )
    }

    assignments <- sample(
      x = possible_assignments,
      size = length(possible_assignments),
      replace = FALSE
    )
    names(assignments) <- names(focused_targets)
    final_assignments <- append(
      x = final_assignments,
      values = assignments
    )
  }

  val_sample <- NULL
  for (i in 1:fin_k_folds) {
    condition <- (final_assignments == i)
    val_sample[i] <- list(names(subset(
      x = final_assignments,
      subset = condition
    )))
  }

  train_sample <- NULL
  for (i in 1:fin_k_folds) {
    train_sample[i] <- list(setdiff(x = names(sample_target), y = val_sample[[i]]))
  }

  unlabeled_cases <- setdiff(x = names(target), y = c(val_sample[[1]], train_sample[[1]]))

  results <- list(
    val_sample = val_sample,
    train_sample = train_sample,
    n_folds = fin_k_folds,
    unlabeled_cases = unlabeled_cases
  )
  return(results)
}

#-------------------------------------------------------------------------------
#' @title Create a stratified random sample
#' @description This function creates a stratified random sample.The difference to [get_train_test_split] is that this
#'   function does not require text embeddings and does not split the text embeddings into a train and validation
#'   sample.
#'
#' @param targets Named `vector` containing the labels/categories for each case.
#' @param val_size `double` Value between 0 and 1 indicating how many cases of each label/category should be part of the
#'   validation sample.
#' @return `list` which contains the names of the cases belonging to the train sample and to the validation sample.
#' @family data_management_utils
#' @keywords internal
#' @noRd
get_stratified_train_test_split <- function(targets, val_size = 0.25) {
  test_sample <- NULL
  categories <- names(table(targets))

  for (cat in categories) {
    condition <- (targets == cat)
    tmp <- names(subset(
      x = targets,
      subset = condition
    ))
    test_sample[cat] <- list(
      sample(tmp, size = max(1, length(tmp) * val_size))
    )
  }
  test_sample <- unlist(test_sample, use.names = FALSE)
  train_sample <- setdiff(names(targets), test_sample)

  results <- list(
    test_sample = test_sample,
    train_sample = train_sample
  )
  return(results)
}

#------------------------------------------------------------------------------
#' @title Get the number of chunks/sequences for each case
#' @description Function for calculating the number of chunks/sequences for every case.
#'
#' @param text_embeddings `data.frame` or `array` containing the text embeddings.
#' @param features `int` Number of features within each sequence.
#' @param times `int` Number of sequences.
#' @return Named`vector` of integers representing the number of chunks/sequences for every case.
#'
#' @family data_management_utils
#'
#' @export
get_n_chunks <- function(text_embeddings, features, times) {
  n_chunks <- vector(length = nrow(text_embeddings))
  n_chunks[] <- 0

  if (length(dim(text_embeddings)) == 2) {
    for (i in 1:times) {
      window <- c(1:features) + (i - 1) * features
      sub_matrix <- text_embeddings[, window, drop = FALSE]
      tmp_sums <- rowSums(abs(sub_matrix))
      n_chunks <- n_chunks + as.numeric(!tmp_sums == 0)
    }
  } else if (length(dim(text_embeddings)) == 3) {
    for (i in 1:times) {
      sub_matrix <- text_embeddings[, i, , drop = FALSE]
      tmp_sums <- rowSums(abs(sub_matrix))
      n_chunks <- n_chunks + as.numeric(!tmp_sums == 0)
    }
  } else {
    stop("Dimensionality of text_embeddings must be 2 (matrix) or 3 (array).")
  }


  names(n_chunks) <- rownames(text_embeddings)
  return(n_chunks)
}
