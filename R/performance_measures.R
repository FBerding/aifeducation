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

#' @title Calculate Cohen's Kappa
#' @description This function calculates different version of Cohen's Kappa.
#' @param rater_one `factor` rating of the first coder.
#' @param rater_two `factor` ratings of the second coder.
#' @return Returns a `list` containing the results for Cohen' Kappa if no weights
#' are applied (`kappa_unweighted`), if weights are applied and the weights increase
#' linear (`kappa_linear`), and if weights are applied and the weights increase quadratic
#' (`kappa_squared`).
#'
#' @references Cohen, J (1968). Weighted kappa: Nominal scale agreement
#' with provision for scaled disagreement or partial credit.
#' Psychological Bulletin, 70(4), 213–220. <doi:10.1037/h0026256>
#' @references Cohen, J (1960). A Coefficient of Agreement for Nominal Scales.
#' Educational and Psychological Measurement, 20(1), 37–46. <doi:10.1177/001316446002000104>
#'
#' @family performance measures
#' @export
cohens_kappa <- function(rater_one, rater_two) {
  check_class(object=rater_one, classes="factor", allow_NULL=FALSE)
  check_class(object=rater_two, classes="factor", allow_NULL=FALSE)
  if (sum(levels(rater_one) == levels(rater_two)) != max(length(levels(rater_one)), length(levels(rater_two)))) {
    stop("Levels for values of rater one and two are not identical.")
  }

  # raters in the columns and units in the rows

  # Total number of cases
  N <- length(rater_one)

  # Cross table
  freq_table <- table(rater_one, rater_two)
  rel_table <- freq_table / N

  freq_rater_one <- table(rater_one)
  freq_rater_two <- table(rater_two)

  # p_0
  probability_observed <- sum(diag(freq_table) / N)

  # p_e
  probability_expected <- sum(freq_rater_one * freq_rater_two / (N * N))


  # Weight matrices and expected_freq table
  weight_matrix_linear <- matrix(
    data = 0,
    nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one)),
    dimnames = list(levels(rater_one), levels(rater_one))
  )
  weight_matrix_squared <- weight_matrix_linear
  expected_freq <- weight_matrix_squared
  for (i in seq_len(length(levels(rater_one)))) {
    for (j in seq_len(length(levels(rater_one)))) {
      weight_matrix_linear[i, j] <- abs(i - j)
      weight_matrix_squared[i, j] <- abs(i - j)^2
      expected_freq[i, j] <- freq_rater_one[i] * freq_rater_two[j] / N^2
    }
  }

  # Calculate Kappa
  kappa_unweighted <- (probability_observed - probability_expected) / (1 - probability_expected)
  kappa_linear <- 1 - sum(rel_table * weight_matrix_linear) / sum(expected_freq * weight_matrix_linear)
  kappa_squared <- 1 - sum(rel_table * weight_matrix_squared) / sum(expected_freq * weight_matrix_squared)

  return(list(
    kappa_unweighted = kappa_unweighted,
    kappa_linear = kappa_linear,
    kappa_squared = kappa_squared
  ))
}

#' @title Calculate Kendall's coefficient of concordance w
#' @description This function calculates Kendall's coefficient of concordance w with and without correction.
#' @param rater_one `factor` rating of the first coder.
#' @param rater_two `factor` ratings of the second coder.
#' @param additional_raters `list` Additional raters with same requirements as `rater_one` and `rater_two`. If
#' there are no additional raters set to `NULL`.
#' @return Returns a `list` containing the results for Kendall's coefficient of concordance w
#' with and without correction.
#'
#' @family performance measures
#' @export
kendalls_w <- function(rater_one, rater_two, additional_raters = NULL) {
  check_class(object=rater_one, classes="factor", allow_NULL=FALSE)
  check_class(object=rater_two, classes="factor", allow_NULL=FALSE)
  check_class(object=additional_raters, classes="list", allow_NULL=TRUE)

  # create list of raters
  raters <- list(rater_one, rater_two)
  raters <- append(raters, additional_raters)

  # Check levels
  for (i in 2:length(raters)) {
    if (sum(levels(raters[[1]]) == levels(raters[[i]])) !=
      max(length(levels(raters[[1]])), length(levels(raters[[i]])))
    ) {
      stop("Levels for values are not identical.")
    }
  }

  # Number of cases
  N <- length(rater_one)

  # Number of raters
  M <- length(raters)

  raw_table <- matrix(data = 0, nrow = N, ncol = length(raters))
  for (i in seq_len(length(raters))) {
    raw_table[, i] <- as.numeric(raters[[i]])
  }

  # Create ranking
  ranking_table <- raw_table
  for (i in seq_len(ncol(raw_table))) {
    ranking_table[, i] <- rank(raw_table[, i], ties.method = "average")
  }

  ranking_sums <- rowSums(ranking_table)
  mean_ranking <- sum(ranking_sums) / N
  deviation <- sum((ranking_sums - mean_ranking)^2)

  # Calculate ties for every rater
  ties_value_per_rater <- vector(length = M)
  for (i in 1:M) {
    ties_raw <- table(ranking_table[, i])
    ties <- ties_raw^3 - ties_raw
    ties_value_per_rater[i] <- sum(ties)
  }

  # Calculate total number of ties
  ties <- sum(ties_value_per_rater)

  # final measures
  kendall_w <- 12 * deviation / (M^2 * (N^3 - N))
  kendall_w_corrected <- 12 * deviation / (M^2 * (N^3 - N) - M * ties)
  return(
    list(
      kendall_w = kendall_w,
      kendall_w_corrected = kendall_w_corrected
    )
  )
}

#' @title Calculate Krippendorff's Alpha
#' @description This function calculates different Krippendorff's Alpha for nominal and ordinal variables.
#' @param rater_one `factor` rating of the first coder.
#' @param rater_two `factor` ratings of the second coder.
#' @param additional_raters `list` Additional raters with same requirements as `rater_one` and `rater_two`. If
#' there are no additional raters set to `NULL`.
#' @return Returns a `list` containing the results for Krippendorff's Alpha for
#' nominal and ordinal data.
#'
#' @references Krippendorff, K. (2019). Content Analysis: An Introduction to
#' Its Methodology (4th Ed.). SAGE
#'
#' @note Missing values are supported.
#'
#' @family performance measures
#' @export
kripp_alpha <- function(rater_one, rater_two, additional_raters = NULL) {
  check_class(object=rater_one, classes="factor", allow_NULL=FALSE)
  check_class(object=rater_two, classes="factor", allow_NULL=FALSE)
  check_class(object=additional_raters, classes="list", allow_NULL=TRUE)

  # create list of raters
  raters <- list(rater_one, rater_two)
  raters <- append(raters, additional_raters)

  # Check levels
  for (i in 2:length(raters)) {
    if (sum(levels(raters[[1]]) == levels(raters[[i]])) !=
      max(length(levels(raters[[1]])), length(levels(raters[[i]])))
    ) {
      stop("Levels for values are not identical.")
    }
  }

  N <- length(rater_one)

  # create canonical form
  # raters in rows, cases in columns
  canonical_form <- matrix(data = 0, nrow = length(raters), ncol = N)
  for (i in seq_len(length(raters))) {
    canonical_form[i, ] <- as.numeric(raters[[i]])
  }
  canonical_form <- replace(x = canonical_form, list = is.na(canonical_form), values = 0)

  # create value unit matrix
  value_unit_matrix <- matrix(data = 0, nrow = length(levels(rater_one)), ncol = N)
  for (i in seq_len(ncol(canonical_form))) {
    value_unit_matrix[, i] <- as.vector(table(factor(
      canonical_form[, i],
      levels = seq(from = 1, to = length(levels(rater_one)))
    )))
  }

  # Create matrix of observed coincidences
  obs_coincidence_matrix <- matrix(
    data = 0, nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one))
  )

  for (i_1 in seq_len(nrow(value_unit_matrix))) {
    for (i_2 in seq_len(nrow(value_unit_matrix))) {
      tmp_sum <- 0
      for (j in seq_len(ncol(value_unit_matrix))) {
        value_1 <- value_unit_matrix[i_1, j]
        value_2 <- value_unit_matrix[i_2, j]
        m_u <- sum(value_unit_matrix[, j])
        if (m_u > 1) {
          if (i_1 == i_2) {
            tmp_sum <- tmp_sum + value_1 * (value_2 - 1) / (m_u - 1)
          } else {
            tmp_sum <- tmp_sum + value_1 * value_2 / (m_u - 1)
          }
        }
      }
      obs_coincidence_matrix[i_1, i_2] <- tmp_sum
    }
  }

  # Create matrix of expected coincidences
  exp_coincidence_matrix <- matrix(
    data = 0, nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one))
  )
  row_sums <- rowSums(obs_coincidence_matrix)
  for (i_1 in seq_len(nrow(obs_coincidence_matrix))) {
    for (i_2 in seq_len(nrow(obs_coincidence_matrix))) {
      if (i_1 == i_2) {
        exp_coincidence_matrix[i_1, i_2] <- row_sums[i_1] * (row_sums[i_2] - 1)
      } else {
        exp_coincidence_matrix[i_1, i_2] <- row_sums[i_1] * row_sums[i_2]
      }
    }
  }
  exp_coincidence_matrix <- exp_coincidence_matrix / (sum(obs_coincidence_matrix) - 1)

  # Create matrix for differences for nominal data
  nominal_metric_matrix <- matrix(
    data = 1, nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one))
  )
  diag(nominal_metric_matrix) <- 0

  # Create matrix for differences for ordinal data
  ordinal_metric_matrix <- matrix(
    data = 0, nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one))
  )
  n_ranks <- rowSums(obs_coincidence_matrix)
  for (i in seq_len(nrow(ordinal_metric_matrix))) {
    for (j in seq_len(nrow(ordinal_metric_matrix))) {
      categories_between <- seq(from = min(i, j), to = max(i, j), by = 1)
      ordinal_metric_matrix[i, j] <- (sum(n_ranks[categories_between]) - (n_ranks[i] + n_ranks[j]) / 2)^2
    }
  }

  # final values
  alpha_nominal <- 1 - sum(obs_coincidence_matrix * nominal_metric_matrix) /
    sum(exp_coincidence_matrix * nominal_metric_matrix)
  alpha_ordinal <- 1 - sum(obs_coincidence_matrix * ordinal_metric_matrix) /
    sum(exp_coincidence_matrix * ordinal_metric_matrix)

  return(
    list(
      alpha_nominal = alpha_nominal,
      alpha_ordinal = alpha_ordinal
    )
  )
}

#' @title Calculate Fleiss' Kappa
#' @description This function calculates Fleiss' Kappa.
#' @param rater_one `factor` rating of the first coder.
#' @param rater_two `factor` ratings of the second coder.
#' @param additional_raters `list` Additional raters with same requirements as `rater_one` and `rater_two`. If
#' there are no additional raters set to `NULL`.
#' @return Returns the value for Fleiss' Kappa.
#'
#' @references Fleiss, J. L. (1971). Measuring nominal scale agreement among
#' many raters. Psychological Bulletin, 76(5), 378–382. <doi:10.1037/h0031619>
#'
#' @family performance measures
#' @export
fleiss_kappa <- function(rater_one, rater_two, additional_raters = NULL) {
  check_class(rater_one, classes="factor", allow_NULL=FALSE)
  check_class(rater_two, classes="factor", allow_NULL=FALSE)
  check_class(additional_raters, classes="list", allow_NULL=TRUE)

  # create list of raters
  raters <- list(rater_one, rater_two)
  raters <- append(raters, additional_raters)

  # Check levels
  for (i in 2:length(raters)) {
    if (sum(levels(raters[[1]]) == levels(raters[[i]])) !=
      max(length(levels(raters[[1]])), length(levels(raters[[i]])))
    ) {
      stop("Levels for values are not identical.")
    }
  }

  N <- length(rater_one)
  k <- length(levels(rater_one))
  n <- length(raters)

  # Create raw matrix
  # cases in the rows and categories in the column
  raw_matrix <- matrix(data = 0, nrow = N, ncol = k)
  for (i in seq_len(length(raters))) {
    raw_matrix <- raw_matrix + to_categorical_c(
      class_vector = (as.numeric(raters[[i]]) - 1),
      n_classes = k
    )
  }

  # calculate probabilities
  p_obs <- colSums(raw_matrix) / (N * n)

  # Agreement
  p_agree <- vector(length = N)
  for (i in 1:N) {
    for (j in 1:k) {
      p_agree[i] <- p_agree[i] + raw_matrix[i, j] * (raw_matrix[i, j] - 1)
    }
  }
  p_agree <- p_agree / (n * (n - 1))

  # Observed Overall Agreement
  p_agree_mean <- mean(p_agree)

  # Expected Overall Agreement
  p_agree_mean_expected <- sum(p_obs * p_obs)

  # Final Kappa
  kappa <- (p_agree_mean - p_agree_mean_expected) / (1 - p_agree_mean_expected)

  return(kappa)
}

#' @title Calculate Gwet's AC1 and AC2
#' @description This function calculates Gwets Agreement Coefficients.
#' @param rater_one `factor` rating of the first coder.
#' @param rater_two `factor` ratings of the second coder.
#' @param additional_raters `list` Additional raters with same requirements as `rater_one` and `rater_two`. If
#' there are no additional raters set to `NULL`.
#' @return Returns a `list` with the following entries
#'  * ac1: Gwet's Agreement Coefficient 1 (AC1) for nominal data which is unweighted.
#'  * ac2_linear: Gwet's Agreement Coefficient 2 (AC2) for ordinal data with linear weights.
#'  * ac2_quadratic: Gwet's Agreement Coefficient 2 (AC2) for ordinal data with quadratic weights.
#'
#' @references Gwet, K. L. (2021). Handbook of inter-rater reliability:
#' The definitive guide to measuring the extent of agreement among raters
#' (Fifth edition, volume 1). AgreeStat Analytics.
#'
#' @note Weights are calculated as described in Gwet (2021).
#' @note Missing values are supported.
#'
#' @family performance measures
#' @export
gwet_ac <- function(rater_one, rater_two, additional_raters = NULL) {
  check_class(object=rater_one, classes="factor", allow_NULL=FALSE)
  check_class(object=rater_two, classes="factor", allow_NULL=FALSE)
  check_class(object=additional_raters, classes="list", allow_NULL=TRUE)

  # create list of raters
  raters <- list(rater_one, rater_two)
  raters <- append(raters, additional_raters)

  # Check levels
  for (i in 2:length(raters)) {
    if (sum(levels(raters[[1]]) == levels(raters[[i]])) !=
      max(length(levels(raters[[1]])), length(levels(raters[[i]])))
    ) {
      stop("Levels for values are not identical.")
    }
  }

  N <- length(rater_one)
  k <- length(levels(rater_one))
  n <- length(raters)

  # Create raw matrix
  # cases in the rows and categories in the column
  raw_matrix <- matrix(data = 0, nrow = N, ncol = k)
  for (i in seq_len(length(raters))) {
    tmp <- to_categorical_c(
      class_vector = (as.numeric(raters[[i]]) - 1),
      n_classes = k
    )
    tmp <- replace(
      x = tmp,
      is.na(raters[[i]]),
      values = 0
    )
    raw_matrix <- raw_matrix + tmp
  }
  row_sums <- rowSums(raw_matrix)

  # Exclude subjects with only one rating
  reduced_raw_matrix <- subset(
    x = raw_matrix,
    subset = (rowSums(raw_matrix) >= 2)
  )
  row_sums_reduced <- rowSums(reduced_raw_matrix)

  # Agreement
  p_a <- 0
  for (i in 1:nrow(reduced_raw_matrix)) {
    for (j in 1:k) {
      p_a <- p_a + (reduced_raw_matrix[i, j] * (reduced_raw_matrix[i, j] - 1)) / (row_sums_reduced[i] * (row_sums_reduced[i] - 1))
    }
  }
  p_a <- p_a / nrow(reduced_raw_matrix)

  # Expected
  p_e <- 0
  for (j in 1:k) {
    pi <- 0
    for (i in 1:nrow(raw_matrix)) {
      pi <- pi + raw_matrix[i, j] / row_sums[i]
    }
    pi <- pi / N
    p_e <- p_e + pi * (1 - pi)
  }
  p_e <- p_e / (k - 1)

  ac1 <- (p_a - p_e) / (1 - p_e)

  # Calculation of ac2
  weights_quadratic <- matrix(data = NA, nrow = k, ncol = k)
  weights_linear <- weights_quadratic
  for (i in 1:k) {
    for (j in 1:k) {
      weights_quadratic[i, j] <- 1 - (i - j)^2 / (k - 1)^2
      weights_linear[i, j] <- 1 - abs((i - j)) / (k - 1)
    }
  }

  weights_list <- list(weights_linear, weights_quadratic)
  ac2_list <- NULL
  for (w in seq_along(weights_list)) {
    weights <- weights_list[[w]]

    # Agreement
    p_a <- 0
    for (i in 1:nrow(reduced_raw_matrix)) {
      for (j in 1:k) {
        weighted_count <- 0
        for (l in 1:k) {
          weighted_count <- weighted_count + weights[j, l] * reduced_raw_matrix[i, l]
        }
        p_a <- p_a + (reduced_raw_matrix[i, j] * (weighted_count - 1)) / (row_sums_reduced[i] * (row_sums_reduced[i] - 1))
      }
    }
    p_a <- p_a / nrow(reduced_raw_matrix)

    # Expected
    p_e <- 0
    for (j in 1:k) {
      pi <- 0
      for (i in 1:nrow(raw_matrix)) {
        pi <- pi + raw_matrix[i, j] / row_sums[i]
      }
      pi <- pi / N
      p_e <- p_e + pi * (1 - pi)
    }
    T_w <- sum(weights)
    p_e <- p_e * (T_w / (k * (k - 1)))

    ac2_list[w] <- list(
      (p_a - p_e) / (1 - p_e)
    )
  }
  return(
    list(
      ac1 = ac1,
      ac2_linear = ac2_list[[1]],
      ac2_quadratic = ac2_list[[2]]
    )
  )
}


#------------------------------------------------------------------------------
#' @title Calculate reliability measures based on content analysis
#' @description This function calculates different reliability measures which are based on the empirical research method
#'   of content analysis.
#'
#' @param true_values `factor` containing the true labels/categories.
#' @param predicted_values `factor` containing the predicted labels/categories.
#' @param return_names_only `bool` If `TRUE` returns only the names of the resulting vector. Use `FALSE` to request
#'   computation of the values.
#' @return If `return_names_only = FALSE` returns a `vector` with the following reliability measures:
#'   * **iota_index**: Iota Index from the Iota Reliability Concept Version 2.
#'   * **min_iota2**: Minimal Iota from Iota Reliability Concept Version 2.
#'   * **avg_iota2**: Average Iota from Iota Reliability Concept Version 2.
#'   * **max_iota2**: Maximum Iota from Iota Reliability Concept Version 2.
#'   * **min_alpha**: Minmal Alpha Reliability from Iota Reliability Concept Version 2.
#'   * **avg_alpha**: Average Alpha Reliability from Iota Reliability Concept Version 2.
#'   * **max_alpha**: Maximum Alpha Reliability from Iota Reliability Concept Version 2.
#'   * **static_iota_index**: Static Iota Index from Iota Reliability Concept Version 2.
#'   * **dynamic_iota_index**: Dynamic Iota Index Iota Reliability Concept Version 2.
#'   * **kalpha_nominal**: Krippendorff's Alpha for nominal variables.
#'   * **kalpha_ordinal**: Krippendorff's Alpha for ordinal variables.
#'   * **kendall**: Kendall's coefficient of concordance W with correction for ties.
#'   * **c_kappa_unweighted**: Cohen's Kappa unweighted.
#'   * **c_kappa_linear**: Weighted Cohen's Kappa with linear increasing weights.
#'   * **c_kappa_squared**: Weighted Cohen's Kappa with quadratic increasing weights.
#'   * **kappa_fleiss**: Fleiss' Kappa for multiple raters without exact estimation.
#'   * **percentage_agreement**: Percentage Agreement.
#'   * **balanced_accuracy**: Average accuracy within each class.
#'   * **gwet_ac1_nominal**: Gwet's Agreement Coefficient 1 (AC1) for nominal data which is unweighted.
#'   * **gwet_ac2_linear**: Gwet's Agreement Coefficient 2 (AC2) for ordinal data with linear weights.
#'   * **gwet_ac2_quadratic**: Gwet's Agreement Coefficient 2 (AC2) for ordinal data with quadratic weights.
#'
#' @return If `return_names_only = TRUE` returns only the names of the vector elements.
#'
#' @family performance measures
#' @export
get_coder_metrics <- function(true_values = NULL,
                              predicted_values = NULL,
                              return_names_only = FALSE) {
  metric_names <- c(
    "iota_index",
    "min_iota2",
    "avg_iota2",
    "max_iota2",
    "min_alpha",
    "avg_alpha",
    "max_alpha",
    "static_iota_index",
    "dynamic_iota_index",
    "kalpha_nominal",
    "kalpha_ordinal",
    "kendall",
    "c_kappa_unweighted",
    "c_kappa_linear",
    "c_kappa_squared",
    "kappa_fleiss",
    "percentage_agreement",
    "balanced_accuracy",
    "gwet_ac1_nominal",
    "gwet_ac2_linear",
    "gwet_ac2_quadratic",
    "avg_precision",
    "avg_recall",
    "avg_f1"
  )
  metric_values <- vector(length = length(metric_names))
  names(metric_values) <- metric_names

  if (return_names_only == TRUE) {
    return(metric_names)
  } else {
    val_res <- iotarelr::check_new_rater(
      true_values = true_values,
      assigned_values = predicted_values,
      free_aem = FALSE
    )
    val_res_free <- iotarelr::check_new_rater(
      true_values = true_values,
      assigned_values = predicted_values,
      free_aem = TRUE
    )

    metric_values["iota_index"] <- val_res$scale_level$iota_index

    metric_values["min_iota2"] <- min(val_res_free$categorical_level$raw_estimates$iota)
    metric_values["avg_iota2"] <- mean(val_res_free$categorical_level$raw_estimates$iota)
    metric_values["max_iota2"] <- max(val_res_free$categorical_level$raw_estimates$iota)

    metric_values["min_alpha"] <- min(val_res_free$categorical_level$raw_estimates$alpha_reliability)
    metric_values["avg_alpha"] <- mean(val_res_free$categorical_level$raw_estimates$alpha_reliability)
    metric_values["max_alpha"] <- max(val_res_free$categorical_level$raw_estimates$alpha_reliability)

    metric_values["static_iota_index"] <- val_res$scale_level$iota_index_d4
    metric_values["dynamic_iota_index"] <- val_res$scale_level$iota_index_dyn2

    # Krippendorff's Alpha
    kripp_alpha <- kripp_alpha(rater_one = true_values, rater_two = predicted_values, additional_raters = NULL)
    metric_values["kalpha_nominal"] <- kripp_alpha$alpha_nominal
    metric_values["kalpha_ordinal"] <- kripp_alpha$alpha_ordinal

    # Kendall
    metric_values["kendall"] <- kendalls_w(
      rater_one = true_values,
      rater_two = predicted_values
    )$kendall_w_corrected

    # Cohens Kappa
    c_kappa <- cohens_kappa(rater_one = true_values, rater_two = predicted_values)
    metric_values["c_kappa_unweighted"] <- c_kappa$kappa_unweighted
    metric_values["c_kappa_linear"] <- c_kappa$kappa_linear
    metric_values["c_kappa_squared"] <- c_kappa$kappa_squared

    metric_values["kappa_fleiss"] <- fleiss_kappa(
      rater_one = true_values, rater_two = predicted_values, additional_raters = NULL
    )

    metric_values["percentage_agreement"] <- sum(diag(table(true_values, predicted_values)) / length(true_values))

    metric_values["balanced_accuracy"] <- sum(
      diag(val_res_free$categorical_level$raw_estimates$assignment_error_matrix)
    ) /
      ncol(val_res_free$categorical_level$raw_estimates$assignment_error_matrix)

    res_gwet_ac <- gwet_ac(rater_one = true_values, rater_two = predicted_values)

    metric_values["gwet_ac1_nominal"] <- res_gwet_ac$ac1
    metric_values["gwet_ac2_linear"] <- res_gwet_ac$ac2_linear
    metric_values["gwet_ac2_quadratic"] <- res_gwet_ac$ac2_quadratic

    # Standard measures
    standard_measures <- calc_standard_classification_measures(
      true_values = true_values,
      predicted_values = predicted_values
    )
    metric_values["avg_precision"] <- mean(standard_measures[, "precision"])
    metric_values["avg_recall"] <- mean(standard_measures[, "recall"])
    metric_values["avg_f1"] <- mean(standard_measures[, "f1"])

    return(metric_values)
  }
}

#------------------------------------------------------------------------------
#' @title Create an iota2 object
#' @description Function creates an object of class `iotarelr_iota2` which can be used with the package iotarelr. This
#'   function is for internal use only.
#'
#' @param iota2_list `list` of objects of class `iotarelr_iota2`.
#' @param free_aem `bool` `TRUE` if the iota2 objects are estimated without forcing the assumption of weak superiority.
#' @param call `string` characterizing the source of estimation. That is, the function within the object was estimated.
#' @param original_cat_labels `vector` containing the original labels of each category.
#' @return Returns an object of class `iotarelr_iota2` which is the mean iota2 object.
#' @family performance measures
#' @keywords internal
#' @noRd
create_iota2_mean_object <- function(iota2_list,
                                     free_aem = FALSE,
                                     call = "aifeducation::te_classifier_neuralnet",
                                     original_cat_labels) {
  if (free_aem == TRUE) {
    call <- paste0(call, "_free_aem")
  }

  mean_aem <- NULL
  mean_categorical_sizes <- NULL
  n_performance_estimation <- length(iota2_list)

  for (i in seq_len(length(iota2_list))) {
    if (i == 1) {
      mean_aem <- iota2_list[[i]]$categorical_level$raw_estimates$assignment_error_matrix
    } else {
      mean_aem <- mean_aem + iota2_list[[i]]$categorical_level$raw_estimates$assignment_error_matrix
    }
  }

  mean_aem <- mean_aem / n_performance_estimation
  mean_categorical_sizes <- iota2_list[[i]]$information$est_true_cat_sizes
  # mean_categorical_sizes <- mean_categorical_sizes / n_performance_estimation

  colnames(mean_aem) <- original_cat_labels
  rownames(mean_aem) <- original_cat_labels

  names(mean_categorical_sizes) <- original_cat_labels
  tmp_iota_2_measures <- iotarelr::get_iota2_measures(
    aem = mean_aem,
    categorical_sizes = mean_categorical_sizes,
    categorical_levels = original_cat_labels
  )

  Esimtates_Information <- NULL
  Esimtates_Information["log_likelihood"] <- list(NA)
  Esimtates_Information["iteration"] <- list(NA)
  Esimtates_Information["convergence"] <- list(NA)
  Esimtates_Information["est_true_cat_sizes"] <- list(mean_categorical_sizes)
  Esimtates_Information["conformity"] <- list(iotarelr::check_conformity_c(aem = mean_aem))
  Esimtates_Information["boundaries"] <- list(NA)
  Esimtates_Information["p_boundaries"] <- list(NA)
  Esimtates_Information["n_rater"] <- list(1)
  Esimtates_Information["n_cunits"] <- list(iota2_list[[i]]$information$n_cunits)
  Esimtates_Information["call"] <- list(call)
  Esimtates_Information["random_starts"] <- list(NA)
  Esimtates_Information["estimates_list"] <- list(NA)

  iota2_object <- NULL
  iota2_object["categorical_level"] <- list(tmp_iota_2_measures$categorical_level)
  iota2_object["scale_level"] <- list(tmp_iota_2_measures$scale_level)
  iota2_object["information"] <- list(Esimtates_Information)
  class(iota2_object) <- "iotarelr_iota2"

  return(iota2_object)
}

#' @title Calculate recall, precision, and f1-scores
#' @description Function for calculating recall, precision, and f1-scores.
#'
#' @param true_values `factor` containing the true labels/categories.
#' @param predicted_values `factor` containing the predicted labels/categories.
#' @return Returns a matrix which contains the cases categories in the rows and the measures (precision, recall, f1) in
#'   the columns.
#'
#' @family performance measures
#' @export
calc_standard_classification_measures <- function(true_values, predicted_values) {
  categories <- levels(true_values)
  results <- matrix(
    nrow = length(categories),
    ncol = 3
  )
  colnames(results) <- c("precision", "recall", "f1")
  rownames(results) <- categories

  for (i in seq_len(length(categories))) {
    bin_true_values <- (true_values == categories[i])
    bin_true_values <- factor(as.character(bin_true_values), levels = c("TRUE", "FALSE"))

    bin_pred_values <- (predicted_values == categories[i])
    bin_pred_values <- factor(as.character(bin_pred_values), levels = c("TRUE", "FALSE"))

    conf_matrix <- table(bin_true_values, bin_pred_values)
    conf_matrix <- conf_matrix[c("TRUE", "FALSE"), c("TRUE", "FALSE")]

    TP_FN <- (sum(conf_matrix[1, ]))
    if (TP_FN == 0) {
      recall <- NA
    } else {
      recall <- conf_matrix[1, 1] / TP_FN
    }

    TP_FP <- sum(conf_matrix[, 1])
    if (TP_FP == 0) {
      precision <- NA
    } else {
      precision <- conf_matrix[1, 1] / TP_FP
    }

    if (is.na(recall) || is.na(precision)) {
      f1 <- NA
    } else {
      f1 <- 2 * precision * recall / (precision + recall)
    }

    results[categories[i], 1] <- precision
    results[categories[i], 2] <- recall
    results[categories[i], 3] <- f1
  }
  return(results)
}
