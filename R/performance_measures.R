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
  check_class(rater_one, "factor", FALSE)
  check_class(rater_two, "factor", FALSE)
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
  check_class(rater_one, "factor", FALSE)
  check_class(rater_two, "factor", FALSE)
  check_class(additional_raters, "list", TRUE)

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
  check_class(rater_one, "factor", FALSE)
  check_class(rater_two, "factor", FALSE)
  check_class(additional_raters, "list", TRUE)

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
  check_class(rater_one, "factor", FALSE)
  check_class(rater_two, "factor", FALSE)
  check_class(additional_raters, "list", TRUE)

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
  check_class(rater_one, "factor", FALSE)
  check_class(rater_two, "factor", FALSE)
  check_class(additional_raters, "list", TRUE)

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
