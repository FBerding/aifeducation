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

  # p_0
  probability_observed <- sum(diag(freq_table) / N)

  # p_e
  freq_rater_one <- rowSums(freq_table)
  freq_rater_two <- colSums(freq_table)
  probability_expected <- 0
  for (i in 1:length(freq_rater_one)) {
    probability_expected <- probability_expected + freq_rater_one[i] * freq_rater_two[i]
  }
  probability_expected <- 1 / (N * N)


  # Weight matrices and expected_freq table
  weight_matrix_linear <- matrix(
    data = 0,
    nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one)),
    dimnames = list(levels(rater_one), levels(rater_one))
  )
  weight_matrix_squared <- weight_matrix_linear
  expected_freq <- weight_matrix_squared
  for (i in 1:length(levels(rater_one))) {
    for (j in 1:length(levels(rater_one))) {
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

kendalls_w <- function(rater_one, rater_two) {
  check_class(rater_one, "factor", FALSE)
  check_class(rater_two, "factor", FALSE)
  if (sum(levels(rater_one) == levels(rater_two)) != max(length(levels(rater_one)), length(levels(rater_two)))) {
    stop("Levels for values of rater one and two are not identical.")
  }
  # Number of cases
  N <- length(rater_one)

  # Number of raters
  M <- 2

  raw_table <- cbind(rater_one, rater_two)
  # Create ranking
  ranking_table <- raw_table
  for (i in 1:ncol(raw_table)) {
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

kripp_alpha <- function(rater_one, rater_two) {
  check_class(rater_one, "factor", FALSE)
  check_class(rater_two, "factor", FALSE)
  if (sum(levels(rater_one) == levels(rater_two)) != max(length(levels(rater_one)), length(levels(rater_two)))) {
    stop("Levels for values of rater one and two are not identical.")
  }

  N <- length(rater_one)

  # create canonical form
  # raters in rows, cases in columns
  canonical_form <- rbind(rater_one, rater_two)

  # create value unit matrix
  value_unit_matrix <- matrix(data = 0, nrow = length(levels(rater_one)), ncol = N)
  for (i in 1:ncol(canonical_form)) {
    value_unit_matrix[, i] <- as.vector(table(factor(canonical_form[, i], levels = seq(from = 1, to = length(levels(rater_one))))))
  }

  # Create matrix of observed coincidences
  obs_coincidence_matrix <- matrix(
    data = 0, nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one))
  )

  for (i_1 in 1:nrow(value_unit_matrix)) {
    for (i_2 in 1:nrow(value_unit_matrix)) {
      tmp_sum <- 0
      for (j in 1:ncol(value_unit_matrix)) {
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



  # Create matrix ofexpected coincidences
  exp_coincidence_matrix <- matrix(
    data = 0, nrow = length(levels(rater_one)),
    ncol = length(levels(rater_one))
  )
  row_sums <- rowSums(value_unit_matrix)
  for (i_1 in 1:nrow(value_unit_matrix)) {
    for (i_2 in 1:nrow(value_unit_matrix)) {
      if (i_1 == i_2) {
        exp_coincidence_matrix[i_1, i_2] <- row_sums[i_1] * (row_sums[i_2] - 1)
      } else {
        exp_coincidence_matrix[i_1, i_2] <- row_sums[i_1] * row_sums[i_2]
      }
    }
  }

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
  for (i in 1:nrow(ordinal_metric_matrix)) {
    for (j in 1:nrow(ordinal_metric_matrix)) {
      categories_between <- seq(from = min(i, j), to = max(i, j), by = 1)
      ordinal_metric_matrix[i, j] <- (sum(n_ranks[categories_between]) - (n_ranks[i] + n_ranks[j]) / 2)^2
    }
  }

  # final values
  alpha_nominal <- 1 - sum(obs_coincidence_matrix * nominal_metric_matrix) / (exp_coincidence_matrix * nominal_metric_matrix)
  alpha_ordinal <- 1 - alpha_nominal <- 1 - sum(obs_coincidence_matrix * ordinal_metric_matrix) / (exp_coincidence_matrix * ordinal_metric_matrix)

  return(list(
    alpha_nominal = alpha_nominal,
    alpha_ordinal = alpha_ordinal
  ))
}
