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
#'   * **gwet_ac**: Gwet's AC1/AC2 agreement coefficient.
#'
#' @return If `return_names_only = TRUE` returns only the names of the vector elements.
#'
#' @family classifier_utils
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
    "gwet_ac",
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

    metric_values["kalpha_nominal"] <- irr::kripp.alpha(
      x = rbind(true_values, predicted_values),
      method = "nominal"
    )$value
    metric_values["kalpha_ordinal"] <- irr::kripp.alpha(
      x = rbind(true_values, predicted_values),
      method = "ordinal"
    )$value

    #Kendall
    metric_values["kendall"] <- kendalls_w(
      rater_one = true_values,
      rater_two = predicted_values)$kendall_w_corrected

    #Cohens Kappa
    c_kappa=cohens_kappa(rater_one = true_values,rater_two = predicted_values)
      metric_values["c_kappa_unweighted"] <- c_kappa$kappa_unweighted
      metric_values["c_kappa_linear"] <- c_kappa$kappa_linear
      metric_values["c_kappa_squared"] <- c_kappa$kappa_squared

    metric_values["kappa_fleiss"] <- irr::kappam.fleiss(
      ratings = cbind(true_values, predicted_values),
      exact = FALSE,
      detail = FALSE
    )$value

    metric_values["percentage_agreement"] <- sum(diag(table(rater_one,rater_two))/length(rater_one))

    metric_values["balanced_accuracy"] <- sum(
      diag(val_res_free$categorical_level$raw_estimates$assignment_error_matrix)) /
      ncol(val_res_free$categorical_level$raw_estimates$assignment_error_matrix)

    metric_values["gwet_ac"] <- irrCAC::gwet.ac1.raw(ratings = cbind(true_values, predicted_values))$est$coeff.val

    #Standard measures
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
#' @family classifier_utils
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

  for (i in 1:length(iota2_list)) {
    if (i == 1) {
      mean_aem <- iota2_list[[i]]$categorical_level$raw_estimates$assignment_error_matrix
    } else {
      mean_aem <- mean_aem + iota2_list[[i]]$categorical_level$raw_estimates$assignment_error_matrix
    }
  }

  mean_aem <- mean_aem / n_performance_estimation
  mean_categorical_sizes <- iota2_list[[i]]$information$est_true_cat_sizes
  # mean_categorical_sizes<-mean_categorical_sizes/n_performance_estimation

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
  # Esimtates_Information["conformity"] <- list(NA)
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

#' @title Calculate standard classification measures
#' @description Function for calculating recall, precision, and f1.
#'
#' @param true_values `factor` containing the true labels/categories.
#' @param predicted_values `factor` containing the predicted labels/categories.
#' @return Returns a matrix which contains the cases categories in the rows and the measures (precision, recall, f1) in
#'   the columns.
#'
#' @family classifier_utils
#' @export
calc_standard_classification_measures <- function(true_values, predicted_values) {
  categories <- levels(true_values)
  results <- matrix(
    nrow = length(categories),
    ncol = 3
  )
  colnames(results) <- c("precision", "recall", "f1")
  rownames(results) <- categories

  for (i in 1:length(categories)) {
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
