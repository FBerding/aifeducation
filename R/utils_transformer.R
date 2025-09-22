# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Estimate tokenizer statistics
#' @description Function for estimating the tokenizer statistics described by Kaya & Tantuğ (2024).
#'
#' @param dataset Object of class datasets.arrow_dataset.Dataset. The data set must contain a column `"length"`
#'   containing the number of tokens for every sequence and a column `"word_ids"` containing the word ids within every
#'   sequence.
#' @param step `string` indicating to which step the statistics belong. Recommended values are
#' * `"creation"` for the creation of the tokenizer.
#' * `"initial_training"` for the first training of the transformer.
#' * `"fine_tuning"` for all following trainings of the transformer.
#' * `"training"` for a training run of the transformer.
#' @param statistics_max_tokens_length `r get_param_doc_desc("statistics_max_tokens_length")`
#' @return Returns a `list` with the following entries:
#' * n_sequences: Number of sequences
#' * n_words: Number for words in whole corpus
#' * n_tokens: Number of tokens in the whole corpus
#' * mu_t: eqn(n_tokens/n_sequences)
#' * mu_w: eqn(n_words/n_sequences)
#' * mu_g: eqn(n_tokens/n_words)
#'
#' @references Kaya, Y. B., & Tantuğ, A. C. (2024). Effect of tokenization granularity for Turkish large language
#' models. Intelligent Systems with Applications, 21, 200335. https://doi.org/10.1016/j.iswa.2024.200335
#'
#' @family Utils Transformers Developers
#' @export
calc_tokenizer_statistics <- function(dataset, step = "creation", statistics_max_tokens_length = 512L) {
  # Argument Checking
  check_class(object = dataset, classes = "datasets.arrow_dataset.Dataset", allow_NULL = FALSE)

  n_sequences <- dataset$num_rows
  n_words <- NA
  n_tokens <- NA
  mu_t <- NA
  mu_w <- NA
  mu_g <- NA

  if (step == "training" || step == "creation") {
    if (!"word_ids" %in% dataset$column_names) {
      stop("dataset must contain a column 'word_ids'.")
    }
    if (!"length" %in% dataset$column_names) {
      stop("dataset must contain a column 'length'.")
    }

    n_words <- 0L
    n_tokens <- 0L
    for (i in 1L:n_sequences) {
      n_words <- n_words + length(unique(unlist(dataset[i - 1L]$word_ids)))
      n_tokens <- n_tokens + dataset[i - 1L]$length
    }

    mu_t <- n_tokens / n_sequences
    mu_w <- n_words / n_sequences
    mu_g <- n_tokens / n_words
  } else {
    stop("Step ", step, " is invalid. Allowed steps: creation or training")
  }

  return(
    list(
      step = step,
      date = get_time_stamp(),
      max_tokens_length = statistics_max_tokens_length,
      n_sequences = n_sequences,
      n_words = n_words,
      n_tokens = n_tokens,
      mu_t = mu_t,
      mu_w = mu_w,
      mu_g = mu_g
    )
  )
}
