#' @title Evaluate layer importance
#' @description Calculates the following values:
#'   - AV: Activation Variance
#'   - NAV: Normalized Activation Variance
#'   - AS: Activation Sparsity
#'   - NAS: Normalized Activation Sparsity
#'   - AVSS: Activation Variance-Sparsity Score
#'   - NAVSS: Normalized AVSS
#'   - CAVSS: Cumulative AVSS
#'
#' @param hidden_states `list` with length being equal to layer number. Each element (layer) of the list is an `array`
#'   with size: (`batch_size`, `sequence_length`, `hidden_size`)
#' @param layer_index `int` the index of the layer that must be evaluated
#' @param e `double` the threshold for activation
#'
#' @returns Returns a `list` with the values: AV, NAV, AS, NAS, AVSS, NAVSS, CAVSS
#'
#' @references
#'   Song, Z., Huang, S., Wu, Y., & Kang, Z. (2024). *Layer Importance and Hallucination Analysis in
#'   Large Language Models via Enhanced Activation Variance-Sparsity*.
#'   arXiv:2411.10069. https://doi.org/10.48550/arXiv.2411.10069
#'
#' @family AVSS
#' @export
evaluate_layer_importance <- function(hidden_states, layer_index, e) {

  num_layers <- length(hidden_states)

  if (layer_index < 1 || layer_index > num_layers) {
    return(NULL)
  }

  if (length(dim(hidden_states[[1]])) != 3) {
    return(NULL)
  }

  args = list(
    hidden_states = hidden_states,
    layer_index = layer_index,
    num_layers = num_layers,
    e = e
  )

  # === Activation Variance
  res <- calculate_value(fun = calculate_layer_AV, args = args)
  AV_Li <- res$value                # Activation Variance of Li layer
  NAV_Li <- res$normalized_value    # Normalized Activation Variance of Li layer

  # === Activation Sparsity
  res <- calculate_value(fun = calculate_layer_AS, args = args)
  AS_Li <- res$value               # Activation Sparsity of Li layer
  NAS_Li <- res$normalized_value   # Normalized Activation Sparsity of Li layer

  # === Activation Variance-Sparsity Score
  res <- calculate_value(fun = calculate_layer_AVSS, args = args, return_for_all_layers = TRUE)
  AVSS_Li <- res$value             # AVSS of Li layer
  NAVSS_Li <- res$normalized_value # Normalized AVSS of Li layer
  AVSS_arr <- res$values           # AVSS of each layer

  # === Normalized AVSS of each layer
  NAVSS_arr <- NULL
  for (k in 1:num_layers) {
    NAVSS_arr[k] <- AVSS_arr[k] / sum(AVSS_arr)
  }

  # === Cumulative AVSS
  CAVSS_Li <- sum(NAVSS_arr[1:layer_index])

  result_list <- list(
    AV = AV_Li,
    NAV = NAV_Li,
    AS = AS_Li,
    NAS = NAS_Li,
    AVSS = AVSS_Li,
    NAVSS = NAVSS_Li,
    CAVSS = CAVSS_Li
  )

  return(result_list)
}

#' @title Reshape a 3-D array (tensor) into 1-D array (vector)
#' @description Reshaping by calculating average. The result is a value for each neuron
#'
#' @param layer_hidden_states `3-D array (tensor)` with size (`batch_size`, `sequence_length`, `hidden_size`)
#'
#' @returns Returns an `array` with length being equal to `hidden_size`
#'
#' @family AVSS
reshape_layer <- function(layer_hidden_states) {
  Li <- layer_hidden_states
  Li <- apply(Li, 3, mean) # has length being equal to hidden_size
  return(Li)
}

#' @title Calculate value with the given function
#'
#' @param fun `function` defined function
#' @param args `list` with the arguments for executing the `fun` function
#' @param return_for_all_layers `bool` `FALSE` (by default): do not return values for all layers, otherwise - `TRUE`
#'
#' @returns Returns a value and normalized value, if needed - value for all layers (`return_for_all_layers` = `TRUE`)
#'
#' @family AVSS
calculate_value <- function(fun, args, return_for_all_layers = FALSE) {
  hidden_states <- args$hidden_states
  layer_index <- args$layer_index
  num_layers <- args$num_layers
  e <- args$e

  value_arr <- NULL
  for (k in 1:num_layers) {
    Lk <- reshape_layer(hidden_states[[k]])
    value_arr[k] <- do.call(what = fun,
                            args = list(layer_hidden_states = Lk,
                                        e = e))
  }
  value_Li <- value_arr[layer_index]               # Value of Li layer
  normalized_value_Li <- value_Li / sum(value_arr) # Normalized value of Li layer

  res_list <- list(
    value = value_Li,
    normalized_value = normalized_value_Li
  )

  if (return_for_all_layers) {
    res_list$values <- value_arr
  }
  return(res_list)
}

#' @title Calculate Activation Variance of a layer
#' @description Formula for calculating:
#'   AV(L_i) = 1/N * sum{j=1,N}((a_j(L_i) - mu(L_i))^2)
#'
#' @param layer_hidden_states `1-D array (vector)` with length being equal to `hidden_size`
#' @param ... to generalize the structure of functions for using with `calculate_value` function
#'
#' @returns Returns Activation Variance
#'
#' @family AVSS
calculate_layer_AV <- function(layer_hidden_states, ...) {
  Li <- layer_hidden_states
  hidden_size <- length(Li)

  # average layer activation
  mu_Li <- mean(Li)

  sum_aj_Li <- 0
  for (j in 1:hidden_size) {
    aj_Li <- Li[j]
    sum_aj_Li <- sum_aj_Li + (aj_Li - mu_Li) * (aj_Li - mu_Li)
  }

  # Activation Variance of Li layer
  AV_Li <- sum_aj_Li / hidden_size
  return(AV_Li)
}

#' @title Calculate Activation Sparsity of a layer
#' @description Formula for calculating:
#'   AS(L_i) = 1/N * sum{j=1,N}(bool(|a_j(L_i)|<e))
#'
#' @param layer_hidden_states `1-D array (vector)` with length being equal to `hidden_size`
#' @param e `double` the threshold for activation
#'
#' @returns Returns Activation Sparsity
#'
#' @family AVSS
calculate_layer_AS <- function(layer_hidden_states, e) {
  Li <- layer_hidden_states
  hidden_size <- length(Li)

  sum_aj_Li <- sum(abs(Li) < e)
  AS_Li <- sum_aj_Li / hidden_size
  return(AS_Li)
}

#' @title Calculate Activation Variance-Sparsity Score of a layer
#' @description Formula for calculating:
#'   AVSS(L_i) = AV(L_i) / AS(L_i)
#'
#' @param layer_hidden_states `1-D array (vector)` with length being equal to `hidden_size`
#' @param e `double` the threshold for activation
#'
#' @returns Returns Activation Variance-Sparsity Score
#'
#' @family AVSS
calculate_layer_AVSS <- function(layer_hidden_states, e) {
  Li <- layer_hidden_states
  AVSS_Li <- calculate_layer_AV(Li) / calculate_layer_AS(Li, e)
  return(AVSS_Li)
}
