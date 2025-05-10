generate_args_for_tests <- function(object_name,
                                    method,
                                    max_samples = 100,
                                    var_objects = list(),
                                    necessary_objects = list(),
                                    var_override = list(
                                      sustain_interval = 30,
                                      trace = FALSE,
                                      epochs = 50,
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
                                      lr_warm_up_ratio = 0.01
                                    )) {
  object <- create_object(object_name)
  arg_list <- rlang::fn_fmls(object[[method]])
  arg_names <- names(arg_list)
  param_dict <- get_param_dict()

  # Generate list of values for every parameter that can vary
  arg_value_list <- NULL
  for (param in arg_names) {
    current_entry <- param_dict[[param]]
    if (is_valid_and_exportable_param(param, param_dict) & !(param %in% c(names(var_override), names(necessary_objects)))) {
      if (current_entry$type == "string") {
        if (!is.null(current_entry$allowed_values)) {
          arg_value_list[param] <- list(current_entry$allowed_values)
        }
      } else if (current_entry$type == "bool") {
        arg_value_list[param] <- list(c(FALSE, TRUE))
      } else {
        if (current_entry$min == -Inf) {
          tmp_min <- -1
        } else {
          tmp_min <- current_entry$min
        }

        if (current_entry$max == Inf) {
          tmp_max <- 2
        } else {
          tmp_max <- current_entry$max
        }
      }

      if (current_entry$type == "int") {
        arg_value_list[param] <- list(seq(from = tmp_min, to = tmp_max, by = 1))
      } else if (current_entry$type == "double") {
        arg_value_list[param] <- list(c(tmp_min, tmp_max, 0.5 * tmp_min + 0.5 * tmp_max))
      } else if (current_entry$type == "(double") {
        arg_value_list[param] <- list(c(0.99 * tmp_min, tmp_max, 0.5 * tmp_min + 0.5 * tmp_max))
      } else if (current_entry$type == "double)") {
        arg_value_list[param] <- list(c(tmp_min, 0.99 * tmp_max, 0.5 * tmp_min + 0.5 * tmp_max))
      } else if (current_entry$type == "(double)") {
        arg_value_list[param] <- list(c(0.99 * tmp_min, 0.99 * tmp_max, 0.5 * tmp_min + 0.5 * tmp_max))
      }
    }
  }

  # Add var objects
  for (var_object in names(var_objects)) {
    arg_value_list[var_object] <- list(c(FALSE, TRUE))
  }

  # create all combinations
  arg_comb <- expand.grid(arg_value_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  # Convert combinations to list and add override parameters and add necessary parameters
  arg_comb_list <- NULL
  override_subset <- intersect(arg_names, names(var_override))
  necessary_subset <- intersect(arg_names, names(necessary_objects))
  for (i in 1:nrow(arg_comb)) {
    arg_comb_list[i] <- list(
      c(
        arg_comb[i, ],
        var_override[override_subset],
        necessary_objects[necessary_subset]
      )
    )
  }

  # add var objects
  # Replace FALSE with NULL and TRUE with the corresponding object
  for (i in seq_along(arg_comb_list)) {
    for (var_object in names(var_objects)) {
      if (arg_comb_list[[i]][[var_object]] == TRUE) {
        arg_comb_list[[i]][var_object] <- list(var_objects[[var_object]])
      } else {
        arg_comb_list[[i]][var_object] <- list(NULL)
      }
    }
  }
  n_all_combos <- length(arg_comb_list)
  n_samples <- min(max_samples, n_all_combos)

  final_arg_list <- arg_comb_list[sample(x = seq(from = 1, to = n_samples, by = 1), replace = FALSE)]

  return(arg_list = list(args = final_arg_list, n_combos = n_samples))
}


check_adjust_n_samples_on_CI <- function(
    n_samples_requested,
    n_CI = 50) {
  # If on github use only a small random sample
  if (Sys.getenv("CI") != "true") {
    return(min(n_samples_requested, n_CI))
  } else {
    return(n_samples_requested)
  }
}

get_test_data_for_classifiers=function(class_range=c(2,3),
                                       path_test_embeddings){
  # Load Embeddings
  imdb_embeddings <- load_from_disk(path_test_embeddings)

  test_embeddings_large <- imdb_embeddings$convert_to_LargeDataSetForTextEmbeddings()
  test_embeddings <- test_embeddings_large$convert_to_EmbeddedText()

  test_embeddings_reduced <- test_embeddings$clone(deep = TRUE)
  test_embeddings_reduced$embeddings <- test_embeddings_reduced$embeddings[1:5, , ]
  test_embeddings_reduced_LD <- test_embeddings_reduced$convert_to_LargeDataSetForTextEmbeddings()

  test_embeddings_single_case <- test_embeddings$clone(deep = TRUE)
  test_embeddings_single_case$embeddings <- test_embeddings_single_case$embeddings[1, , , drop = FALSE]
  test_embeddings_single_case_LD <- test_embeddings_single_case$convert_to_LargeDataSetForTextEmbeddings()



  # Prepare data for different classification types---------------------------
  target_data <- NULL
  target_levels <- NULL
  for (n_classes in class_range) {
    example_data <- imdb_movie_reviews

    rownames(example_data) <- rownames(test_embeddings$embeddings)
    example_data$id <- rownames(test_embeddings$embeddings)
    example_data <- example_data[intersect(
      rownames(example_data), rownames(test_embeddings$embeddings)
    ), ]

    example_data$label <- as.character(example_data$label)
    example_data$label[c(201:300)] <- NA
    if (n_classes > 2) {
      example_data$label[c(201:250)] <- "medium"
      tmp_target_levels <- c("neg", "medium", "pos")
    } else {
      tmp_target_levels <- c("neg", "pos")
    }
    example_targets <- as.factor(example_data$label)
    names(example_targets) <- example_data$id

    target_data[n_classes] <- list(example_targets)
    target_levels[n_classes] <- list(tmp_target_levels)
  }

  return(
    list(
      target_data=target_data,
      target_levels=target_levels,
      test_embeddings_large=test_embeddings_large,
      test_embeddings=test_embeddings,
      test_embeddings_reduced=test_embeddings_reduced,
      test_embeddings_reduced_LD=test_embeddings_reduced_LD,
      test_embeddings_single_case=test_embeddings_single_case,
      test_embeddings_single_case_LD=test_embeddings_single_case_LD
         )
         )
}

get_current_args_for_print=function(arg_list){
  if(!is.list(arg_list)){
    stop("arg_list must be a list.")
  }
  return(paste(names(arg_list),arg_list,collapse = ", "))
}
