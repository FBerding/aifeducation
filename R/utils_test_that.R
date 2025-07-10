
#'@title Generate combinations of arguments
#'@description Function generates a specific number of combinations for a method.
#'These are used for automating tests of objects.
#'@param object_name `string` Name of the object to generate the arguments for.
#'@param method `string` Name of the method of the object to generate the arguments for.
#'@param var_objects `list` of other objects which should be combined with the other arguments.
#'@param necessary_objects `list` of other objects which are part of every combination.
#'@param var_override Named `list` containing the arguments which should be set to a specific value
#'for all combinations.
#'@returns Returns a `list` with combinations of arguments.
#'@note `var_objects`, `necessary_objects`, and `var_override` the names must exactly match
#'the name of the parameter. Otherwise they are not applied. Names of arguments which are not part
#'a a method are ignored. #'
#' @family Utils TestThat Developers
generate_args_for_tests <- function(object_name,
                                    method,
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
          tmp_max <- 3
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

  # create a combination
  arg_comb=list()
  for(i in seq_along(arg_value_list)){
    arg_comb[names(arg_value_list)[i]]=list(sample(
      x=arg_value_list[[i]],size = 1
    )
    )
  }

  # Convert combinations to list and add override parameters and add necessary parameters
  arg_comb_list <- NULL
  override_subset <- intersect(arg_names, names(var_override))
  necessary_subset <- intersect(arg_names, names(necessary_objects))

  arg_comb_list=append(x=arg_comb,values = var_override[override_subset])
  arg_comb_list=append(x=arg_comb_list,values = necessary_objects[necessary_subset])

  # add var objects
  # Replace FALSE with NULL and TRUE with the corresponding object
  #print(arg_comb_list)
  #print("---------")
    for (var_object in names(var_objects)) {
      if (arg_comb_list[[var_object]] == TRUE) {
        arg_comb_list[var_object] <- list(var_objects[[var_object]])
      } else {
        arg_comb_list[var_object] <- list(NULL)
      }
    }
return(arg_comb_list)
}

#'@title Set sample size for argument combinations
#'@description Function adjust the number of samples depending on the test environment.
#'On continuous integration it is limited to a random sample of combinations.
#'@param n_samples_requested `int` Number of samples if the test do not run on continuous integration.
#'@param n_CI `int` Number of samples if the test run on continuous integration.
#'@return Returns an `int` depending on the test environment.
#' @family Utils TestThat Developers
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

#'@title Get test data
#'@description Function returns example data for testing the package
#'@param class_range `vector` containing the number of classes.
#'@param path_test_embeddings `string` Path to the location where the test data is stored.
#'@return Returns a `list` with test data.
#' @family Utils TestThat Developers
get_test_data_for_classifiers=function(class_range=c(2,3),
                                       path_test_embeddings){
  # Load Embeddings
  imdb_embeddings <- load_from_disk(path_test_embeddings)

  test_embeddings_large <- imdb_embeddings$convert_to_LargeDataSetForTextEmbeddings()
  test_embeddings <- test_embeddings_large$convert_to_EmbeddedText()

  test_embeddings_reduced <- test_embeddings$clone(deep = TRUE)
  test_embeddings_reduced$embeddings <- test_embeddings_reduced$embeddings[c(1:5,120:125), , ]
  test_embeddings_reduced_LD <- test_embeddings_reduced$convert_to_LargeDataSetForTextEmbeddings()

  test_embeddings_single_case <- test_embeddings$clone(deep = TRUE)
  test_embeddings_single_case$embeddings <- test_embeddings_single_case$embeddings[1, , , drop = FALSE]
  test_embeddings_single_case_LD <- test_embeddings_single_case$convert_to_LargeDataSetForTextEmbeddings()



  # Prepare data for different classification types---------------------------
  target_data <- NULL
  target_levels <- NULL
  for (n_classes in class_range) {
    #Load raw text and labels
    #imdb_movie_reviews
    load(testthat::test_path("test_data/imdb_movie_reviews.rda"))
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

#'@title Print arguments
#'@description Functions prints the used arguments. The aim of this function is
#'to print the arguments to the console that resulted in a failed test.
#'@param arg_list Named `list` of arguments. The list should be generated with [generate_args_for_tests].
#'@return Function does nothing return.
#' @family Utils TestThat Developers
get_current_args_for_print=function(arg_list){
  if(!is.list(arg_list)){
    stop("arg_list must be a list.")
  }
  return(paste(names(arg_list),arg_list,collapse = ", "))
}

#'@title Generate test tensors
#'@description Functions generates a random test tensor that can be used for
#' testing methods and functions based on 'PyTorch'. The tensors have the shape
#' (Batch, Times,Features).
#' @param times `int` Maximal length of a sequence.
#' @param features `int` Number of features of the sequence.
#' @param seq_len Numeric `vector` containing the length of the given cases. The
#' length of this vector determines the value for 'Batch'. Values must be at least 1 and
#' maximal `times`.
#' @param pad_value `int` Value used to indicate padding.
#' @returns Returns an object of class `Tensor` from 'PyTorch'.
#' @note To request a *R* array please use [generate_embeddings].
#' @family Utils TestThat Developers
generate_tensors=function(times,
                          features,
                          seq_len,
                          pad_value
){
  tensor_data=array(
    data = pad_value,
    dim = c(length(seq_len),times,features)
  )
  for(i in seq_along(seq_len)){
    for(j in seq(from=1,to=seq_len[i])){
      for(f in 1:features){
        tensor_data[i,j,f]<-runif(n=1,min = -1,max = 1)
      }
    }
  }
  tensor_np=reticulate::np_array(tensor_data)
  if (numpy_writeable(tensor_np) == FALSE) {
    warning("Numpy array is not writable")
  }
  tensor=torch$from_numpy(tensor_np)
  return(tensor)
}

#'@title Generate test embeddings
#'@description Functions generates a random test embedding that can be used for
#' testing methods and functions. The embeddings have the shape
#' (Batch, Times,Features).
#' @param times `int` Maximal length of a sequence.
#' @param features `int` Number of features of the sequence.
#' @param seq_len Numeric `vector` containing the length of the given cases. The
#' length of this vector determines the value for 'Batch'. Values must be at least 1 and
#' maximal `times`.
#' @param pad_value `int` Value used to indicate padding.
#' @returns Returns an `array` with dim `(length(seq_len),times,features)`.
#' @note To generate a 'PyTorch' object please use [generate_tensors].
#' @family Utils TestThat Developers
generate_embeddings=function(times,
                          features,
                          seq_len,
                          pad_value
){
  tensor_data=array(
    data = pad_value,
    dim = c(length(seq_len),times,features)
  )
  for(i in seq_along(seq_len)){
    for(j in seq(from=1,to=seq_len[i])){
      for(f in 1:features){
        tensor_data[i,j,f]<-runif(n=1,min = -1,max = 1)
      }
    }
  }
  return(tensor_data)
}

#'@title Generate static test tensor
#'@description Function generates a static test tensor which is always the same.
#' @param pad_value `int` Value used to indicate padding.
#' @returns Returns an object of class `Tensor` which is always the same except padding.
#' Shape (5,3,7).
#' @family Utils TestThat Developers
get_fixed_test_tensor=function(pad_value){
  times=3
  features=7
  batch=5
  seq_len=c(1,2,1,3,2)
  tensor_data=array(
    data = pad_value,
    dim = c(batch,times,features)
  )

  for(i in seq_along(seq_len)){
    for(j in seq(from=1,to=seq_len[i])){
      tensor_data[i,j,]=seq(from=-seq_len[i],to=j*batch,length.out=features)
    }
  }
  tensor_np=reticulate::np_array(tensor_data)
  if (numpy_writeable(tensor_np) == FALSE) {
    warning("Numpy array is not writable")
  }
  tensor=torch$from_numpy(tensor_np)
  return(tensor)

}

