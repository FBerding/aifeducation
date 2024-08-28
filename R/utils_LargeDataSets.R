#' @title Convert data.frame to arrow data set
#' @description Function for converting a data.frame into a pyarrow data set.
#'
#' @param data_frame Object of class `data.frame`.
#' @return Returns the `data.frame` as a pyarrow data set of class `datasets.arrow_dataset.Dataset`.
#'
#' @family Data Management
#' @keywords internal
data.frame_to_py_dataset <- function(data_frame) {
  if (nrow(data_frame) == 1) {
    data_frame <- rbind(data_frame, data_frame)
    data_list <- as.list(data_frame)
    data_dict <- reticulate::dict(data_list)
    dataset <- datasets$Dataset$from_dict(data_dict)
    dataset <- dataset$select(indices = list(as.integer(0)))
  } else {
    data_list <- as.list(data_frame)
    data_dict <- reticulate::dict(data_list)
    dataset <- datasets$Dataset$from_dict(data_dict)
  }
  return(dataset)
}

#' @title Convert arrow data set to an arrow data set
#' @description Function for converting an arrow data set into a data set that can be used to store and process
#'   embeddings.
#'
#' @param py_dataset Object of class `datasets.arrow_dataset.Dataset`.
#' @return Returns the data set of class `datasets.arrow_dataset.Dataset` with only two columns ("id","input"). "id"
#'   stores the name of the cases while "input" stores the embeddings.
#'
#' @family Data Management
#' @keywords internal
py_dataset_to_embeddings <- function(py_dataset) {
  py_dataset$set_format("np")
  embeddings <- py_dataset["input"]
  rownames(embeddings) <- py_dataset["id"]
  return(embeddings)
}

#' @title Convert R array for arrow data set
#' @description Function converts a R array into a numpy array that can be added to an arrow data set. The array should
#'   represent embeddings.
#'
#' @param r_array `array` representing embeddings.
#' @return Returns a numpy array.
#'
#' @family Data Management
#' @keywords internal
prepare_r_array_for_dataset <- function(r_array) {
  return(np$squeeze(np$split(reticulate::np_array(r_array), as.integer(nrow(r_array)), axis = 0L)))
}

#' @title Assign cases to batches
#' @description Function groups cases into batches.
#'
#' @param number_rows `int` representing the number of cases or rows of a matrix or array.
#' @param batch_size `int` size of a batch.
#' @param zero_based `bool` If `TRUE` the indices of the cases within each batch are zero based. One based if `FALSE`.
#' @return Returns a `list` of batches. Each entry in the list contains a `vector` of `int` representing the cases
#'   belonging to that batch.
#'
#' @family Data Management
#' @keywords internal
get_batches_index <- function(number_rows, batch_size, zero_based = FALSE) {
  n_batches <- ceiling(number_rows / batch_size)
  index_list <- NULL
  for (i in 1:n_batches) {
    min <- 1 + (i - 1) * batch_size
    max <- min(batch_size + (i - 1) * batch_size, number_rows)
    if (zero_based == FALSE) {
      index_list[i] <- list(seq.int(from = min, to = max, by = 1))
    } else {
      index_list[i] <- list(seq.int(from = min, to = max, by = 1) - 1)
    }
  }
  return(index_list)
}

#' @title Reduce to unique cases
#' @description Function creates an arrow data set that contains only unique cases. That is, duplicates are removed.
#'
#' @param dataset_to_reduce Object of class `datasets.arrow_dataset.Dataset`.
#' @param column_name `string` Name of the column whose values should be unique.
#' @return Returns a data set of class `datasets.arrow_dataset.Dataset` where the duplicates are removed according to
#'   the given column.
#'
#' @family Data Management
#' @keywords internal
reduce_to_unique <- function(dataset_to_reduce, column_name) {
  selected_column <- dataset_to_reduce$data$column(column_name)
  unique_values <- pyarrow$compute$unique(selected_column)
  unique_indices <- pyarrow$compute$index_in(unique_values, selected_column)
  reduced_dataset <- dataset_to_reduce$select(unique_indices)
  return(reduced_dataset)
}

#' @title Convert class vector to arrow data set
#' @description Function converts a vector of class indices into an arrow data set.
#'
#' @param vector `vector` of class indices.
#' @return Returns a data set of class `datasets.arrow_dataset.Dataset` containing the class indices.
#'
#' @family Data Management
#' @keywords internal
class_vector_to_py_dataset <- function(vector) {
  data_frame <- as.data.frame(vector)
  data_frame$id <- names(vector)
  colnames(data_frame) <- c("labels", "id")
  if (length(data_frame) == 1) {
    data_frame <- rbind(data_frame, data_frame)
    data_list <- as.list(data_frame)
    data_dict <- reticulate::dict(data_list)
    dataset <- datasets$Dataset$from_dict(data_dict)
    dataset <- dataset$select(indices = list(as.integer(0)))
  } else {
    data_list <- as.list(data_frame)
    data_dict <- reticulate::dict(data_list)
    dataset <- datasets$Dataset$from_dict(data_dict)
  }
  return(dataset)
}

#' @title Combine embeddings in array form
#' @description Function combines two array by adding an array to another array along the
#' first dimension.
#'
#' @param ... `array` or `list` of `array`s. Arrays to be combined. All array must have row names.
#'
#' @return Returns the combined array
#'
#' @family Data Management
#' @keywords internal
array_form_bind <- function(...) {
  objects <- list(...)
  arrays <- NULL

  for (object in objects) {
    if (is.list(object)) {
      for (j in 1:length(object)) {
        arrays[length(arrays) + 1] <- list(object[[j]])
      }
    } else {
      arrays[length(arrays) + 1] <- list(object)
    }
  }

  # arrays <- list(...)
  if (length(arrays) > 1) {
    total_rows <- 0

    dimension <- dim(arrays[[1]])

    for (i in 1:length(arrays)) {
      total_rows <- total_rows + dim(arrays[[i]])[1]

      # Check number of dimensions
      if (sum(dim(arrays[[i]])[-1] != dimension[-1])) {
        stop("The dimensions of the array differ.")
      }
    }

    combined_array <- array(
      data = NA,
      dim = c(total_rows, dimension[2], dimension[3])
    )

    intercept <- 0
    row_names <- NULL


    for (i in 1:length(arrays)) {
      index <- seq.int(
        from = 1,
        to = nrow(arrays[[i]]),
        by = 1
      ) + intercept

      combined_array[index, , ] <- arrays[[i]]

      intercept <- intercept + length(index)

      row_names <- append(x = row_names, rownames(arrays[[i]]))
    }

    rownames(combined_array) <- row_names
    return(combined_array)
  } else {
    return(arrays[[1]])
  }
}
