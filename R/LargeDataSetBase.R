#' @title Abstract base class for large data sets.
#'
#' @description This object contains public and private methods which may be useful for every
#' large data sets. Objects of this class are not intended to be used directly.
#' [LargeDataSetForTextEmbeddings] or [LargeDataSetForText].
#'
#' @return Returns a new object of this class.
#' @export
#' @family LargeDataSets for developers
LargeDataSetBase <- R6::R6Class(
  classname = "LargeDataSetBase",
  public = list(

    #--------------------------------------------------------------------------
    #' @description Number of columns in the data set.
    #' @return `int` describing the number of columns in the data set.
    n_cols = function() {
      return(private$data$num_columns)
    },


    #--------------------------------------------------------------------------
    #' @description Number of rows in the data set.
    #' @return `int` describing the number of rows in the data set.
    n_rows = function() {
      return(private$data$num_rows)
    },

    #--------------------------------------------------------------------------
    #' @description Get names of the columns in the data set.
    #' @return `vector` containing the names of the columns as `string`s.
    get_colnames = function() {
      return(private$data$column_names)
    },

    #--------------------------------------------------------------------------
    #' @description Get data set.
    #' @return Returns the data set of this object as an object of class `datasets.arrow_dataset.Dataset`.
    get_dataset = function() {
      return(private$data)
    },

    #--------------------------------------------------------------------------
    #' @description Reduces the data set to a data set containing only unique ids.
    #' In the case an id exists multiple times in the data set the first case
    #' remains in the data set. The other cases are dropped.
    #'** Attention** Calling this method will change the data set in place.
    #' @return Method does not return anything. It changes the data set of this
    #' object in place.
    reduce_to_unique_ids = function() {
      private$data <- reduce_to_unique(private$data, "id")
    },

    #--------------------------------------------------------------------------
    #' @description Returns a data set which contains only the cases belonging to
    #' the specific indices.
    #' @param  indicies `vector` of `int` for selecting rows in the data set.
    #'** Attention** The indices are zero-based.
    #' @return Returns a data set of class `datasets.arrow_dataset.Dataset` with the
    #' selected rows.
    select = function(indicies) {
      private$data$set_format("np")
      if (length(indicies) > 1) {
        return(private$data$select(as.integer(indicies)))
      } else {
        return(private$data$select(list(as.integer(indicies))))
      }
    },

    #--------------------------------------------------------------------------
    #' @description Get ids
    #' @return Returns a `vector` containing the ids of every row as `string`s.
    get_ids = function() {
      return(private$data["id"])
    },

    #--------------------------------------------------------------------------
    #' @description Saves a data set to disk.
    #' @param dir_path Path where to store the data set.
    #' @param folder_name `string` Name of the folder for storing the data set.
    #' @param create_dir `bool` If `True` the directory will be created if it
    #' does not exist.
    #' @return Method does not return anything. It write the data set to disk.
    save = function(dir_path, folder_name, create_dir = TRUE) {
      # Create directory
      if (dir.exists(dir_path) == FALSE) {
        if (create_dir == TRUE) {
          dir.create(dir_path)
        } else {
          stop("Directory does not exist.")
        }
      }

      # Create folder
      save_location <- paste0(dir_path, "/", folder_name)
      if (dir.exists(save_location) == FALSE) {
        dir.create(save_location)
      }

      # Save
      private$data$save_to_disk(dataset_path = save_location)
    },
    #--------------------------------------------------------------------------
    #'@description loads an object of class [LargeDataSetBase] from disk
    #'and updates the object to the current version of the package.
    #'@param dir_path Path where the data set set is stored.
    #'@return Method does not return anything. It loads an object from disk.
    load_from_disk=function(dir_path){
      self$load(dir_path)
    },
    #--------------------------------------------------------------------------
    #' @description Loads a data set from disk.
    #' @param dir_path Path where the data set is stored.
    #' @return Method does not return anything. It loads a data set from disk.
    load = function(dir_path) {
      private$data <- datasets$Dataset$load_from_disk(dataset_path = dir_path)
    },
    #--------------------------------------------------------------------------
    #' @description Return all fields.
    #' @return Method returns a `list` containing all public and private fields
    #' of the object.
    get_all_fields = function() {
      public_list <- NULL
      private_list <- NULL

      for (entry in names(self)) {
        if (is.function(self[[entry]]) == FALSE & is.environment(self[[entry]]) == FALSE) {
          public_list[entry] <- list(self[[entry]])
        }
      }

      for (entry in names(private)) {
        if (is.function(private[[entry]]) == FALSE & is.environment(private[[entry]]) == FALSE) {
          private_list[entry] <- list(private[[entry]])
        }
      }

      return(
        list(
          public = public_list,
          private = private_list
        )
      )
    }
  ),
  private = list(
    data = NULL,
    #--------------------------------------------------------------------------
    add = function(new_dataset) {
      # Check
      check_class(new_dataset, "datasets.arrow_dataset.Dataset", allow_NULL = TRUE)

      if (is.null(private$data)) {
        private$data <- new_dataset
      } else {
        private$data <- datasets$concatenate_datasets(
          list(private$data, new_dataset)
        )
      }
    }
  )
)
