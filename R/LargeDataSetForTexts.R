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

#' @title Abstract class for large data sets containing raw texts
#' @description This object stores raw texts. The data of this objects is not stored in memory directly. By using memory
#'   mapping these objects allow to work with data sets which do not fit into memory/RAM.
#'
#' @return Returns a new object of this class.
#' @export
#' @family Data Management
LargeDataSetForText <- R6::R6Class(
  classname = "LargeDataSetForText",
  inherit = LargeDataSetBase,
  public = list(
    #--------------------------------------------------------------------------
    #' @description Method for creation of [LargeDataSetForText] instance. It can be initialized with `init_data`
    #'   parameter if passed (Uses `add_from_data.frame()` method if `init_data` is `data.frame`).
    #' @param init_data Initial `data.frame` for dataset.
    #' @return A new instance of this class initialized with `init_data` if passed.
    initialize = function(init_data = NULL) {
      if (is.data.frame(init_data)) {
        self$add_from_data.frame(init_data)
      }
    },
    #--------------------------------------------------------------------------
    #' @description Method for adding raw texts saved within .txt files to the data set. Please note the the directory
    #'   should contain one folder for each .txt file. In order to create an informative data set every folder can
    #'   contain the following additional files:
    #'
    #' * bib_entry.txt: containing a text version of the bibliographic information of the raw text.
    #' * license.txt: containing a statement about the license to use the raw text such as "CC BY".
    #' * url_license.txt: containing the url/link to the license in the internet.
    #' * text_license.txt: containing the license in raw text.
    #' * url_source.txt: containing the url/link to the source in the internet.
    #'
    #'   The id of every .txt file is the file name without file extension. Please be aware to provide unique file
    #'   names. Id and raw texts are mandatory, bibliographic and license information are optional.
    #' @param dir_path Path to the directory where the files are stored.
    #' @param batch_size `int` determining the number of files to process at once.
    #' @param log_file `string` Path to the file where the log should be saved. If no logging is desired set this
    #'   argument to `NULL`.
    #' @param log_write_interval `int` Time in seconds determining the interval in which the logger should try to update
    #'   the log files. Only relevant if `log_file` is not `NULL`.
    #' @param log_top_value `int` indicating the current iteration of the process.
    #' @param log_top_total `int` determining the maximal number of iterations.
    #' @param log_top_message `string` providing additional information of the process.
    #' @param trace `bool` If `TRUE` information on the progress is printed to the console.
    #' @return The method does not return anything. It adds new raw texts to the data set.
    add_from_files_txt = function(dir_path,
                                  batch_size = 500,
                                  log_file = NULL,
                                  log_write_interval = 2,
                                  log_top_value = 0,
                                  log_top_total = 1,
                                  log_top_message = NA,
                                  trace = TRUE) {
      # Gather all text files
      file_paths <- private$get_file_paths(dir_path, ".txt")

      if (length(file_paths) > 0) {
        # calculate number of batches
        n_batches <- ceiling(length(file_paths) / batch_size)

        # get indices for every batch
        batches <- get_batches_index(
          number_rows = length(file_paths),
          batch_size = batch_size
        )

        # Process every batch
        list_datasets <- list()
        last_log <- NULL
        for (i in 1:n_batches) {
          chunk <- private$get_batch(batches[[i]],
            file_paths = file_paths,
            clean_text = TRUE
          )
          chunk_dataset <- data.frame_to_py_dataset(chunk)
          list_datasets[i] <- list(chunk_dataset)
          if (trace == TRUE) {
            message(paste(
              date(),
              "Batch", i, "from", n_batches, "processed"
            ))
          }
          last_log <- write_log(
            log_file = log_file,
            last_log = last_log,
            write_interval = log_write_interval,
            value_top = log_top_value,
            value_middle = i,
            value_bottom = 0,
            total_top = log_top_total,
            total_middle = n_batches,
            total_bottom = 1,
            message_top = log_top_message,
            message_middle = ".txt files",
            message_bottom = NA
          )
        }

        # concatenate datasets
        new_dataset <- datasets$concatenate_datasets(dsets = list_datasets, axis = 0L)

        # Add new dataset
        private$add(new_dataset)
      }
    },

    #--------------------------------------------------------------------------
    #' @description Method for adding raw texts saved within .pdf files to the data set. Please note the the directory
    #'   should contain one folder for each .pdf file. In order to create an informative data set every folder can
    #'   contain the following additional files:
    #'
    #' * bib_entry.txt: containing a text version of the bibliographic information
    #'   of the raw text.
    #' * license.txt: containing a statement about the license to use the raw text
    #'   such as "CC BY".
    #' * url_license.txt: containing the url/link to the license in the internet.
    #' * text_license.txt: containing the license in raw text.
    #' * url_source.txt: containing the url/link to the source in the internet.
    #'
    #'   The id of every .pdf file is the file name without file extension. Please be aware to provide unique file
    #'   names. Id and raw texts are mandatory, bibliographic and license information are optional.
    #' @param dir_path Path to the directory where the files are stored.
    #' @param batch_size `int` determining the number of files to process at once.
    #' @param trace `bool` If `TRUE` information on the progress is printed to the console.
    #' @param log_top_value `int` indicating the current iteration of the process.
    #' @param log_top_total `int` determining the maximal number of iterations.
    #' @param log_top_message `string` providing additional information of the process.
    #' @param log_file `string` Path to the file where the log should be saved. If no logging is desired set this
    #'   argument to `NULL`.
    #' @param log_write_interval `int` Time in seconds determining the interval in which the logger should try to update
    #'   the log files. Only relevant if `log_file` is not `NULL`.
    #' @return The method does not return anything. It adds new raw texts to the data set.
    add_from_files_pdf = function(dir_path,
                                  batch_size = 500,
                                  log_file = NULL,
                                  log_write_interval = 2,
                                  log_top_value = 0,
                                  log_top_total = 1,
                                  log_top_message = NA,
                                  trace = TRUE) {
      # Gather all files
      file_paths <- private$get_file_paths(dir_path, ".pdf")
      if (length(file_paths) > 0) {
        # calculate number of batches
        n_batches <- ceiling(length(file_paths) / batch_size)

        # get indices for every batch
        batches <- get_batches_index(
          number_rows = length(file_paths),
          batch_size = batch_size
        )

        # Process every batch
        list_datasets <- list()
        last_log <- NULL

        for (i in 1:n_batches) {
          chunk <- private$get_batch(batches[[i]],
            file_paths = file_paths,
            clean_text = TRUE
          )
          chunk_dataset <- data.frame_to_py_dataset(chunk)
          list_datasets[i] <- list(chunk_dataset)
          if (trace == TRUE) {
            message(paste(
              date(),
              "Batch", i, "from", n_batches, "processed"
            ))
          }
          last_log <- write_log(
            log_file = log_file,
            last_log = last_log,
            write_interval = log_write_interval,
            value_top = log_top_value,
            value_middle = i,
            value_bottom = 0,
            total_top = log_top_total,
            total_middle = n_batches,
            total_bottom = 1,
            message_top = log_top_message,
            message_middle = ".pdf files",
            message_bottom = NA
          )
        }

        # concatenate datasets
        new_dataset <- datasets$concatenate_datasets(dsets = list_datasets, axis = 0L)

        # Add new dataset
        private$add(new_dataset)
      }
    },

    #--------------------------------------------------------------------------
    #' @description Method for adding raw texts saved within .xlsx files to the data set. The method assumes that the
    #'   texts are saved in the rows and that the columns store the id and the raw texts in the columns. In addition, a
    #'   column for the bibliography information and the license can be added. The column names for these rows must be
    #'   specified with the following arguments. They must be the same for all .xlsx files in the chosen directory. Id
    #'   and raw texts are mandatory, bibliographic, license, license's url, license's text, and source's url are
    #'   optional. Additional columns are dropped.
    #' @param dir_path Path to the directory where the files are stored.
    #' @param id_column `string` Name of the column storing the ids for the texts.
    #' @param text_column `string` Name of the column storing the raw text.
    #' @param bib_entry_column `string` Name of the column storing the bibliographic information of the texts.
    #' @param license_column `string` Name of the column storing information about the licenses.
    #' @param url_license_column `string` Name of the column storing information about the url to the license in the
    #'   internet.
    #' @param text_license_column `string` Name of the column storing the license as text.
    #' @param url_source_column `string` Name of the column storing information about about the url to the source in the
    #'   internet.
    #' @param trace `bool` If `TRUE` prints information on the progress to the console.
    #' @param log_top_value `int` indicating the current iteration of the process.
    #' @param log_top_total `int` determining the maximal number of iterations.
    #' @param log_top_message `string` providing additional information of the process.
    #' @param log_file `string` Path to the file where the log should be saved. If no logging is desired set this
    #'   argument to `NULL`.
    #' @param log_write_interval `int` Time in seconds determining the interval in which the logger should try to update
    #'   the log files. Only relevant if `log_file` is not `NULL`.
    #' @return The method does not return anything. It adds new raw texts to the data set.
    add_from_files_xlsx = function(dir_path,
                                   trace = TRUE,
                                   id_column = "id",
                                   text_column = "text",
                                   bib_entry_column = "bib_entry",
                                   license_column = "license",
                                   url_license_column = "url_license",
                                   text_license_column = "text_license",
                                   url_source_column = "url_source",
                                   log_file = NULL,
                                   log_write_interval = 2,
                                   log_top_value = 0,
                                   log_top_total = 1,
                                   log_top_message = NA) {
      # Check
      check_type(id_column, "string", FALSE)
      check_type(text_column, "string", FALSE)
      check_type(bib_entry_column, "string", TRUE)
      check_type(license_column, "string", TRUE)
      check_type(url_license_column, "string", TRUE)
      check_type(text_license_column, "string", TRUE)
      check_type(url_source_column, "string", TRUE)
      check_type(trace, "bool", FALSE)
      check_type(dir_path, "string", FALSE)

      # Gather all files
      file_paths <- private$get_file_paths(dir_path, file_type = ".xlsx")
      n_batches <- length(file_paths)

      # Process every batch
      list_datasets <- list()
      last_log <- NULL
      for (i in 1:n_batches) {
        chunk <- readtext::readtext(
          file = file_paths[i],
          docid_field = id_column,
          text_field = text_column,
          docvarsfrom = "metadata"
        )
        if (nrow(chunk) < 2) {
          chunk <- readtext::readtext(
            file = file_paths[i],
            docid_field = NULL,
            text_field = text_column,
            docvarsfrom = "metadata"
          )
        } else {
          # Set correct name of id column
          index <- which(colnames(chunk) %in% "doc_id")
          colnames(chunk)[index] <- "id"
        }

        # Bib_entry column
        index <- which(colnames(chunk) %in% bib_entry_column)
        if (length(index) == 0) {
          chunk$bib_entry <- rep(NA_character_, nrow(chunk))
        } else {
          colnames(chunk)[index] <- "bib_entry"
        }

        # License column
        index <- which(colnames(chunk) %in% license_column)
        if (length(index) == 0) {
          chunk$license <- rep(NA_character_, nrow(chunk))
        } else {
          colnames(chunk)[index] <- "license"
        }

        # URL License column
        index <- which(colnames(chunk) %in% url_license_column)
        if (length(index) == 0) {
          chunk$url_license <- rep(NA_character_, nrow(chunk))
        } else {
          colnames(chunk)[index] <- "url_license"
        }

        # Text License column
        index <- which(colnames(chunk) %in% text_license_column)
        if (length(index) == 0) {
          chunk$text_license <- rep(NA_character_, nrow(chunk))
        } else {
          colnames(chunk)[index] <- "text_license"
        }

        # URL source column
        index <- which(colnames(chunk) %in% url_source_column)
        if (length(index) == 0) {
          chunk$url_source <- rep(NA_character_, nrow(chunk))
        } else {
          colnames(chunk)[index] <- "url_source"
        }

        # Select only the necessary columns
        chunk <- chunk[c(
          "id",
          "text",
          "bib_entry",
          "license",
          "url_license",
          "text_license",
          "url_source"
        )]

        chunk_dataset <- data.frame_to_py_dataset(chunk)

        list_datasets[i] <- list(chunk_dataset)
        if (trace == TRUE) {
          message(paste(
            date(),
            "Batch", i, "from", n_batches, "processed"
          ))
        }
        last_log <- write_log(
          log_file = log_file,
          last_log = last_log,
          write_interval = log_write_interval,
          value_top = log_top_value,
          value_middle = i,
          value_bottom = 0,
          total_top = log_top_total,
          total_middle = n_batches,
          total_bottom = 1,
          message_top = log_top_message,
          message_middle = ".xlsx files",
          message_bottom = NA
        )

        # concatenate datasets
        new_dataset <- datasets$concatenate_datasets(dsets = list_datasets, axis = 0L)

        # Add new dataset
        private$add(new_dataset)
      }
    },

    #--------------------------------------------------------------------------
    #' @description Method for adding raw texts from a `data.frame`
    #' @param data_frame Object of class `data.frame` with at least the following columns "id","text","bib_entry",
    #'   "license", "url_license", "text_license", and "url_source". If "id" and7or "text" is missing an error occurs.
    #'   If the other columns are not present in the `data.frame` they are added with empty values(`NA`).
    #'   Additional columns are dropped.
    #' @return The method does not return anything. It adds new raw texts to the data set.
    add_from_data.frame = function(data_frame) {
      # Necessary columns
      if (is.data.frame(data_frame) == FALSE) {
        stop("Input must be of type data.frame")
      }
      if ("id" %in% colnames(data_frame) == FALSE) {
        stop("data.frame must contain a column id.")
      }
      if ("text" %in% colnames(data_frame) == FALSE) {
        stop("data.frame must contain a column text.")
      }

      data_to_add <- data_frame

      # optional columns
      if ("bib_entry" %in% colnames(data_to_add) == FALSE) {
        data_to_add["bib_entry"] <- rep(x = NA, times = nrow(data_to_add))
      }
      if ("license" %in% colnames(data_to_add) == FALSE) {
        data_to_add["license"] <- rep(x = NA, times = nrow(data_to_add))
      }
      if ("url_license" %in% colnames(data_to_add) == FALSE) {
        data_to_add["url_license"] <- rep(x = NA, times = nrow(data_to_add))
      }
      if ("text_license" %in% colnames(data_to_add) == FALSE) {
        data_to_add["text_license"] <- rep(x = NA, times = nrow(data_to_add))
      }
      if ("url_source" %in% colnames(data_to_add) == FALSE) {
        data_to_add["url_source"] <- rep(x = NA, times = nrow(data_to_add))
      }

      # Transform to a python dataset
      new_dataset <- data.frame_to_py_dataset(data_to_add[c(
        "id",
        "text",
        "bib_entry",
        "license",
        "url_license",
        "text_license",
        "url_source"
      )])

      # Add new dataset
      private$add(new_dataset)
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting all private fields and methods. Used for loading and updating an object.
    #' @return Returns a `list` with all private fields and methods.
    get_private = function() {
      return(private)
    }
  ),
  private = list(
    get_file_paths = function(dir_path, file_type) {
      file_paths <- list.files(
        path = dir_path,
        include.dirs = FALSE,
        all.files = TRUE,
        full.names = TRUE,
        recursive = TRUE,
        pattern = paste0("*", file_type)
      )
      if (length(file_paths) > 0) {
        file_paths <- private$clean_path(file_paths)
      }
      return(file_paths)
    },
    clean_path = function(paths) {
      new_paths <- vector(length = length(paths))
      new_paths[] <- NA
      for (i in seq_len(length(paths))) {
        path <- paths[i]
        bib_entry_path <- paste0(
          dirname(path), "/bib_entry.txt"
        )
        licence_path <- paste0(
          dirname(path), "/license.txt"
        )
        license_text_path <- paste0(
          dirname(path), "/text_license.txt"
        )
        url_licence_path <- paste0(
          dirname(path), "/url_license.txt"
        )
        url_source_path <- paste0(
          dirname(path), "/url_source.txt"
        )
        path_list <- c(bib_entry_path, licence_path, license_text_path, url_licence_path, url_source_path)
        if (path %in% path_list == FALSE) {
          new_paths[i] <- path
        }
      }
      return(na.omit(new_paths))
    },
    get_batch = function(batch, file_paths, clean_text = TRUE) {
      col_names <- c(
        "id",
        "text",
        "bib_entry",
        "license",
        "url_license",
        "text_license",
        "url_source"
      )
      data <- matrix(
        data = NA,
        nrow = length(batch),
        ncol = length(col_names)
      )
      colnames(data) <- col_names

      for (i in seq_len(length(batch))) {
        index <- batch[i]
        document <- readtext::readtext(file = file_paths[index])

        # ID
        data[i, 1] <- private$remove_file_extenstion(document$doc_id)

        # Text
        if (clean_text == TRUE) {
          text <- private$clean_text(document$text)
        } else {
          text <- document$text
        }
        data[i, 2] <- text

        # Bib_entry
        file_path <- paste0(dirname(file_paths[index]), "/bib_entry.txt")
        if (file.exists(file_path) == TRUE) {
          data[i, 3] <- readLines(con = file_path, warn = FALSE)
        } else {
          data[i, 3] <- NA
        }

        # License
        file_path <- paste0(dirname(file_paths[index]), "/license.txt")
        if (file.exists(file_path) == TRUE) {
          data[i, 4] <- readLines(con = file_path, warn = FALSE)
        } else {
          data[i, 4] <- NA
        }

        # URL License
        file_path <- paste0(dirname(file_paths[index]), "/url_license.txt")
        if (file.exists(file_path) == TRUE) {
          data[i, 5] <- readLines(con = file_path, warn = FALSE)
        } else {
          data[i, 5] <- NA
        }

        # Text License
        file_path <- paste0(dirname(file_paths[index]), "/text_license.txt")
        if (file.exists(file_path) == TRUE) {
          data[i, 6] <- readLines(con = file_path, warn = FALSE)
        } else {
          data[i, 6] <- NA
        }

        # URL Source
        file_path <- paste0(dirname(file_paths[index]), "/url_source.txt")
        if (file.exists(file_path) == TRUE) {
          data[i, 7] <- readLines(con = file_path, warn = FALSE)
        } else {
          data[i, 7] <- NA
        }
      }
      return(as.data.frame(data))
    },
    clean_text = function(text) {
      # text <- stringr::str_replace_all(text, pattern = "[:space:]{1,}", replacement = " ")
      # text <- stringr::str_replace_all(text, pattern = "-(?=[:space:])", replacement = "")
      text <- stringi::stri_replace_all(text, regex = "[:space:]{1,}", replacement = " ")
      text <- stringi::stri_replace_all(text, regex = "-(?=[:space:])", replacement = "")
      return(text)
    },
    remove_file_extenstion = function(file) {
      # tmp_string <- stringr::str_split_fixed(file, pattern = "\\.", n = Inf)
      tmp_string <- stringi::stri_split_fixed(file, pattern = ".", n = -1, simplify = TRUE)
      return(paste0(tmp_string[1, 1:(ncol(tmp_string) - 1)], collapse = "."))
    }
  )
)
