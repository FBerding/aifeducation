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


#' @title Write log
#' @description Function for writing a log file from R containing three rows
#' and three columns. The log file can report the current status of maximal
#' three processes. The first row describes the top process. The second row describes
#' the status of the process within the top process. The third row can be used
#' to describe the status of a process within the middle process.
#'
#' The log can be read with [read_log].
#'
#' @param log_file `string` Path to the file where the log should be saved and updated.
#' @param value_top `double` Current value for the top process.
#' @param total_top `double` Maximal value for the top process.
#' @param message_top `string` Message describing the current state of the top process.
#' @param value_middle `double` Current value for the middle process.
#' @param total_middle `double` Maximal value for the middle process.
#' @param message_middle `string` Message describing the current state of the middle process.
#' @param value_bottom `double` Current value for the bottom process.
#' @param total_bottom `double` Maximal value for the bottom process.
#' @param message_bottom `string` Message describing the current state of the bottom process.
#' @param last_log `POSIXct` Time when the last log was created. If there is no log file set this value to `NULL`.
#' @param write_interval `int` Time in seconds. This time must be past before a new log is created.
#'
#' @return This function writes a log file to the given location. If `log_file` is `NULL` the function
#' will not try to write a log file.
#' @return If `log_file` is a valid path to a file the function will write a log if the time specified by `write_interval` has passed.
#' In addition the function will return an object of class `POSIXct` describing the time when the log file was successfully updated. If the initial attempt
#' for writing log fails the function returns the value of `last_log` which is `NULL` by default.
#'
#' @family log_utils
#' @keywords internal
#' @noRd
#'
write_log <- function(log_file,
                      value_top = 0,
                      value_middle = 0,
                      value_bottom = 0,
                      total_top = 1,
                      total_middle = 1,
                      total_bottom = 1,
                      message_top = NA,
                      message_middle = NA,
                      message_bottom = NA,
                      last_log = NULL,
                      write_interval = 2) {
  if (!is.null(log_file)) {
    if (value_top == total_top ||
      value_middle == total_middle ||
      value_bottom == total_bottom ||
      value_top == 1 ||
      value_middle == 1 ||
      value_bottom == 1) {
      log_data <- rbind(
        c(value_top, total_top, message_top),
        c(value_middle, total_middle, message_middle),
        c(value_bottom, total_bottom, message_bottom)
      )
      colnames(log_data) <- c("value", "total", "message")

      log <- try(write.csv(
        x = log_data,
        file = log_file,
        row.names = FALSE
      ))
      if (!inherits(log,"try-error")) {
        return(Sys.time())
      } else {
        return(last_log)
      }
    } else {
      if (!is.null(last_log)) {
        diff <- difftime(Sys.time(), last_log, units = "secs")[[1]]
        if (diff > write_interval) {
          log_data <- rbind(
            c(value_top, total_top, message_top),
            c(value_middle, total_middle, message_middle),
            c(value_bottom, total_bottom, message_bottom)
          )
          colnames(log_data) <- c("value", "total", "message")

          try(write.csv(x = log_data, file = log_file, row.names = FALSE))
          if (!inherits(log,"try-error")) {
            return(Sys.time())
          } else {
            return(last_log)
          }
        }
      } else {
        log_data <- rbind(
          c(value_top, total_top, message_top),
          c(value_middle, total_middle, message_middle),
          c(value_bottom, total_bottom, message_bottom)
        )
        colnames(log_data) <- c("value", "total", "message")

        try(write.csv(
          x = log_data,
          file = log_file,
          row.names = FALSE
        ))
        if (!inherits(log,"try-error")) {
          return(Sys.time())
        } else {
          return(last_log)
        }
      }
    }
  } else {
    return(NULL)
  }
}

#' Function for reading a log file in R
#'
#' This function reads a log file at the given location. The log file should be
#' created with [write_log].
#'
#' @param file_path `string` Path to the log file.
#'
#' @return Returns a matrix containing the log file.
#'
#' @family log_utils
#' @keywords internal
#' @noRd
#'
read_log <- function(file_path) {
  res <- NULL
  if (!is.null_or_na(file_path)) {
    log_file <- try(read.csv(file_path))
    if (!inherits(log_file,"try-error")) res <- log_file
  }
  return(res)
}

#' Function that resets a log file.
#'
#' This function writes a log file with default values. The file can be read with
#' [read_log].
#'
#' @param log_path `string` Path to the log file.
#'
#' @return Function does nothing return. It is used to write an "empty" log file.
#'
#' @family log_utils
#' @keywords internal
#' @noRd
#'
reset_log <- function(log_path) {
  log_data <- rbind(
    c(0, 1, NA),
    c(0, 1, NA),
    c(0, 1, NA)
  )
  colnames(log_data) <- c("value", "total", "message")

  if (!is.null(log_path)) {
    try(write.csv(x = log_data, file = log_path, row.names = FALSE))
  }
}

#' Function for reading a log file containing a record of the loss during training.
#'
#' This function reads a log file that contains values for every epoch for the loss.
#' The values are grouped for training and validation data. The log contains
#' values for test data if test data was available during training.
#'
#' In general the loss is written by a python function during model's training.
#'
#' @param path_loss `string` Path to the log file.
#' @return Function returns a `matrix` that contains two or three row depending on
#' the data inside the loss log. In the case of two rows the first represents the
#' training data and the second the validation data. In the case of three rows
#' the third row represents the values for test data. All Columns represent the
#' epochs.
#'
#' @family log_utils
#' @keywords internal
#' @noRd
#'
read_loss_log <- function(path_loss) {
  loss_data <- NULL
  if (file.exists(path_loss)) {
    loss_data <- try(
      utils::read.table(
        file = path_loss,
        sep = ",",
        header = FALSE
      ),
      silent = TRUE
    )

    if (!("try-error" %in% class(loss_data))) {
      loss_data <- t(loss_data)
      if (ncol(loss_data) > 2) {
        colnames(loss_data) <- c("train", "validation", "test")
      } else {
        colnames(loss_data) <- c("train", "validation")
      }
      loss_data <- as.data.frame(loss_data)
      for (i in 1:ncol(loss_data)) {
        loss_data[, i] <- as.numeric(loss_data[, i])
      }
      loss_data$epoch <- seq.int(
        from = 1,
        to = nrow(loss_data),
        by = 1
      )
    }
  }
  return(loss_data)
}

#' Reset log for loss information
#'
#' This function writes an empty log file for loss information.
#'
#' @param log_path `string` Path to the log file.
#' @param epochs `int` Number of epochs for the complete training process.
#'
#' @return Function does nothing return. It writes a log file at the given location.
#' The file is a .csv file that contains three rows. The first row takes the
#' value for the training, the second for the validation, and the third row for the
#' test data. The columns represent epochs.
#'
#' @family log_utils
#' @keywords internal
#' @noRd
#'
reset_loss_log <- function(log_path, epochs) {
  log_data <- rbind(
    rep(-100, times = epochs),
    rep(-100, times = epochs),
    rep(-100, times = epochs)
  )

  if (!is.null(log_path)) {
    try(
      utils::write.table(
        x = log_data, file = log_path,
        row.names = FALSE,
        col.names = FALSE,
        sep = ","
      ),
      silent = TRUE
    )
  }
}
