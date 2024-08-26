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
    if (value_top == total_top |
      value_middle == total_middle |
      value_bottom == total_bottom |
      value_top == 1 |
      value_middle == 1 |
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
      if (class(log) != "try-error") {
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
          if (class(log) != "try-error") {
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
        if (class(log) != "try-error") {
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

read_log <- function(file_path) {
  if (!is.null_or_na(file_path)) {
    log_file <- try(read.csv(file_path))
    if (class(log_file) != "try-error") {
      return(log_file)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

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

read_loss_log <- function(path_loss) {
  if (file.exists(path_loss)) {
    loss_data <- try(
      read.table(
        file = path_loss,
        sep = ",",
        header = FALSE
      ),
      silent = TRUE
    )
    if ("try-error" %in% class(loss_data)) {
      logs_data <- NULL
    } else {
      loss_data <- t(loss_data)
      if (ncol(loss_data) > 2) {
        colnames(loss_data) <- c("train", "validation", "test")
      } else {
        colnames(loss_data) <- c("train", "validation")
      }
      loss_data <- as.data.frame(loss_data)
      for (i in 1:ncol(loss_data)) {
        loss_data[, i] <- as.numeric(loss_data[, i])
        #loss_data[, i] <- replace(
        #  x = loss_data[, i],
        #  list = (loss_data[, i] == -100),
        #  values = NA
        #)
      }
      loss_data$epoch <- seq.int(
        from = 1,
        to = nrow(loss_data),
        by = 1
      )
    }
  } else {
    loss_data <- NULL
  }
  return(loss_data)
}

reset_loss_log <- function(log_path, epochs) {
  log_data <- rbind(
    rep(-100, times = epochs),
    rep(-100, times = epochs),
    rep(-100, times = epochs)
  )

  if (!is.null(log_path)) {
    try(
      write.table(
        x = log_data, file = log_path,
        row.names = FALSE,
        col.names = FALSE,
        sep = ","
      ),
      silent = TRUE
    )
  }
}
