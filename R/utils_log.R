write_log <- function(log_file,
                      value_top = 0, total_top = 1, message_top = NA,
                      value_middle = 0, total_middle = 1, message_middle = NA,
                      value_bottom = 0, total_bottom = 1, message_bottom = NA,
                      last_log = NULL, write_interval = 2) {
  if (is.null(log_file)) {
    return(NULL)
  }

  try_write_log_data <- function() {
    log_data <- rbind(
      c(value_top, total_top, message_top),
      c(value_middle, total_middle, message_middle),
      c(value_bottom, total_bottom, message_bottom)
    )
    colnames(log_data) <- c("value", "total", "message")

    log <- try(write.csv(x = log_data, file = log_file, row.names = FALSE))

    if (!inherits(log, "try-error")) {
      return(Sys.time())
    } else {
      return(last_log)
    }
  }

  if (value_top %in% c(1, total_top) ||
      value_middle %in% c(1, total_middle) ||
      value_bottom %in% c(1, total_bottom)) {
    # if this is the first or last iteration
    return(try_write_log_data())
  } else {
    if (!is.null(last_log)) {
      diff <- difftime(Sys.time(), last_log, units = "secs")[[1]]
      if (diff > write_interval) {
        return(try_write_log_data())
      }
    } else {
      return(try_write_log_data())
    }
  }
}

read_log <- function(file_path) {
  res <- NULL
  if (!is.null_or_na(file_path)) {
    log_file <- try(read.csv(file_path))
    if (!inherits(log_file, "try-error")) res <- log_file
  }
  return(res)
}

reset_log <- function(log_path) {
  if (is.null(log_path)) {
    return(invisible())
  }

  log_data <- rbind(
    c(0, 1, NA),
    c(0, 1, NA),
    c(0, 1, NA)
  )
  colnames(log_data) <- c("value", "total", "message")

  try(write.csv(x = log_data, file = log_path, row.names = FALSE))
}

read_loss_log <- function(path_loss) {
  if (!file.exists(path_loss)) {
    return(NULL)
  }

  loss_data <- try(
    utils::read.table(file = path_loss, sep = ",", header = FALSE),
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

  return(loss_data)
}

reset_loss_log <- function(log_path, epochs) {
  if (is.null(log_path)) {
    return(invisible())
  }

  log_data <- rbind(
    rep(-100, times = epochs),
    rep(-100, times = epochs),
    rep(-100, times = epochs)
  )

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
