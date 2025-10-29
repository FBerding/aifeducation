#' @title Temporary directory
#' @description Function getting the path to the directory where 'aifeducation' stores
#' temporary files. If the directory does not exists it will be created.
#'
#' In general this folder is stored at the position which can be requested with `tempdir`.
#' On continous integration it will use the path provides with `testthat::test_path`
#'
#' @return Returns a `string` representing the path to the temporary directory.
#'
#' @family dev_memory_cache
#' @keywords internal
#' @noRd
#'
create_and_get_tmp_dir <- function() {
  if (Sys.getenv("CI") == "true") {
    requireNamespace("testthat")
    temp_dir <- file.path(testthat::test_path(), "r_aifeducation")
    create_dir(dir_path = temp_dir, trace = FALSE)
  } else {
    temp_dir <- file.path(tempdir(), "r_aifeducation")
  }
  create_dir(dir_path = temp_dir, trace = FALSE)
  return(temp_dir)
}

#' @title Clean Temporary directory
#' @description Function deleting all files stored in the temporary folder of 'aifeducation'.
#'
#' @return Returns nothing. It used to clean temporary files.
#'
#' @family dev_memory_cache
#' @keywords internal
#' @noRd
#'
clean_tmp_dir <- function() {
  temp_dir <- create_and_get_tmp_dir()
  if (dir.exists(temp_dir)) {
    unlink(x = temp_dir)
  } else {
    message(tempdir, " does not exist.")
  }
}

#' @title Inspect Temporary directory
#' @description Function reporting the number of files and the cumulative size
#' of the files in temporary directory.
#'
#' @return Returns a `list` containing a `vector` with the paths of all files in
#' the temporary directory and the cumulative file size in bytes.
#'
#' @family Memory Cache
#' @export
#'
inspect_tmp_dir <- function() {
  tmp_dir <- create_and_get_tmp_dir()
  file_list <- list.files(
    path=tmp_dir,
    all.files=TRUE,
    full.names=TRUE,
    recursive=TRUE
    )
  print(file_list)
  n_files <- length(file_list)
  cum_file_size <- 0L
  if (n_files > 0L) {
    for (file in file_list) {
      cum_file_size <- cum_file_size + file.size(file)
    }
  } else {
    file_list <- NULL
  }
  results=list(
    files=file_list,
    cum_size=cum_file_size
  )
  message(n_files, " files with ", format_size(cum_file_size))
  return(results)
}

#' @title Format number of bytes
#' @description Function formats an `int` representing the number of bytes
#' into another unit (e.g. kb, mb, gb).
#' @param  size `int` Size in bytes.
#'
#' @return Returns a `string` representing the size in bytes in another unit.
#'
#' @family dev_memory_cache
#' @keywords internal
#' @noRd
#'
format_size<-function(size){
  if(size>=10^12){
    return(paste(size/(10^12),"TB"))
  } else if(size<10^12 & size>=10^9){
    return(paste(round(size/(10^9),digits=3),"GB"))
  } else if(size<10^9 & size>=10^6){
    return(paste(round(size/(10^6),digits=3),"MB"))
  } else if(size<10^6 & size>=10^3){
    return(paste(round(size/(10^3),digits=3),"KB"))
  } else if(size<10^3 & size>=0){
    return(paste(round(size,digits=3),"Byte"))
  }
}
