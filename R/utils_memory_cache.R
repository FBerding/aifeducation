
#' @title Temporary directory
#' @description Function getting the path to the directory where 'aifeducation' stores
#' temporary files. If the directory does not exists it will be created.
#'
#' In general this folder is stored at the position which can be requested with `tempdir`.
#' On continous integration it will use the path provides with `testthat::test_path`
#'
#'@return Returns a `string` representing the path to the temporary directory.
#'
#' @family dev_memory_cache
#' @keywords internal
#' @noRd
#'
create_and_get_tmp_dir=function(){
  if(Sys.getenv("CI") == "true"){
    requireNamespace("testthat")
    temp_dir=paste0(testthat::test_path(),"/r_aifeducation")
    create_dir(dir_path = temp_dir,trace=FALSE)
  } else {
    temp_dir=paste0(tempdir(),"/r_aifeducation")
  }
  create_dir(dir_path = temp_dir,trace=FALSE)
  return(temp_dir)
}

#' @title Clean Temporary directory
#' @description Function deleting all files stored in the temporary folder of 'aifeducation'.
#'
#'@return Returns nothing. It used to clean temporary files.
#'
#' @family dev_memory_cache
#' @keywords internal
#' @noRd
#'
clean_tmp_dir=function(){
  temp_dir=create_and_get_tmp_dir()
  if(dir.exists(temp_dir)){
    unlink(x=temp_dir)
  } else {
    message(paste(tempdir,"does not exist."))
  }
}
