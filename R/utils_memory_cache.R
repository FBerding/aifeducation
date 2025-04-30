
#' @title Temporary directory
#' @description Function getting the path to the directory where 'aifeducation' stores
#' temporary files. If the directory does not exists it will be created.
#'
#'@return Returns a `string` representing the path to the temporary directory.
#'
#' @family dev_memory_cache
#' @keywords internal
#' @noRd
#'
create_and_get_tmp_dir=function(){
  tempdir=paste0(tempdir(),"/r_aifeducation")
  create_dir(dir_path = tempdir,trace=FALSE)
  return(tempdir)
}
