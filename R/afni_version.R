#' @title GET AFNI Version
#' @description Finds the AFNI version from \code{anfni -ver}
#' 
#' @return Character vector of the version.
#' @export
#' @examples 
#' if (have_afni()) {
#'  afni_version()
#' }
afni_version = function(){
  
  cmd = get_afni()
  cmd = paste0(cmd, "afni -ver")
  version = system(cmd, intern = TRUE)
  return(version)
}

#' @rdname afni_version
#' @aliases afniversion
#' @export
afniversion = function(){
  return(afni_version())
}
