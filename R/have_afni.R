#' @title Logical check if AFNI is accessible
#' @description Uses \code{get_afni} to check if AFNIDIR is accessible or the option
#' \code{afni.path} is set and returns logical
#' @param ... options to pass to \code{\link{get_afni}}
#' @return Logical TRUE is AFNI is accessible, FALSE if not
#' @export
#' @examples
#' have_afni()
have_afni = function(...){
  x = suppressWarnings(try(get_afni(...), silent = TRUE))
  return(!inherits(x, "try-error"))
}

