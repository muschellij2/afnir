#' @title AFNI 3dABoverlap function
#' @description Wrapper for AFNI \code{3dABoverlap} function
#'
#' @param file1 nifti object or NIfTI filename for overlap
#' @param file2 nifti object or NIfTI filename for overlap
#' @param automask if \code{TRUE}, \code{automask} is run on the files.  
#' If \code{FALSE}, consider input datasets as masks 
#' (automask does not work on mask datasets)
#' 
#' @return Output filename of the brik
#' @export
afni_3dABoverlap = function(
  file1, #source
  file2,
  automask = TRUE) {
  
  func = "3dABoverlap"
  cmd = get_afni()
  cmd <- paste0(cmd, func)
  
  
  file1 = checkimg(file1)
  file2 = checkimg(file2)
  
  opts = ""
  if (!automask) {
    opts = c(opts, "-no_automask")
  }

  opts = trimws(opts)
  opts = opts[ opts != "" ]
  
  opts = paste(opts, collapse = " ")

  cmd = paste(cmd, opts, file1, file1)
  res = system(cmd, intern = TRUE)
  # res = res[seq(2, length(res))]
  return(res)
}


#' @rdname afni_3dABoverlap
#' @export
ABoverlap = function(...) {
  afni_3dABoverlap(...)  
}

#' @rdname afni_3dABoverlap
#' @export
aboverlap = function(...) {
  afni_3dABoverlap(...)  
}