
#' @title AFNI 3dSkullStrip function
#' @description Wrapper for AFNI \code{3dSkullStrip} function
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param opts Additional options passed to \code{3dSkullStrip}
#' @param retimg Should a nifti be returned?
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#' 
#'
#' @return Output filename of the image or a \code{nifti} image, depending 
#' on \code{retimg}
#' @export
afni_3dSkullStrip = function(
  file,
  opts = "",
  retimg = TRUE,
  ...) {
  
  func = "3dSkullStrip"
  
  file = checkimg(file)
  suffix = afni_suffix(file, default = "orig")
  
  outfile = tempfile()
  opts = paste0(opts, " -prefix")
  opts = trimws(opts)
  
  res = afni_cmd(
    file = file,
    func = func,
    opts = opts,
    frontopts = "-input",
    outfile = outfile,
    add_ext = FALSE,
    quote_outfile = FALSE,
    retimg = FALSE,
    run = TRUE
  )  
  if (res != 0) {
    warning(paste0("Result does not indicate success ", 
                   "- function may not work as expected!"))
  }  
  outfile = paste0(outfile, suffix, ".BRIK")
  outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  return(outfile)
}



#' @rdname afni_3dSkullStrip
#' @export
SkullStrip = function(...) {
  afni_3dSkullStrip(...)  
}
