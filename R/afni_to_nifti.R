#' @title Convert AFNI File to NIFTI Image
#'
#' @param file AFNI BRIK or HEAD file
#' @param outfile output filename or prefix.  The default is to always use
#' \code{.nii.gz} extensions and will override if \code{outfile} has a
#' \code{.nii} extension.
#' @param opts Additional options to pass to \code{3dAFNItoNIFTI}
#' @param float Should the \code{-float} option be called in 
#' \code{3dAFNItoNIFTI}
#' @param retimg Should an image be returned?
#' @param ... additional arguments to \code{\link{readnii}}
#'
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' the \code{outfile} argument is returned (or a temporary filename)
#' @export
afni_3dAFNItoNIFTI = function(
  file,
  outfile = NULL,
  opts = "",
  float = TRUE,
  retimg = TRUE,
  ...) {
  func = "3dAFNItoNIFTI"
  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  if (float) {
    opts = paste0(opts, " -float")
  }
  outfile = paste0(nii.stub(outfile), ".nii.gz")
  opts = paste0(opts, " -prefix ", outfile)
  res = afni_cmd(
    file = file,
    func = func,
    frontopts = opts,
    opts = "",
    outfile = NULL,
    samefile = TRUE,
    quote_outfile = FALSE,
    retimg = FALSE
  )
  if (res != 0) {
    warning(paste0("Result does not indicate success ", 
                   "- function may not work as expected!"))
  }
  if (retimg) {
    img = readnii(outfile,
                  ...)
  } else {
    img = outfile
  }
  return(img)
}

#' @export
afni_to_nifti = function(...) {
  afni_3dAFNItoNIFTI(...)  
}
