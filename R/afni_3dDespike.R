#' @title Convert AFNI File to NIFTI Image
#' @description Converts an AFNI file to an NIfTI file or \code{nifti} 
#' using \code{3dDespike}
#'
#' @param file AFNI BRIK or HEAD file
#' @param opts Additional options to pass to \code{3dDespike}
#' @param retimg Should an image be returned?
#' @param ignore_vols Number of volumes to ignore/delete
#' @param curve_order the curve fit order 
#' @param cut_values a length 2 vector with the first being
#' the threshold value of sigma for a spike and the second being 
#' the upper range of the allowed deviation from the curve:
#' @param mask should a mask of high-intensity voxels be used (\code{TRUE}) or 
#' all voxels (\code{FALSE})
#' @param dilate dilation factor of the mask
#' @param localedit Change the editing process to the following:
#' If a voxel |s| value is >= c2, then replace
#' the voxel value with the average of the two
#' nearest non-spike (|s| < c2) values; the first
#' one previous and the first one after.
#' Note that the first cut value is not used here.
#' @param method should the NEW or OLD method be used for the L1 fit.  NEW is 
#' faster
#' @param ... additional arguments to \code{\link{readnii}}
#'
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' the \code{outfile} argument is returned (or a temporary filename)
#' @export
afni_3dDespike = function(
  file,
  ignore_vols = 0,
  curve_order = NULL,
  cut_values = c(2.5, 4.0),
  mask = TRUE,
  dilate = 4,
  localedit = FALSE,
  method = c("OLD", "NEW"),
  opts = "",
  retimg = TRUE,
  ...) {
  
  
  func = "3dDespike"
  
  ppaste0 = function(..., x) {
    if (is.null(x)) {
      return("")
    } else {
      return(paste0(..., x))
    }
  }
  #############################################
  # Making all the options
  #############################################  
  opts = c(opts, ppaste0("-ignore ", x = ignore_vols))
  opts = c(opts, ppaste0("-corder ", x = curve_order))
  stopifnot(length(cut_values) == 2)
  opts = c(opts, ppaste0("-cut ", x = paste(cut_values, collapse = " ")))
  if (mask) {
    mask = ""
  } else {
    mask = "-nomask"
  }
  opts = c(opts, mask)
  opts = c(opts, ppaste0("-dilate ", x = dilate))
  if (localedit) {
    localedit = ""
  } else {
    localedit = "-localedit"
  }
  opts = c(opts, localedit)
  method = match.arg(method)
  method = paste0("-", method)
  opts = c(opts, method)
  opts = opts[ opts != "" ]
  #############################################
  # end of options
  #############################################
  
  file = checkimg(file)
  suffix = afni_suffix(file, default = "orig")
  
  outfile = tempfile(fileext = "")
  opts = c(opts, paste0("-prefix ", outfile))
  opts = paste(opts, collapse = " ")
  
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
  outfile = paste0(outfile, suffix, ".BRIK")
  outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  
  return(outfile)
}

#' @rdname afni_3dDespike
#' @export
despike = function(...) {
  afni_3dDespike(...)  
}
