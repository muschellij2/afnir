
#' @title AFNI 3dWinsor function
#' @description Wrapper for AFNI \code{3dWinsor} function, 
#' which applies a 3D 'Winsorizing' filter.
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param radius include all points within 'distance' in the operation, where distance
#' is defined as sqrt(i*i+j*j+k*k), and (i,j,k) are voxel index offsets
#' @param trim Quantiles for the top and bottom of winsorize filter
#' @param repeat_filter repeat filter nn times.  If nn < 0, means to repeat filter until
#' less than abs(n) voxels change
#' @param keepzero don't filter voxels that are zero
#' @param lower_threshold set voxels at or below \code{lower_threshold} to zero
#' @param mask use a mask dataset - voxels NOT in the mask won't be filtered
#' @param opts Additional options passed to \code{3dWinsor}
#' @param retimg Should a nifti be returned?
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#' 
#'
#' @return Output filename of the image or a \code{nifti} image, depending 
#' on \code{retimg}
#' @export
afni_3dWinsor = function(
  file,
  radius = 1.5,
  trim = c(0.2, 0.8),
  repeat_filter = 1,
  keepzero = FALSE,
  lower_threshold = NULL, # clip
  mask = NULL,
  opts = "",
  retimg = TRUE,
  ...) {
  
  func = "3dWinsor"
  
  ppaste0 = function(..., x) {
    if (is.null(x)) {
      return("")
    } else {
      return(paste0(..., x))
    }
  }
  
  ##############################
  # Checking Trim
  ##############################
  stopifnot(!all(length(trim) %in% c(1, 2)))
  stopifnot(all(trim > 0))
  if (length(trim) == 1) {
    stopifnot(trim <= 0.5)
    trim = c(trim, 1 - trim)
  }
  trim = sort(trim)

  file = checkimg(file)
  suffix = afni_suffix(file, default = "orig")
  
  
  opts = c(opts, ppaste0("-irad ", x = radius))
  opts = c(opts, paste0("-cbot ", trim[1]))
  opts = c(opts, paste0("-ctop ", trim[2]))
  opts = c(opts, ppaste0("-nrep ", x = repeat_filter))
  if (!is.null(mask)) {
    mask = checkimg(mask)
    opts = c(opts, paste0("-mask ", mask))
  }
  opts = c(opts, ifelse(keepzero, "-keepzero ", ""))
  opts = c(opts, ppaste0("-clip ", x = lower_threshold))
  
  
  outfile = tempfile()
  opts = paste0(opts, " -prefix")
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  
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
  attr(outfile, "afni_version") = afni_version()
  
  return(outfile)
}



#' @rdname afni_3dWinsor
#' @export
Winsor = function(...) {
  afni_3dWinsor(...)  
}

#' @rdname afni_3dWinsor
#' @export
winsor = function(...) {
  afni_3dWinsor(...)  
}
