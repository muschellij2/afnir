#' @title Perform Volume Registration for Motion Correction
#' @description Performs Volume Registration for Motion Correction on 
#' an NIfTI file or \code{nifti} 
#' using \code{3dvolreg}
#' @param file file or \code{nifti} object
#' @param interpolation interpolation done after realignment.
#' @param rp_file filename of motion parameter output, which should have columns:
#' roll  = rotation about the I-S axis (degrees)
#' 
#' pitch = rotation about the R-L axis,  (degrees) CCW
#' 
#' yaw   = rotation about the A-P axis  (degrees)
#' 
#' dS  = displacement in the Superior direction (mm)
#' 
#' dL  = displacement in the Left direction (mm)
#' 
#' dP  = displacement in the Posterior direction (mm)
#' @param full_motion_file filename of full motion parameter output, including fields
#' as in rp file, but also brik indices and root mean squares
#' @param opts Additional options to pass to \code{3dvolreg}
#' @param retimg Should an image be returned?
#' @param ... additional arguments to \code{\link{readnii}}
#'
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' the \code{outfile} argument is returned (or a temporary filename)
#' @export
afni_3dvolreg = function(
  file,
  interpolation = c("Fourier", 
              "heptic", 
              "quintic", 
              "cubic", 
              "linear"),
  rp_file = NULL,
  full_motion_file = NULL,
  opts = "",
  retimg = TRUE,
  ...) {
  
  
  func = "3dvolreg"
  file = checkimg(file)
  suffix = afni_suffix(file, default = "orig")
  
  ppaste0 = function(..., x) {
    if (is.null(x)) {
      return("")
    } else {
      return(paste0(..., x))
    }
  }
  interpolation = match.arg(interpolation)
  interpolation = paste0("-", interpolation)
  
  if (is.null(rp_file)) {
    rp_file = tempfile(fileext = ".txt")
  }
  opts = c(opts, ppaste0("-1Dfile ", x = rp_file))
  
  if (is.null(full_motion_file)) {
    full_motion_file = tempfile(fileext = ".txt")
  }
  opts = c(opts, ppaste0("-dfile ", x = full_motion_file))  
  
  
  #############################################
  # Making all the options
  #############################################  
  opts = c(opts, interpolation)
  opts = opts[ opts != "" ]
  #############################################
  # end of options
  #############################################
  
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
  attr(outfile, "rp_file") = rp_file
  attr(outfile, "motion_file") = full_motion_file
  
  return(outfile)
}

#' @rdname afni_3dvolreg
#' @export
volreg = function(...) {
  afni_3dvolreg(...)  
}

