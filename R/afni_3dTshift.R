#' @title Convert AFNI File to NIFTI Image
#' @description Converts an AFNI file to an NIfTI file or \code{nifti} 
#' using \code{3dTshift}
#' @param file AFNI BRIK or HEAD file
#' @param tr Repetition time. You may attach the suffix 's' for seconds,
#' or 'ms' for milliseconds.
#' @param tpattern Pattern of slice time pattern.  
#' alt+z = altplus   = alternating in the plus direction
#' alt+z2            = alternating, starting at slice #1 instead of #0
#' alt-z = altminus  = alternating in the minus direction
#' alt-z2            = alternating, starting at slice #nz-2 instead of #nz-1
#' seq+z = seqplus   = sequential in the plus direction
#' seq-z = seqminus  = sequential in the minus direction
#' @param opts Additional options to pass to \code{3dTshift}
#' @param float Should the \code{-float} option be called in 
#' \code{3dTshift}
#' @param retimg Should an image be returned?
#' @param ... additional arguments to \code{\link{readnii}}
#'
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' the \code{outfile} argument is returned (or a temporary filename)
#' @export
afni_3dTshift = function(
  file,
  tr,
  tpattern= c("alt+z", 
              "alt+z2", 
              "alt-z", 
              "alt-z2", 
              "seq+z", 
              "seq-z"),
  tzero = NULL,
  slice = NULL,
  ignore_vols = 0,
  opts = "",
  retimg = TRUE,
  ...) {
  
  
  func = "3dTshift"
  
  ppaste0 = function(..., x) {
    if (is.null(x)) {
      return("")
    } else {
      return(paste0(..., x))
    }
  }
  tpattern = match.arg(tpattern)
  #############################################
  # Making all the options
  #############################################  
  opts = c(opts, ppaste0("-ignore ", x = ignore_vols))
  opts = c(opts, ppaste0("-TR ", x = tr))
  opts = c(opts, ppaste0("-tpattern ", x = tpattern))
  
  have_tzero = !is.null(tzero)
  have_slice = !is.null(slice)
  non_null = sum(c(have_tzero, have_slice))
  if (non_null > 1) {
    stop("tzero and slice cannot both be specified (non-NULL)")
  }
  
  opts = c(opts, ppaste0("-tzero ", x = tzero))
  opts = c(opts, ppaste0("-slice ", x = slice))
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
  outfile = paste0(outfile, "+orig.BRIK")
  outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  
  return(outfile)
}

#' @rdname afni_3dTshift
#' @export
tshift = function(...) {
  afni_3dTshift(...)  
}

