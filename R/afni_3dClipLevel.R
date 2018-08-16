#' @title AFNI 3dClipLevel function
#' @description Estimates the value at which to clip the anatomical dataset using
#' \code{3dClipLevel} so that background regions are set to zero.
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param mfrac Multiplier of the median for clipping (see notes)
#' @note From AFNI docs:
#' Algorithm:
#' 
#' (a) Set some initial clip value using wizardry (AKA 'variance').
#' 
#' (b) Find the median of all positive values >= clip value.
#' 
#' (c) Set the clip value to \code{mfrac} of this median.
#' 
#' (d) Loop back to (b) until the clip value doesn't change.
#' @param opts Additional options passed to \code{3dClipLevel}
#' @param ... not used
#'
#' @return Single character string of a nubmer 
#' (by default), unless \code{opts = "-doall"}
#' @export
afni_3dClipLevel = function(
  file,
  mfrac = 0.5,
  opts = "") {
  func = "3dClipLevel"
  
  
  file = checkimg(file, allow_array = FALSE)
  opts = c(opts, paste0("-mfrac ", mfrac))
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  

  res = afni_cmd(
    file = file,
    func = func,
    opts = opts,
    outfile = NULL,
    samefile = TRUE,
    intern = TRUE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    retimg = FALSE,
    quote_file = FALSE
  ) 
  attr(res, "afni_version") = afni_version()
  
  return(res)
}


#' @rdname afni_3dClipLevel
#' @export
ClipLevel = function(...) {
  afni_3dClipLevel(...)  
}
