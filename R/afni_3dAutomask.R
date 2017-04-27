#' @title AFNI 3dAutomask function
#' @description Wrapper for AFNI \code{3dAutomask} function
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param mfrac The 'clip level fraction', which 
#' must be a number between 0.1 and 0.9, which is passed to 
#' \code{\link{afni_3dClipLevel}}
#' @param gradual_clipping The program uses a 'gradual' clip level by default,
#' To use a fixed clip level, set \code{gradual_clipping = FALSE}.
#' @param n_peels Peel the mask \code{n_peels} times, then unpeel.  Designed
#' to clip off protuberances less than 2*\code{n_peels} voxels 
#' thick. 
#' @param n_neighbors  Define the number of neighbors needed for a voxel
#' NOT to be peeled.  The 18 nearest neighbors in
#' the 3D lattice are used, so \code{n_neighbors} should be between
#' 9 and 18.
#' @param outfile Output filename (should not have an extension)
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dAutomask}
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#'
#' @return Output filename of the brik
#' @export
afni_3dAutomask = function(
  file,
  mfrac = 0.5,
  gradual_clipping = TRUE,
  n_peels = 1,
  n_neighbors = 17,
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  ...) {
  func = "3dAutomask"

  file = checkimg(file)
  suffix = afni_suffix(file, default = "orig")
  
  opts = c(opts, paste0("-clfrac ", mfrac))
  opts = c(opts, ifelse(gradual_clipping, "", "-nograd"))

  opts = c(opts, paste0("-peels ", n_peels))
  opts = c(opts, paste0("-nbhrs ", n_neighbors))
  
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  opts = paste0(opts, " -prefix")
  
  brik_outfile = paste0(outfile, suffix, ".BRIK")
  if (file.exists(brik_outfile)) {
    stop(paste0("Dataset name conflicts with existing file,", 
                " delete if overwriting"))
  }
  
  res = afni_cmd(
    file = file,
    func = func,
    opts = opts,
    outfile = outfile,
    samefile = FALSE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    retimg = FALSE,
    quote_file = FALSE
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


#' @rdname afni_3dAutomask
#' @export
Automask = function(...) {
  afni_3dAutomask(...)  
}

#' @rdname afni_3dAutomask
#' @export
automask = function(...) {
  afni_3dAutomask(...)  
}