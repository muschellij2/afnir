#' @title AFNI 3dQwarp function
#' @description Wrapper for AFNI \code{3dQwarp} function
#'
#' @param file nifti object or NIfTI filename to register to \code{fixed} image
#' @param fixed nifti object or NIfTI filename of fixed image, target of registration 
#' @param outfile Output filename (should not have an extension)
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dQwarp}
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#' @param align Should \code{\link{afni_3dAllineate}} be run for an affine alignment
#' done before warping?
#' @param align_opts Options for alignment, in a single string.
#' @param resample This option simply resamples the file dataset to match the
#' fixed dataset grid.  You can use this if the two datasets
#' overlap well (as seen in the AFNI GUI), but are not on the
#' same 3D grid.
#' @param blur Gaussian blur the input images (specified in FWHM!) voxels before
#' doing the alignment (the output dataset will not be blurred).
#' The default is 2.345 (for no good reason).
#'
#' @return Output filename of the brik
#' @export
afni_3dQwarp = function(
  file, #source
  fixed, # base
  align = TRUE,
  align_opts = "",
  resample = FALSE,
  blur = 2.345,
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  ...) {
  
  func = "3dQwarp"
  
  file = checkimg(file)
  suffix = afni_suffix(file, default = "tlrc")
  
  fixed = checkimg(fixed)
  
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
  opts = c(opts, ppaste0("-base ", x = fixed))

  opts = c(opts, ifelse(align, "-allineate", ""))
  if (align) {
    align_opts = afni_quote_expression(align_opts)
    align_opts = align_opts[ align_opts != "" ]
    if (length(align_opts) > 0) {
      align_opts = paste0("-allineate_opts ", align_opts)
      opts = c(opts, ifelse(align, align_opts, ""))
    }
  }
  opts = c(opts, ppaste0("-blur ", x = blur))
  opts = c(opts, ifelse(resample, "-resample", ""))
  
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  
  if (is.null(outfile)) {
    outfile = tempfile()
  }
  
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
    frontopts = "-source ",
    outfile = outfile,
    samefile = FALSE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    # run = FALSE,
    retimg = FALSE
  )  
  if (res != 0) {
    warning(paste0("Result does not indicate success ",
                   "- function may not work as expected!"))
  }
  warp_outfile = paste0(outfile, "_WARP", suffix, ".BRIK")
  warp_outfile = afni_3dAFNItoNIFTI(warp_outfile, retimg = FALSE, ...)
  if (align) {
    align_outfile = paste0(outfile, "_Allin.nii")
    if (retimg) {
      align_outfile = neurobase::readnii(align_outfile)
    }
    align_matrix = paste0(outfile, "_Allin.aff12.1D")
    align_matrix = afni_read_affine(align_matrix)
  }
  outfile = paste0(outfile, suffix, ".BRIK")
  outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  attr(outfile, "afni_version") = afni_version()
  return(outfile)
}

#' @rdname afni_3dQwarp
#' @export
Qwarp = function(...) {
  afni_3dQwarp(...)  
}

#' @rdname afni_3dQwarp
#' @export
qwarp = function(...) {
  afni_3dQwarp(...)  
}