#' @title AFNI 3dresample function
#' @description Wrapper for AFNI \code{3dresample} function
#'
#' @param file nifti object or NIfTI filename.  
#' @param outfile Output filename (should not have an extension)
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dresample}
#' @param bound_type specify which boundary is preserved, options are 
#' "FOV" (default if \code{NULL}) or "SLAB"
#' @param dxyz resample to new dimensions for x, y, and z-directions
#' @param orient new axis order.  The orientation code is a 3 character string,
#' where the characters come from the respective sets {A,P}, {I,S}, {L,R}.
#' For example LPI makes the x-axis is
#' Left-to-Right, the y-axis is Posterior-to-Anterior,
#' and the z-axis is Inferior-to-Superior.
#' @param resample_mode Resampling method. 
#' The options are for 'Nearest Neighbor', 'Linear', 'Cubic' 
#' and 'Blocky' interpolation, respectively.
#' @param master a filename of an image, which the input file is aligned to 
#' (not doing registration, but takes orientation and pixel dimension information)
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#'
#' @return Output filename of the brik
#' @export
afni_3dresample = function(
  file,
  bound_type = NULL,
  dxyz = NULL, 
  orient = NULL,  
  resample_mode = c('NN', 'Li', 'Cu', 'Bk'),
  master = NULL, 
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  ...) {
  
  func = "3dresample"
  
  file = checkimg(file, allow_array = FALSE)
  suffix = afni_suffix(file, default = "orig")
  
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
  
  if (!is.null(master)) {
    master = checkimg(master, allow_array = FALSE)
  }
  opts = c(opts, ppaste0("-master ", x = master))
  
  if (!is.null(dxyz)) {
    if (length(dxyz) != 3) {
      stop("dxyz must be numbers of length 3")
    }
    dxyz = paste(dxyz, collapse = " ")
    if (!is.null(master)) {
      warning(paste0("if both -master and -dxyz are used, the dxyz values", 
      " will override those from the master dataset."))
    }
  }
  opts = c(opts, ppaste0("-dxyz ", x = dxyz))
  if (!is.null(orient)) {
    orient = paste(orient, collapse = "")
  }
  opts = c(opts, ppaste0("-orient ", x = orient))
  resample_mode = match.arg(resample_mode)
  opts = c(opts, ppaste0("-rmode ", x = resample_mode))
  
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
    frontopts = "-inset ",
    outfile = outfile,
    samefile = FALSE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    retimg = FALSE
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

#' @rdname afni_3dresample
#' @export
resample = function(...) {
  afni_3dresample(...)  
}
