#' @title AFNI 3dAllineate function
#' @description Wrapper for AFNI \code{3dAllineate} function
#'
#' @param file nifti object or NIfTI filename to register to \code{fixed} image
#' @param fixed nifti object or NIfTI filename of fixed image, target of registration 
#' @param outfile Output filename (should not have an extension)
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dAllineate}
#' @param cost cost function that defines the matching 
#' between the file and the fixed image 
#' ls= Least Squares [Pearson Correlation]
#' 
#' mi  = Mutual Information [H(b)+H(s)-H(b,s)]
#' 
#' crM = Correlation Ratio (Symmetrized*)
#' 
#' nmi = Normalized MI [H(b,s)/(H(b)+H(s))]
#' 
#' hel = Hellinger metric
#' 
#' crA = Correlation Ratio (Symmetrized+)
#' 
#' crU = Correlation Ratio (Unsym)
#' @param interpolator interpolation method to use during matching process
#' NN - nearest neighbor.
#' 
#' @param final_interpolator interpolation used to create the
#' output dataset. \code{wsinc5} - weighted sinc interpolation method
#' @param typeOfTransform transformation to use for registration 
#' shift_only - 3 parameters
#' 
#' shift_rotate - 6 parameters
#' 
#' shift_rotate_scale -  9 parameters
#' 
#' affine_general - 12 parameters
#' 
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#'
#' @return Output filename of the brik
#' @export
afni_3dAllineate = function(
  file,
  fixed,
  cost = c(
    "hel", 
    "ls", 
    "mi", 
    "crM", 
    "nmi", 
    "crA", 
    "crU"),
  interpolator = c(
    "linear",
    "cubic",
    "NN",
    "quintic"),
  final_interpolator = c(
    "cubic",
    "linear",
    "NN",
    "wsinc5",
    "quintic"),
  typeOfTransform = c(
    "affine_general",
    "shift_only",
    "shift_rotate",
    "shift_rotate_scale"),
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  ...) {
  
  func = "3dAllineate"
  
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
  interpolator = match.arg(interpolator)
  final_interpolator = match.arg(final_interpolator)
  cost = match.arg(cost)
  
  opts = c(opts, ppaste0("-interp ", x = interpolator))
  opts = c(opts, ppaste0("-final ", x = final_interpolator))
  
  typeOfTransform = match.arg(typeOfTransform)
  opts = c(opts, ppaste0("-warp ", x = typeOfTransform))
  
  opts = c(opts, ppaste0("-cost ", x = cost))
  
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
    frontopts = "-input ",
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
  outfile = paste0(outfile, suffix, ".BRIK")
  outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  attr(outfile, "afni_version") = afni_version()
  return(outfile)
}

#' @rdname afni_3dAllineate
#' @export
allineate = function(...) {
  afni_3dAllineate(...)  
}