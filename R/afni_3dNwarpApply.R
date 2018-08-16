#' @title AFNI 3dNwarpApply function
#' @description Wrapper for AFNI \code{3dNwarpApply} function
#'
#' @param file nifti object or NIfTI filename to register to \code{fixed} image
#' @param warp list of nifti objects or NIfTI filenames of warps to be applied
#' @param fixed nifti object or NIfTI filename of the target space, used 
#' to determine the output grid/resolution.  If not specified, then it will use 
#' that of \code{file}.  You can also specify this as \code{"WARP"} to use the 
#' warps as the destination resolutione/etc.
#' @param interpolator interpolation method to use during matching process
#' NN - nearest neighbor. 
#' @param invert_warp After the warp specified in \code{warp} is computed,
#' invert it.  If the input warp would take a dataset
#' from space A to B, then the inverted warp will do
#' the reverse.
#' @param outfile Output filename (should not have an extension).  This 
#' should be the same length as \code{file}
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dNwarpApply}
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#'
#' @return Output filename of the brik
#' @export
afni_3dNwarpApply = function(
  file, #source
  warp, # base
  fixed = NULL,
  interpolator = c(
    "wsinc5",
    "cubic",
    "linear",
    "NN",
    "quintic"),
  invert_warp = FALSE,
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  ...) {
  
  func = "3dNwarpApply"
  
  file = checkimg(file, allow_array = FALSE)
  suffix = afni_suffix(file, default = "tlrc")
  
  #############################
  # Put the warps together
  #############################  
  warp = checkimg(warp, allow_array = FALSE)
  warp = paste(warp, collapse = " ")
  warp = afni_quote_expression(warp)

  
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
  opts = c(opts, ppaste0("-nwarp ", x = warp))
  
  if (!is.null(fixed)) {
    if (fixed != "WARP") {
      fixed = checkimg(fixed, allow_array = FALSE)
    }
  }
  opts = c(opts, ppaste0("-master ", x = fixed))
  
  opts = c(opts, ifelse(invert_warp, "-iwarp", ""))
  
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  
  if (is.null(outfile)) {
    outfile = sapply(file, function(x) {
      tempfile()
    })
  }
  
  if (length(outfile) != length(file)) {
    warning(paste0("There may be an issue if you do not ", 
                   "specify same number of output files as input files"))
  }
  cat_outfile = paste(outfile, collapse = " ")
  cat_outfile = afni_quote_expression(cat_outfile)
  
  cat_file = paste(file, collapse = " ")
  cat_file = afni_quote_expression(cat_file)
  
  opts = paste(opts, collapse = " ")
  opts = paste0(opts, " -prefix")
  
  brik_outfile = paste0(outfile, suffix, ".BRIK")
  if (any(file.exists(brik_outfile))) {
    stop(paste0("Dataset name conflicts with existing file,", 
                " delete if overwriting"))
  }
  
  res = afni_cmd(
    file = cat_file,
    func = func,
    frontopts = "-source",
    opts = opts,
    outfile = cat_outfile,
    samefile = FALSE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    quote_file = FALSE,
    # run = FALSE,
    retimg = FALSE
  )  
  if (res != 0) {
    warning(paste0("Result does not indicate success ",
                   "- function may not work as expected!"))
  }

  outfile = paste0(outfile, suffix, ".BRIK")
  if (length(outfile) > 1) {
    outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
    attr(outfile, "afni_version") = afni_version()
  } else {
    outfile = lapply(outfile, function(x) {
      x = afni_3dAFNItoNIFTI(x, retimg = retimg, ...)
      attr(x, "afni_version") = afni_version()
      return(x)      
    })
  }
  
  return(outfile)
}

#' @rdname afni_3dNwarpApply
#' @export
afni_warp_apply = function(...) {
  afni_3dNwarpApply(...)  
}

#' @rdname afni_3dNwarpApply
#' @export
NwarpApply = function(...) {
  afni_3dNwarpApply(...)  
}