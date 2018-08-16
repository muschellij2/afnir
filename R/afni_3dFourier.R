#' @title AFNI 3dFourier function
#' @description Wrapper for AFNI \code{3dFourier} function
#'
#' @param file nifti object or NIfTI filename. 
#' @param lowpass low pass filter with a cutoff in Hz
#' @param highpass high pass filter with a cutoff in Hz
#' @param ignore ignore the first \code{n} images
#' @param retrend Any mean and linear trend are removed 
#' before filtering. This will restore the trend after filtering.
#' @param retimg Should an image be returned (\code{TRUE}) 
#' or a filename?
#' @param opts Additional options passed to \code{3dFourier}
#' @param ... additional arguments to \code{\link{readnii}}
#'
#'
#' @return Output filename
#' @export
afni_3dFourier = function(
  file,
  lowpass = NULL,
  highpass = NULL,
  ignore = NULL,
  retrend = FALSE,
  retimg = FALSE,
  opts = "",
  ...) {
  
  outfile = NULL
  
  func = "3dFourier"
  
  opts = trimws(opts)
  file = checkimg(file, allow_array = FALSE)
  
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
  opts = c(opts, ppaste0("-lowpass ", x = lowpass))
  opts = c(opts, ppaste0("-highpass ", x = highpass))
  opts = c(opts, ppaste0("-ignore ", x = ignore))
  opts = c(opts, ifelse(retrend, "-retrend", ""))

  outfile = tempfile(fileext = ".nii.gz")
  opts = c(opts, paste0(" -prefix ", outfile))
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  

  res = afni_cmd(
    file = file,
    func = func,
    frontopts = opts,
    opts = "",
    outfile = NULL,
    samefile = TRUE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    retimg = FALSE,
    run = TRUE,
    quote_file = FALSE
  )  
  if (res != 0) {
    warning(paste0("Result does not indicate success ", 
                   "- function may not work as expected!"))
  }
  if (retimg) {
    outfile = readnii(outfile, ...)
  } 
  attr(outfile, "afni_version") = afni_version()
  
  return(outfile)
}