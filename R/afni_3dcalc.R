#' @title AFNI 3dcalc function
#' @description Wrapper for AFNI \code{3dcalc} function
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param expression Calculation expression, with single quotes
#' @param outfile Output filename, should have a \code{.nii.gz}
#' extension
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dcalc}
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#'
#' @return Output filename of the brik
#' @export
afni_3dcalc = function(
  file,
  expression,
  outfile = NULL,
  retimg = TRUE,
  opts = "",
  ...) {
  func = "3dcalc"
  
  expression = afni_quote_expression(expression)
  opts = paste0("-expr ", expression, " ", opts)
  opts = trimws(opts)
  
  file = checkimg(file)
  suffix = afni_suffix(file[1], default = "orig")
  
  names(file) = letters[seq(length(file))]
  file = paste0("-", names(file), " ", file)
  file = paste(file, collapse = " ")

  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  out_ext = parse_img_ext(outfile)
  
  opts = paste0(opts, " -prefix")

    
  msg = paste0("Dataset name conflicts with existing file,", 
               " delete if overwriting")
  if (is.na(out_ext)) {
    brik_outfile = paste0(outfile, suffix, ".BRIK")
    if (file.exists(brik_outfile)) {
      stop(msg)
    }
  } else {
    if (file.exists(outfile)) {
      stop(msg)
    }
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
  if (is.na(out_ext)) {
    brik_outfile = paste0(outfile, suffix, ".BRIK")
    outfile = paste0(outfile, suffix, ".BRIK")
    outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  }
  attr(outfile, "afni_version") = afni_version()
  
  return(outfile)
}
  
