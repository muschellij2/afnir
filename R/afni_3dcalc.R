
#' @title AFNI 3dcalc function
#' @description Wrapper for AFNI \code{3dcalc} function
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param expression Calculation expression, with single quotes
#' @param outfile Output filename (should not have an extension)
#' @param opts Additional options passed to \code{3dcalc}
#'
#' @return Output filename of the brik
#' @export
afni_3dcalc = function(
  file,
  expression,
  outfile = NULL,
  opts = "") {
  func = "3dcalc"
  
  opts = paste0("-expr ", expression, " ", opts)
  opts = trimws(opts)
  
  file = checkimg(file)
  
  names(file) = letters[seq(length(file))]
  file = paste0("-", names(file), ' "', file, '"')
  file = paste(file, collapse = " ")
  
  brik_outfile = paste0(outfile, "+orig.BRIK")
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
  outfile = paste0(outfile, "+orig.BRIK")
  return(outfile)
}
  
