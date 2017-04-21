#' @title Convert NIfTI Image to AFNI FIle
#'
#' @param file NIfTI image or \code{nifti} object
#' @param outfile output filename or prefix.  
#'
#' @return A character filename of the output BRIK
#' @export
nifti_to_afni = function(file,
                         outfile = NULL) {
  expression = "'a'"
  opts = "-prefix"
  if (is.null(outfile)) {
    outfile = tempfile()
  }
  res = afni_3dcalc(file = file,
              expression = expression,
              outfile = outfile,
              opts = opts)
  return(res)
}