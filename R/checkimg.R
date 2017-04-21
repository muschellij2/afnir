#' @rdname checkimg-methods
#' @aliases checkimg,afni-method
#' @title Force object to filename  
#' @description Ensures the output to be a character filename (or vector) from an input
#' image or \code{nifti/afni}.  
#' @param file character or \code{afni} object
#' @param ... options passed to \code{\link{afni_3dAFNItoNIFTI}} 
#' @export
#' @importFrom neurobase checkimg 
#' @importFrom oro.nifti writeAFNI
#' @importFrom methods setMethod
setMethod("checkimg", "afni", function(file, ...) { 
  tfile = tempfile()
  writeAFNI(nim = file, fname = tfile)
  res = afni_3dAFNItoNIFTI(file = tfile, outfile = NULL, retimg = FALSE, ...)
  return(res)
})