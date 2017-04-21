#' @rdname checkimg-methods
#' @aliases checkimg,afni-method
#' @export
#' @importFrom neurobase checkimg 
#' @importFrom oro.nifti writeAFNI
setMethod("checkimg", "afni", function(file, ...) { 
  tfile = tempfile()
  writeAFNI(nim = file, fname = tfile)
  res = afni_3dAFNItoNIFTI(file = tfile, outfile = NULL, retimg = FALSE)
  return(res)
})