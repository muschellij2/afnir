#' @title AFNI Edge Mask 
#' @description Compute the edge voxels of a mask dataset.  An edge voxel is one    
#' that shares some face with a non-masked voxel.  This computation    
#' assumes file is a binary mas
#'
#' @param file nifti object or NIfTI filename to get edge mask, assumed binary.
#' @param ... additional arguments passed to \code{\link{afni_3dcalc}}
#' 
#' @return Output filename or nifti object (default)
#' @export
afni_edge_mask = function(
  file, 
  ...) {

  # reate a mask of edge-only voxels via
  file = checkimg(file)
  opts = c(b = "a+i", c = "a-i", 
           d = "a+j", e = "a-j", 
           f = "a+k", g = "a-k")
  opts = paste(paste0("-", names(opts), " ", opts), collapse = " ")
  res = afni_3dcalc(file = file, 
                    opts = opts,
                    expression = "ispositive(a)*amongst(0,b,c,d,e,f,g)")
  return(res)
}