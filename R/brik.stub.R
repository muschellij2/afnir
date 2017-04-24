#' @title Grab filename without BRIK, HEAD or nii
#' @description Quick helper function to strip off .nii, .nii.gz, BRIK, BRIK.gz,
#' HEAD, HEAD.gz, or brik/head from filename
#' @param x character vector of filenames ending in .nii or .nii.gz
#' @param bn Take \code{\link{basename}} of file?
#' @return A character vector with the same length as \code{x}
#' @export
brik.stub = function(x, bn = FALSE){
  nx = names(x)
  x = path.expand(x)
  stub = gsub("\\.gz$", "", x)
  stub = gsub("\\.nii$", "", stub)
  stub = gsub("\\.(HEAD|head)$", "", stub)
  stub = gsub("\\.(BRIK|brik)$", "", stub)
  if (bn) stub = basename(stub)
  names(stub) = nx
  return(stub)
}
