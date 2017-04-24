#' @title Get AFNI suffix for file
#' @description Returns 
#' @param x A vector of character filenames
#' @param default if no suffix is found, use this value.  You can also specify
#' \code{NA}
#'
#' @return A character vector, the same length as \code{x}, with suffixes
#' @export
#'
#' @examples
#' afni_suffix("mybrik+orig.nii.gz")
#' afni_suffix("mybrik+orig[1].nii.gz")
afni_suffix = function(x, default = "orig") {
  nx = names(x)
  x = brik.stub(x, bn = TRUE)
  ss = strsplit(x, split = "+", fixed = TRUE)
  remove_bracket = function(x){
    gsub("(.*)\\[.*", "\\1", x)
  }
  ss = sapply(ss, function(r) {
    r = remove_bracket(r)
    if (length(r) < 2) {
      return(NA)
    }
    r = r[seq(2, length(r))]
    r = paste(r, collapse = "+")
    return(r)
  })
  
  ss[is.na(ss)] = default
  
  ss[!is.na(ss)] = paste0("+", ss[!is.na(ss)])
  names(ss) = nx
  return(ss)
}
