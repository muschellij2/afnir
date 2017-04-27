#' @title Read AFNI Affine Matrix
#' @description Reads in an AFNI Affine matrix from an ASCII file using
#' \code{\link{readLines}}
#'
#' @param x Filename of Affine matrix
#'
#' @return A matrix (most likely) or list of matrices (not likely)
#' @export
afni_read_affine = function(x) {
  con = file(x)
  on.exit({
    close(con)
  })
  mat = readLines(con)
  mat = trimws(mat)
  mat = gsub("\\s+", " ", mat)
  comment = grepl("^#", mat)
  comments = mat[comment]
  mat = mat[ !comment ]
  if (length(mat) > 1) {
    warning("Matrix has more than one row, may not work as intended!")
  }
  mat = strsplit(mat, " ")
  mat = lapply(mat, as.numeric)
  mat = lapply(mat, function(x) {
    matrix(x, nrow = 3, byrow = TRUE)
  })
  if (length(mat) == 1) {
    mat = mat[[1]]
  }
  attr(mat, "header") = comments
  return(mat)
}