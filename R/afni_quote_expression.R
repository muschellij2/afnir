#' @title Quote an Expression
#' @description Simple wrapper for ensuring a string has single quotes at the front
#' and end of the string
#'
#' @param x character string
#' @param trim should \code{\link{trimws}} be run first
#'
#' @return A character string
#' @export
#' 
#' @examples
#' afni_quote_expression("hey")
#' afni_quote_expression("'hey'")
#' afni_quote_expression(c("'hey'", "ho", ""))
#' 
afni_quote_expression = function(x, trim = TRUE) {
  if (trim) {
    x = trimws(x)
  }
  x = strsplit(x, split = "")
  x = sapply(x, function(r){
    if (length(r) > 0) {
      if (r[1] != "'") {
        r = c("'", r)
      }
      if (r[length(r)] != "'") {
        r = c(r, "'")
      }  
      r = paste(r, collapse = "")
    } else {
      r = ""
    }
    return(r)
  })
  return(x)
}