#' @title AFNI 3dTstat function
#' @description Wrapper for AFNI \code{3dTstat} function
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dTstat}
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#'
#' @return Output filename
#' @export
afni_3dTstat = function(
  file,
  retimg = FALSE,
  opts = "",
  ...) {
  
  outfile = NULL
  
  func = "3dTstat"
  
  opts = trimws(opts)
  
  file = checkimg(file, allow_array = FALSE)
  suffix = afni_suffix(file[1], default = "orig")
  
  if (is.null(outfile)) {
    outfile = tempfile()
  }
  
  opts = c(opts, paste0(" -prefix ", outfile))
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  
  
  brik_outfile = paste0(outfile, suffix, ".BRIK")
  if (file.exists(brik_outfile)) {
    stop(paste0("Dataset name conflicts with existing file,", 
                " delete if overwriting"))
  }
  
  res = afni_cmd(
    file = file,
    func = func,
    frontopts = opts,
    opts = "",
    outfile = NULL,
    samefile = TRUE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    retimg = FALSE,
    run = TRUE,
    quote_file = FALSE
  )  
  if (res != 0) {
    warning(paste0("Result does not indicate success ", 
                   "- function may not work as expected!"))
  }
  outfile = paste0(outfile, suffix, ".BRIK")
  outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  attr(outfile, "afni_version") = afni_version()
  
  return(outfile)
}


#' @export
#' @rdname afni_3dTstat
afni_3dtstat = afni_3dTstat


#' @export
#' @rdname afni_3dTstat
tstat = afni_3dTstat


#' @export
#' @rdname afni_3dTstat
Tstat = afni_3dTstat