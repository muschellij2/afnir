#' @title AFNI 3drefit function
#' @description Wrapper for AFNI \code{3drefit} function
#'
#' @param file nifti object or NIfTI filename to change the header
#' @param opts Additional options passed to \code{3drefit}
#' @param ... not currently used
#'
#' @return Output filename of the image
#' @importFrom neurobase parse_img_ext
#' @export
afni_3drefit = function(
  file,
  opts = "",
  ...) {
  
  func = "3drefit"
  file = checkimg(file, allow_array = FALSE)
  
  #############################################
  # Making all the options
  #############################################  
  opts = trimws(opts)
  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")
  
  img_ext = neurobase::parse_img_ext(file)
  ext = tools::file_ext(file)
  img_ext = paste0(img_ext, ifelse(ext %in% "gz", ".gz", ""))
  outfile = tempfile(fileext = paste0(".", img_ext))
  file.copy(file, outfile)

  res = afni_cmd(
    file = outfile,
    func = func,
    opts = "",
    frontopts = opts,
    outfile = NULL,
    samefile = TRUE,
    add_ext = FALSE,
    quote_outfile = FALSE,
    # run = FALSE,
    retimg = FALSE
  )  
  if (res != 0) {
    warning(paste0("Result does not indicate success ",
                   "- function may not work as expected!"))
  }
  # outfile = paste0(outfile, suffix, ".BRIK")
  # outfile = afni_3dAFNItoNIFTI(outfile, retimg = retimg, ...)
  attr(outfile, "afni_version") = afni_version()
  return(outfile)
}

#' @rdname afni_3drefit
#' @export
refit = function(...) {
  afni_3drefit(...)  
}