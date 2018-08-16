
#' Subset 4D images
#'
#' @param file 4-dimensional Filename or nifti object to subset
#' @param time_start the time point to subset from. 
#' Remember timepoint numbering starts from 0
#' @param time_end the last time point to subset
#' @param by How should the subsetting be done.  If you want to 
#' subset every second time point, use by = 2 for example
#' @param retimg Should an image be returned (\code{TRUE}) 
#' or a filename?
#'
#' @return A \code{nifti} object or an output file depending on
#' \code{retimg}
#' @export
#'
#' @examples
#' library(kirby21.fmri)
#' if (have_afni()) {
#' tdir = tempfile()
#' res = download_fmri_data(outdir = tdir)
#' file = kirby21.fmri::get_fmri_filenames(outdir = tdir)[1]
#' sub_file = afni_subset_4d(file, time_start = 0, time_end = 1)
#' dim(neurobase::readnii(sub_file))[4] == 2
#' }
afni_subset_4d = function(
  file, 
  time_start = 0,
  time_end = "$",
  by = 1,
  retimg = FALSE) {
  file = checkimg(file, allow_array = FALSE)
  if (by != 1) {
    time_end = paste0(time_end, "(", by, ")")
  }
  suffix = paste0("[", time_start, "..", time_end, "]")
  file = paste0(file, suffix)
  
  res = afni_3dcalc(file = file, expression = "a", retimg = retimg)
  return(res)
}