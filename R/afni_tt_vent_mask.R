
#' @title Create AFNI Ventricle Mask in Talairach space
#' @description Example of making a ventricle mask, to be used if Freesurfer
#' is not available or needed before using afni_proc.py
#' @note See 
#' \url{https://afni.nimh.nih.gov/pub/dist/doc/program_help/afni_proc.py.html}
#' for example.  This code uses \code{TT_desai_dd_mpm+tlrc}
#' @param dxyz Dimensions to Resample the Image
#' @param ... additional arguments passed to \code{\link{afni_3dresample}}
#'
#' @return An object of class \code{nifti}, unless \code{retimg = FALSE} in
#' \code{...}
#' @export
afni_tt_vent_mask = function(dxyz = rep(2.5, 3),
                             ...){
  orig_file = file.path(afni_dir(), "TT_desai_dd_mpm+tlrc")
  file = afni_3dcalc(
    file = orig_file,
    expression = "'amongst(a,152,170)'",
    retimg = FALSE)
  
  resampled = afni_3dresample(file = file, dxyz = dxyz, ...)
  return(resampled)
}