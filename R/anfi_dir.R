#' @title Get AFNI Directory 
#' @description Finds the AFNIDIR from system environment or 
#' \code{getOption("afni.path")}
#' for location of AFNI functions and returns it
#' @return Character path
#' @export
afni_dir = function() {
  afnidir = Sys.getenv("AFNIDIR")
  if (afnidir == "") {
    afnidir = getOption("afni.path")
    ## Will try a default directory (/usr/local/fsl) if nothing else
    if (is.null(afnidir)) {
      #### adding in "/usr/share/afni" for NeuroDeb
      def_paths = c("~/abin", "~/afni", "~/afni_bin", "/usr/share/afni")
      for (def_path in def_paths) {
        if (file.exists(def_path)) {
          def_path = path.expand(def_path)
          warning(paste0("Setting afni.path to ", def_path))
          afnidir = def_path
          break;
        }
      }
    }
  }
  if (is.null(afnidir)) stop("Can't find AFNI")
  if (afnidir %in% "") stop("Can't find AFNI")
  return(afnidir)
}

#' @rdname afni_dir
#' @export
afnidir = function(){
  afni_dir()
}