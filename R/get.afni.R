#' @title Create command declaring for the AFNI directory
#' @description Finds the AFNIDIR from system environment or \code{getOption("afni.path")}
#' for location of AFNI functions
#' @return NULL if AFNI in path, or bash code for setting up AFNI DIR
#' @export
get_afni = function(fallback = TRUE){
  cmd = NULL
  afnidir = Sys.getenv("AFNIDIR")
  if (afnidir == "") {
    afnidir = getOption("afni.path")
    ## Will try a default directory (/usr/local/fsl) if nothing else
    if (is.null(afnidir)) {
      #### adding in "/usr/share/afni" for NeuroDeb
      def_paths = c("~/abin", "~/afni", "~/afni_bin", "/usr/share/afni")
      for (def_path in def_paths) {
        if (file.exists(def_path)) {
          warning(paste0("Setting afni.path to ", def_path))
          options(afni.path = def_path)
          afnidir = def_path
          break;
        }
      }
    }    
    cmd = ""
    if (fallback) {
      cmd = paste0("export DYLD_FALLBACK_LIBRARY_PATH=", shQuote(afnidir), ";")
    }
    cmd <- paste0(cmd, 
                  "AFNIDIR=", shQuote(afnidir), "; ", 
                  paste0('PATH=${AFNIDIR}:${PATH};'),
                  'export PATH AFNIDIR; ', 
                  paste0("${AFNIDIR}/")
    )
  } 
  if (is.null(afnidir)) stop("Can't find AFNI")
  if (afnidir %in% "") stop("Can't find AFNI")
  return(cmd)
}
