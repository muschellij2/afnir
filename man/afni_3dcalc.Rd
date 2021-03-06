% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afni_3dcalc.R
\name{afni_3dcalc}
\alias{afni_3dcalc}
\title{AFNI 3dcalc function}
\usage{
afni_3dcalc(file, expression, outfile = NULL, retimg = TRUE,
  opts = "", ...)
}
\arguments{
\item{file}{nifti object or NIfTI filename.  If more than one is given,
they are given names of the letters up to \code{z}.}

\item{expression}{Calculation expression, with single quotes}

\item{outfile}{Output filename, should have a \code{.nii.gz}
extension}

\item{retimg}{Should an image be returned (\code{TRUE}) or a filename?}

\item{opts}{Additional options passed to \code{3dcalc}}

\item{...}{additional arguments to \code{\link{afni_3dAFNItoNIFTI}}}
}
\value{
Output filename of the brik
}
\description{
Wrapper for AFNI \code{3dcalc} function
}
