% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure.cm.R
\name{measure.cm}
\alias{measure.cm}
\title{Cramer von Misses}
\source{
L. Sachs and J. Hedderich (2006). Angewandte Statistik. Springer.
}
\usage{
measure.cm(
  indexObs = NULL,
  indexPrd = NULL,
  obs = NULL,
  prd = NULL,
  dates,
  Nbins = 100,
  what = c("CvM", "pval", "KSdiff"),
  threshold = NULL
)
}
\arguments{
\item{indexObs}{index computed from the observations}

\item{indexPrd}{index computed from the predictions}

\item{obs}{A vector of observations}

\item{prd}{A vector of predictions}

\item{dates}{dates}

\item{Nbins}{Number of bins used for discretization. Default to 100.}

\item{what}{Measure to be returned. This can be the Cramer von Misses index (\code{"CvM"}), 
the p-value (\code{"pvalue"}) of the test or the maximum absolute difference between the observed and predicted
 distributions (\code{"KSdiff"}). Default to \code{"CvM"}.}

\item{threshold}{Optional. Numeric value indicating the threshold above which to compute the measure.
Mainly used for precipitation, tipically to use only the wet days in the analysis (i.e.: \code{threshold = 1}).}
}
\value{
A floating number with the measure indicated in \code{what} argument.
}
\description{
Function to compare the observed and predicted distribution using the Cramer von Misses index.
}
\author{
Ole Roessler, J. Bedia, D. San-Martín, S. Herrera
}
