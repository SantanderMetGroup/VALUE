% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.rv.R
\name{index.rv}
\alias{index.rv}
\title{Return Value}
\usage{
index.rv(ts, prob = 20, annual.index = TRUE, tail)
}
\arguments{
\item{ts}{A vector containing the data}

\item{prob}{Return period considered (in years). Default to 20.}

\item{annual.index}{Logical. If set to TRUE, a vector defining the years for aggregation is passed by \code{\link{wrapperFUN}}.
Otherwise no temporal aggregation of the input time series is performed.}

\item{tail}{A character indicating wheter GEV is being fit for maxima (\code{tail="right"}) or minima (\code{tail="left"}).}
}
\value{
Return value
}
\description{
Function to compute return values
}
\author{
Neyko Neykov, S. Herrera, J. Bedia
}
