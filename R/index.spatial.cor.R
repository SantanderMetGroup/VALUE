#' @title Pairwise Spatial Correlation
#' @description Function to compute the pairwise spatial correlation for a set of grid
#' points/stations.
#' @author M.N. Legasa
#' @param tsl A list of vectors (one for each gridpoint/station) containing the data
#' @param type Correlation type: \code{"pearson"}, \code{"kendall"} or \code{"spearman"}
#' @param use Check \code{?cor}. \code{"pairwise.complete.obs"} (default) ignores
#'  uses all available info for each column pair.
#' @importFrom stats cor
#' @return A matrix number corresponding to the proportion/number of days below/above the defined threshold.


index.spatial.cor <- function(tsl, type = "pearson", use = "pairwise.complete.obs"){
  
  type <- match.arg(type, c("pearson", "spearman"))
  tsld <- as.data.frame(tsl)
  names(tsld) <- 1:length(tsl)
  tsl <- NULL
  tbr <- cor(x = tsld, use = use, method = type)

  return(tbr)
}      
