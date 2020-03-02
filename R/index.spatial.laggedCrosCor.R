#' @title Lagged Spatiotemporal Cross-Correlations
#' @description Function to compute the pairwise lagged cross-correlations for a set of grid
#' points/stations.
#' @author M.N. Legasa
#' @param tsl A list of vectors (one for each gridpoint/station) containing the data
#' @param dates A character (or \code{POSIXct}) vector following the format
#'  \dQuote{YYYY-MM-DD} (i.e., \code{format = "\%Y-\%m-\%d"} as in 
#'  \code{\link{strptime}}). Note that, if dates are not provided, data 
#'  will be considered consecutive. 
#' @param lag Time lag. 1 by default.
#' @param type Correlation type: \code{"pearson"} or \code{"spearman"}
#' @param use Check \code{?cor}. \code{"pairwise.complete.obs"} (default) ignores
#'  uses all available info for each column pair.
#' @param parallel Use parallel computation.
#' @param max.ncores Maximum number of cores for parallel computing.
#' @param ncores Number of threads for parallel computation. Default to ncores - 1.
#' @importFrom stats cor
#' @importFrom transformeR parallelCheck
#' @importFrom pbapply pblapply
#' @return index.code matrix
#' @author M. N. Legasa

index.spatial.laggedCrosCor <- function(tsl, 
                                        dates = NULL,
                                        lag = 1,
                                        type = "pearson",
                                        use = "pairwise.complete.obs",
                                        parallel = FALSE,
                                        max.ncores = 16,
                                        ncores = NULL
                                        ){
  type <- match.arg(type, c("pearson", "spearman"))
  
  if (is.null(dates)) ind <- c(0, length(dates))
  else{
    dates <- as.Date(dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    disc <- diff(dates)
    ind <- if (any(disc > 1)) {
      c(0, which(disc > 1), length(dates))
    } else {
      c(0, length(dates))
    }
  }
  
  if (length(which(diff(ind) <= lag)) != 0){
    ind <- ind[-(which(diff(ind) <= lag)-1)]
  }
  
  tsnodisc <- list()
  for (i in 1:(length(ind)-1)){
    tsnodisc[[i]] <- lapply(tsl, 
                            function(ts) {
                              ts[(ind[i]+1):ind[i+1]]
                            }
    )
  }
    
  tsnodisc[which(sapply(tsnodisc, 
                        function(x) length(x[[1]])) <= lag)] <- NULL
  
  parallel.args <- parallelCheck(parallel = parallel,
                                 ncores = ncores,
                                 max.ncores = max.ncores)
  lagged.correlations <- 
    pblapply(1:length(tsl), 
           function(loc){
             loc.displaced <- 
               do.call(rbind,
                       lapply(tsnodisc,
                              function(ts.gaps){
                                l <- length(ts.gaps[[loc]])
                                return(cbind(ts.gaps[[loc]][(lag+1):l],
                                             do.call(cbind,
                                                     ts.gaps)[1:(l-lag),]
                                             )
                                       )
                              }
                              )
                       )
             return(
               as.vector(
                 cor(x = loc.displaced[ ,1], 
                     y = loc.displaced[ ,-1],
                     use = use, method = type)
                 )
             )
           }, cl = parallel.args$cl
           )
  
  if (parallel.args$hasparallel) on.exit(parallel::stopCluster(parallel.args$cl))
  
  lagged.correlations <- do.call(cbind, lagged.correlations)
  attr(lagged.correlations, "dimnames.names") <-
    c(paste0("loc.T",-lag), "loc.T")
  return(lagged.correlations)
}
