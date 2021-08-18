#' @title Autocorrelation function
#' @description Function to compute the estimated autocorrelation function at pre-defined lags.
#' @author Neyko Neykov, J. Bedia, D. San-Martín, S. Herrera
#' @template templateIndexParams
#' @param lag.max Maximum lag considered for acf calculation. Default \code{lag.max = 1}.
#' @return A scalar with the estimated autocorrelation for that lag.
#' @importFrom stats acf na.pass
#' @importFrom utils tail
#' @keywords internal
#' @export

index.acf <- function(ts, lag.max = 1){
      if(all(is.na(ts))) {
        NA
      } else {
      meanObj <- acf(ts, na.action = na.pass,
                     plot = FALSE,
                     lag.max = lag.max,
                     type = "correlation",
                     demean = TRUE)
      tail(meanObj$acf,1)
      }
}
