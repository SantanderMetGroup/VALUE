#' @title Root mean square error
#' @description Function to compute the root mean square error between the observed and predicted time series.
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateMeasureParams
#' @param dates dates
#' @return A float number corresponding to the root mean square error between the predicted and observed series.
#' @export

measure.rmse <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      sqrt(index.mean((prd - obs) ** 2))
}
