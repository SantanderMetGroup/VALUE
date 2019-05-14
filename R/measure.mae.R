#' @title Mean absolute error
#' @description Function to compute the mean absolute difference between the observed and predicted time series.
#' @author Daniel San-Martín, J. Bedia, S. Herrera
#' @template templateMeasureParams
#' @param dates dates
#' @return A float number corresponding to the mean absolute difference between the predicted and observed series.
#' @export


measure.mae <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates) {
    if (length(obs) <= 1) stop("Observed time series is needed")
    if (length(prd) <= 1) stop("Predicted time series is needed")
    index.mean(abs(prd - obs))
}
