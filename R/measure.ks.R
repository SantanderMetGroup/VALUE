#' @title Two-sided KS score
#' @description Function to compute the (two-sided) Kolmogorov-Smirnov (KS) score between the observed and predicted time series. 
#' @template templateMeasureParams
#' @return A float number corresponding to the KS score.
#' @author J. Bedia
#' @importFrom stats ks.test
#' @export

measure.ks <- function(indexObs = NULL, indexPrd = NULL, obs, prd) {
    if (length(obs) <= 1) stop("Observed time series is required")
    if (length(prd) <= 1) stop("Predicted time series is required")
    unname(ks.test(obs, prd, alternative = "two.sided")$statistic)
}


