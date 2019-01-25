#' @title Brier score
#' @description Function to compute the Brier score. 
#' @template templateMeasureParams
#' @param dates dates
#' @param threshold A float number defining the threshold considered. Default to 0 (for temperature).
#' @param condition Inequality operator to be applied considering the given threshold.
#' \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than.
#' @return A float number corresponding to the BS.
#' @references https://en.wikipedia.org/wiki/Brier_score
#' @author J. Bedia
#' @export


measure.bs <- function(indexObs = NULL, indexPrd = NULL, obs, prd, dates,
                       threshold = 0,
                       condition) {
    if (length(obs) <= 1) stop("Observed time series is needed")
    if (length(prd) <= 1) stop("Predicted time series is needed")
    condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
    ineq <- switch(condition,
                   "GT" = ">",
                   "GE" = ">=",
                   "LT" = "<",
                   "LE" = "<=")
    obs.bin <- eval(parse(text = paste("as.integer(obs", ineq, "threshold)")))
    prd.bin <- eval(parse(text = paste("as.integer(prd", ineq, "threshold)")))
    sum((prd.bin - obs.bin) ^ 2) / length(obs)
}
