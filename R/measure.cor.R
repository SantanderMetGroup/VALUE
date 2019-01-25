#' @title Correlation
#' @description Function to compute the temporal correlation between the observed and predicted time series. 
#' @template templateMeasureParams
#' @param dates dates
#' @param method Type of correlation applied. Options: \code{c("pearson","kendall","spearman")}.
#' @param deseason Default to \code{NULL}, and ignored. Only used if removal of the seasonal cycle is to be done.
#'  In this case, this is an integer number indicating the width, in days, of the window used for moving average computation
#'   of the reference daily climatology. This argument is passed to \code{\link{deseason.VALUE}} via \code{\link{wrapperFUN}}.
#' @param aggregation temporal aggregation options
#' @return A float number corresponding to the correlation coefficient of choice between the predicted and observed series.
#' @author J. Bedia, D. San-Martín, S. Herrera
#' @keywords internal
#' @export


measure.cor <- function(indexObs = NULL, indexPrd = NULL, obs, prd, dates,
                           method = c("pearson", "kendall", "spearman"),
                           deseason = NULL, aggregation = NULL) {
    if (length(obs) <= 1) stop("Observed time series is needed")
    if (length(prd) <= 1) stop("Predicted time series is needed")
    if (is.null(aggregation)) {
        cor(obs, prd, use = "pairwise.complete.obs", method = method)    
    } else if (aggregation == "annual") {
        index <- substr(dates, 1,4)
        obsY <- tapply(obs, INDEX = index, FUN = mean, na.rm = TRUE)
        prdY <- tapply(prd, INDEX = index, FUN = mean, na.rm = TRUE)
        cor(obsY, prdY, use = "pairwise.complete.obs", method = method)    
    } else if (aggregation == "seasonal") {
        year <- substr(dates, 1,4)
        season <- substr(dates, 6,7)
        ixWinter <- which(season %in% c("12","01","02"))
        ixSpring <- which(season %in% c("03","04","05"))
        ixSummer <- which(season %in% c("06","07","08"))
        ixAutumn <- which(season %in% c("09","10","11"))
        season[ixWinter] <- "01"
        season[ixSpring] <- "02"
        season[ixSummer] <- "03"
        season[ixAutumn] <- "04"
        index = paste0(year, season)
        obsS <- tapply(obs, INDEX = index, FUN = mean, na.rm = TRUE)
        prdS <- tapply(prd, INDEX = index, FUN = mean, na.rm = TRUE)
        cor(obsS, prdS, use = "pairwise.complete.obs", method = method)
    }
}
