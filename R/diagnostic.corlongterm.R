#' @title Correlation on Long time scales
#' @description Function to compute the temporal correlation between the observed and predicted time series.
#' @template templateMeasureParams
#' @param dates dates
#' @param method Character. Type of correlation applied. Options: \code{c("pearson","kendall","spearman")}.
#' @param t.aggr Character. Temporal aggregation options. Current accepted values are either \code{"annual"} or \code{"seasonal"}.
#' @param timescale Integer. Approximate filtering time scale (according to the \code{t.aggr} units).
#' @param detrend Logical. Whether the aggregated time series should be linearly detrended prior to filtering
#' @param plot Logical, for internal use only. Should the original (aggregated) and filtered series be plotted?
#' @return A float number corresponding to the correlation coefficient of choice between the predicted and observed series.
#' @author D. Maraun, J. Bedia
#' @keywords internal
#' @importFrom signal hamming
#' @importFrom RcppEigen fastLm
#' @importFrom stats filter cor
#' @export

measure.corlongterm <- function(indexObs = NULL, indexPrd = NULL, obs, prd, dates,
                                method = c("pearson", "kendall", "spearman"),
                                t.aggr = "annual", timescale = 1, detrend = FALSE, plot = FALSE) {
      method <- match.arg(method, choices = c("pearson", "kendall", "spearman"))
      aggregation <- match.arg(t.aggr, choices = c("annual", "seasonal"))
      if (t.aggr == "annual") {
            index <- substr(dates, 1,4)
            obsAg <- tapply(obs, INDEX = index, FUN = mean, na.rm = TRUE)
            prdAg <- tapply(prd, INDEX = index, FUN = mean, na.rm = TRUE)
      } else if (t.aggr == "seasonal") {
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
            obsAg <- tapply(obs, INDEX = index, FUN = mean, na.rm = TRUE)
            prdAg <- tapply(prd, INDEX = index, FUN = mean, na.rm = TRUE)
      }
      ## optional detrending
      if (detrend) {
            t <- 1:length(obsAg)
            obsAg <- obsAg - predict(fastLm(obsAg ~ t))
            prdAg <- prdAg - predict(fastLm(prdAg ~ t))
      }
      ## Filtering of aggregated time series
      obsAgfilt <- tryCatch(expr = stats::filter(obsAg, hamming(timescale) / sum(hamming(timescale))),
                            error = function(er) {
                                  er <- NA
                                  return(er)
                            })
      prdAgfilt <- tryCatch(stats::filter(prdAg, hamming(timescale) / sum(hamming(timescale))),
                            error = function(er) {
                                  er <- NA
                                  return(er)
                            })
      if (plot) {
            ylim <- range(c(obsAg, prdAg), na.rm = TRUE)
            plot(obsAg, type = "l", ylim = ylim, lty = 2)
            lines(prdAg, col = "blue", lty = 2)
            lines(obsAgfilt, lwd = 2)
            lines(prdAgfilt, lwd = 2, col = "blue")
            legend("topleft", c("obs", "pred", "obs.filt", "pred.filt"),
                   lty = c(2,2,1,1), lwd = c(1,1,2,2), col = c(1,4,1,4))
      }
      cor(obsAgfilt, prdAgfilt, use = "pairwise.complete.obs", method = method)
}
