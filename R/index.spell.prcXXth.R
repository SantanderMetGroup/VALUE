#' @title Spell length percentiles
#' @description Computes a user-defined percentile for the duration of spells (above/below) a given threshold.
#' @template templateIndexParams
#' @template templateDates
#' @param threshold A float number defining the absolute threshold considered. Default to 1 
#' @param threshold.type Is the value of \code{threshold} an absolute value or a quantile [0-1]?.
#' Two possible values: \code{"abs"} and \code{"prob"} respectively. Default to \code{"abs"}.
#' @param condition Inequality operator to be applied considering the given threshold.
#'  \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than
#' @param prob A float number in the range [0,1] defining the probability of the quantile to be calculated.
#'  Default to median (\code{prob = 0.5}).
#' @param min.spell.length integer value (in the time units of the input data, usually days).
#' Removes all spells equal or lower than this number prior to percentile calculation. See details.
#' @return A float number with the corresponding percentile.
#' @author N. Neykov, S. Herrera, J. Bedia
#' @details The function requires the date information in order to ensure that spells
#'  are computed on consecutive records.
#'  
#'  In case a \code{min.spell.length} is indicated, note that the function will (silently) ignore 
#'  it if no spells of such duration are found in the input time series.
#' @export

index.spell.prcXXth <- function(ts, dates, threshold = 1,
                                threshold.type = "abs",
                                condition = c("GT", "GE", "LT", "LE"),
                                prob = .5,
                                min.spell.length = NULL) {
      condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
      threshold.type <- match.arg(threshold.type, choices = c("abs","prob"))
      ineq <- switch(condition,
                     "GT" = ">",
                     "GE" = ">=",
                     "LT" = "<",
                     "LE" = "<=")
      # Index of discontinuities in the data series
      datesi <- as.Date(dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      if(all(is.na(datesi))) datesi <- as.Date(dates, format = "%Y-%m-%d", tz = "UTC")
      disc <- diff(datesi)
      datesi <- NULL
      dates <- NULL
      ind <- if (any(disc > 1)) {
            c(0, which(disc > 1))
      } else {
            c(0, length(ts))
      }
      if (threshold.type == "prob") {
            threshold <- quantile(ts, probs = threshold, type = 7, na.rm = TRUE)
      }
      spell.list <- lapply(2:length(ind), function(x) {
            aux <- ts[(ind[x - 1] + 1):ind[x]]
            rle.obj <- eval(parse(text = paste("rle(aux", ineq, "threshold)")))
            output <- rle.obj$lengths[rle.obj$values == TRUE]
            ## Retain spells above min.spell.length:
            if (!is.null(min.spell.length)) {
               ind <- which(output > min.spell.length)
               if (length(ind) > 0L) {
                  output <- output[ind]      
               }
            }
            return(output)
      })
      q <- quantile(do.call("c", spell.list), probs = prob, type = 7, na.rm = TRUE)
      if (is.na(q)) q <- 0
      return(q)
}
