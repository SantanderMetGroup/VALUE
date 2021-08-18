#' @title Annual spell length percentiles
#' @description Computes a user-defined percentile for the duration of spells (above/below) a given threshold, on an annual basis.
#' @template templateIndexParams
#' @template templateDates
#' @param threshold A float number defining the threshold considered. Default to 1
#' @param threshold.type Is the value of \code{threshold} an absolute value or a quantile [0-1]?.
#'  Two possible values: \code{"abs"} and \code{"prob"} respectively. Default to \code{"abs"}.
#' @param condition Inequality operator to be applied considering the given threshold.
#'  \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than
#' @param prob A float number in the range [0,1] defining the probability of the quantile to be calculated.
#'  Default to median (\code{prob = 0.5}).
#' @return A float number with the corresponding percentile.
#' @author J. Bedia, J.M. Gutierrez
#' @details The function requires the date information in order to ensure that spells
#'  are computed on consecutive records.
#' @export

index.spellmax.annual.prcXXth <- function(ts, dates, threshold = 1, threshold.type = "abs", condition = c("GT", "GE", "LT", "LE"), prob = .5) {
      condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
      threshold.type <- match.arg(threshold.type, choices = c("abs","prob"))
      ineq <- switch(condition,
                     "GT" = ">",
                     "GE" = ">=",
                     "LT" = "<",
                     "LE" = "<=")
      if (threshold.type == "prob") {
            threshold <- quantile(ts, probs = threshold, type = 7, na.rm = TRUE)
      }
      yrs <- substr(dates,1,4)
      datesi <- as.Date(dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      if(all(is.na(datesi))) datesi <- as.Date(dates, format = "%Y-%m-%d", tz = "UTC")
      uyrs <- unique(yrs)
      annual.values <- sapply(1:length(uyrs), function(x) {
            ind <- which(yrs == uyrs[x]) 
            disc <- diff(datesi[ind])
            aux <- ts[ind]
            ind2 <- if (any(disc > 1)) {# Index of discontinuities in the data series
                  c(0, which(disc > 1))
            } else {
                  c(0, length(aux))
            }
            spell.list <- lapply(2:length(ind2), function(y) {
                  aux2 <- aux[(ind2[y - 1] + 1):ind2[y]]
                  rle.obj <- eval(parse(text = paste("rle(aux2", ineq, "threshold)")))
                  rle.obj$lengths[rle.obj$values == TRUE]
            })
            max(do.call("c", spell.list), na.rm = TRUE)
      })
      q <- quantile(annual.values, probs = prob, type = 7, na.rm = TRUE)
      if (is.na(q)) q <- 0
      return(q)
}
