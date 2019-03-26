#' @title Spell length mean
#' @description Computes the mean duration of spells (above/below) a given threshold.
#' @template templateIndexParams
#' @template templateDates
#' @param threshold A float number defining the absolute threshold considered. Default to 1 
#' @param threshold.type Is the value of \code{threshold} an absolute value or a quantile [0-1]?.
#' Two possible values: \code{"abs"} and \code{"prob"} respectively. Default to \code{"abs"}.
#' @param condition Inequality operator to be applied considering the given threshold.
#'  \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than
#' @return A float number with the corresponding percentile.
#' @author N. Neykov, S. Herrera, J. Bedia
#' @details The function requires the date information in order to ensure that spells
#'  are computed on consecutive records.
#' @export

index.spell.mean <- function(ts, dates, threshold = 1, threshold.type = "abs", condition = c("GT", "GE", "LT", "LE")) {
      condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
      threshold.type <- match.arg(threshold.type, choices = c("abs","prob"))
      ineq <- switch(condition,
                     "GT" = ">",
                     "GE" = ">=",
                     "LT" = "<",
                     "LE" = "<=")
      # Index of discontinuities in the data series
      dates <- as.Date(dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      disc <- diff(dates)
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
            rle.obj$lengths[rle.obj$values == TRUE]
      })
      q <- mean(do.call("c", spell.list), na.rm = TRUE)
      return(q)
}
