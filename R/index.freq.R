#' @title Relative/Absolute frequencies w.r.t. user-defined thresholds
#' @description Function to compute the relative/absolute amount of days below/above a predefined threshold. 
#' @author Douglas Maraun, J. Bedia, D. San-Martin, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1 (for precipitation).
#' @param condition Inequality operator to be applied considering the given threshold.
#' \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than.
#' @param freq.type A character string indicating if frequecies to be computed are relative (\code{"rel"}) or
#' absolute (counts) (\code{"abs"}).
#' @return A float number corresponding to the proportion/number of days below/above the defined threshold.
#' @export

index.freq <- function(ts, threshold = 1, condition, freq.type) {
      condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
      freq.type <- match.arg(freq.type, choices = c("abs","rel"))
      ineq <- switch(condition,
               "GT" = ">",
               "GE" = ">=",
               "LT" = "<",
               "LE" = "<=")
      len <- 1
      if (freq.type == "rel") len <- length(ts)
      if (all(is.na(ts))) {
         NA
      } else {
         eval(parse(text = paste("sum(ts", ineq, "threshold, na.rm = TRUE) / len")))
      }
}      



