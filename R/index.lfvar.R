#' @title Proportion of variance in low frequency
#' @description Annual and seasonal proportion of variance in low frequency
#' @template templateIndexParams
#' @param lowVarPeriod Low frequency threshold. Minimum cycles per unit time below which frequency is considered as low
#' @param INDEX A Vector defining the aggregation periods (passed to \code{tapply}). No aggregation is performed by default.
#' @author Douglas Maraun
#' @importFrom stats spec.pgram na.exclude
#' @keywords internal
#' @export

index.lfvar <- function(ts, lowVarPeriod = 30, INDEX = 1:length(ts)) {
  mean.x <- tapply(ts, INDEX, mean, na.rm = TRUE)
  if (any(!is.na(mean.x))) {
    specVar <- spec.pgram(mean.x, na.action = na.exclude, plot = FALSE)
    lowfreqvar <- sum(specVar$spec[1/specVar$freq >= lowVarPeriod], na.rm = TRUE)
    totalvar <- sum(specVar$spec, na.rm = TRUE)
    lowfreqvar / totalvar
  } else {
    NA
  }
}


