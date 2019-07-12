#' @title Roc Skill Score (ROCSS)
#' @description Calculate Roc Skill Score (ROCSS)
#' @author J.Baño-Medina
#' @template templateMeasureParams
#' @param dates dates
#' @return A float number corresponding to the ROCSS
#' @importFrom verification roc.area
#' @export

measure.rocss <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates) {
  if (length(obs) <= 1) stop("Observed time series is needed")
  if (length(prd) <= 1) stop("Predicted time series is needed")
  if (!all(is.na(obs))) {
    tryCatch({
      roc.area(obs = obs, pred = prd)$A*2 - 1
    }, error = function(x) {
      NA
    })
  } else {
    NA
  }
}
