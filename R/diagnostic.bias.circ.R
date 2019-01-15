#' @title Circular bias
#' @description Function to compute the bias considering circular series
#' @template templateMeasureParams
#' @param dates dates
#' @return A float number corresponding to the circular bias.
#' @author D. San-Martin
#' @export

diagnostic.bias.circ <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates) {
      if (is.null(indexObs) | is.null(indexPrd)) {
            return(NA)
      }
      if (is.na(indexObs) | is.na(indexPrd)) {
            return(NA)
      }
      distance <- abs(indexPrd - indexObs) %% 365
      if (distance > 365/2) {
            distance = 365 - distance
      }
      if (indexObs < indexPrd) {
            distance <- distance * -1
      }
      return(distance)
}
