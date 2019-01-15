#' @title Ratio
#' @description Function to compute the ratio between the predicted and observed means. 
#' The measure depends on the index mean.
#' @author J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateMeasureParams
#' @param dates dates
#' @return A float number corresponding to the ratio.
#' @export

diagnostic.ratio <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates) {
    indexPrd / indexObs
}
