#' @title Relative bias
#' @description Function to compute the difference between the relative bias between the predicted and observed means. 
#' The measure depends on the index mean.
#' @author J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateMeasureParams
#' @param dates dates
#' @return A float number corresponding to the bias.
#' @export

measure.bias.rel <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates) {
    (indexPrd - indexObs) / indexObs
}
