#' @title Bias
#' @description Function to compute the difference between the observed and predicted means. 
#' The measure depends on the index mean.
#' @author J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateMeasureParams
#' @param dates dates
#' @return A float number corresponding to the bias.
#' @export

diagnostic.bias <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates) {
    indexPrd - indexObs
}
