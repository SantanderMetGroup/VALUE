#' @title Variance
#' @description Function to compute the variance index. 
#' @author Ole Roessler, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the variance of the input.
#' @export

index.var <- function(ts) {
      var(ts, na.rm = TRUE)
}
