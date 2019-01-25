#' @title Arithmetic mean 
#' @description Function to compute the arithmetic mean index.
#' @author Ole Roessler, J. Bedia, D. San-Martín, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the mean of the input.
#' @export


index.mean <- function(ts) {
      mean(ts, na.rm = TRUE)
}
