#' @title Sum
#' @description Function to compute the sum index.
#' @author Ole Roessler, J. Bedia, D. San-Martín, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the sum of the input.
#' @export


index.sum <- function(ts) {
      sum(ts, na.rm = TRUE)
}
