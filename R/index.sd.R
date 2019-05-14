#' @title Standard deviation
#' @description Function to compute the (quasi) standard deviation index.
#' @author Ole Roessler, J. Bedia, D. San-Martín, S. Herrera
#' @param ts A vector containing the data
#' @importFrom stats sd 
#' @keywords internal
#' @return A float number corresponding to the standard deviation of the input sample.
#' @export

index.sd <- function(ts) {
    sd(ts, na.rm = TRUE)
}

