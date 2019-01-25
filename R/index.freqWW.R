#' @title Wet-wet probability
#' @description Function to compute the wet-wet-probability index. 
#' @author Neyko Neykov, J. Bedia, D. San-Martín, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the Wet-wet transition probability.
#' @export


index.freqWW <- function(ts, threshold = 1) {
      indToday <- 1:(length(ts) - 1)
      indTomorrow <- 2:length(ts)
      mean((ts[indToday] >= threshold)*(ts[indTomorrow] >= threshold), na.rm = TRUE)
}
