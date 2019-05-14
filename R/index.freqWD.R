#' @title Wet-dry probability
#' @description Function to compute the wet-dry probability index. 
#' @author Neyko Neykov, J. Bedia, D. San-Martín, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered.
#' @return A float number corresponding to the wet-dry transition probability.
#' @export

# Transition probabilities: Wet-Dry
index.freqWD <- function(ts, threshold = 1) {
      indToday <- 1:(length(ts) - 1)
      indTomorrow <- 2:length(ts)
      mean((ts[indToday] >= threshold)*(ts[indTomorrow] < threshold), na.rm = TRUE)
}
