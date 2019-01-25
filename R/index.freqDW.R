#' @title Dry-wet probability
#' @description Function to compute the dry-wet probability index. Missing values are ignored.
#' @author Neyko Neykov, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the dry-wet transition probability.
#' @export

index.freqDW <- function(ts, threshold = 1){
      indToday <- 1:(length(ts) - 1)
      indTomorrow <- 2:length(ts)
      mean((ts[indToday] < threshold)*(ts[indTomorrow] >= threshold), na.rm = TRUE)
}
