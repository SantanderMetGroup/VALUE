#' @title Total amount of precipitation of wet days above percentile
#' @description Function to compute the total amount of precipitation falling in wet days above percentile.
#' @author J. Bedia
#' @param ts A vector containing the data
#' @param prob A float number in the range [0,1] defining the probability of the quantile to be calculated.
#' Default to 0.98
#' @details Wet days are internally defined as those exceeding the 1mm threshold
#' @return A float number corresponding to the proportion/number of days below/above the defined threshold.
#' @export

index.wetAmount.prcXXth <- function(ts, prob = .98) {
      wetdays <- ts[ts > 1]
      q <- quantile(wetdays, prob = prob, na.rm = TRUE)
      sum(wetdays[wetdays >= q])      
}



