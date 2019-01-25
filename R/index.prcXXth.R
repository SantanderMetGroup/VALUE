#' @title Sample quantiles
#' @description Function to compute the sample quantiles for the given probabilities. 
#' @author Neyko Neykov, J. Bedia, D. San-Martín, S. Herrera
#' @param ts A vector containing the data
#' @param prob A float number in the range [0,1] defining the probability of the quantile to be calculated.
#' @param threshold Optional parameter. Remove data below the threshold value.
#'                  Included to consider the case of precipitation. NULL by default.
#' @return A float number corresponding to the quantile.
#' @keywords internal
#' @importFrom stats quantile
#' @export


index.prcXXth <- function(ts, prob = .25, threshold = NULL) {
      if (!is.null(threshold)) ts[ts < threshold] <- NA
      quantile(ts, probs = prob, type = 7, na.rm = TRUE)
}
