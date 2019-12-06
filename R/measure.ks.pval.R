# @useDynLib VALUE
# NULL


#' @title Compute p-value of the two sample Kolmogorov-Smirnov test 
#' @description Function to compute the p-value of the K-S test, optionally performing a correction by the effective sanmple size
#' @template templateMeasureParams
#' @param corrected Logical flag. SHound the p-value be corrected by the effective sample size?. Default to \code{TRUE}
#' @param dates Ignored. Introduced for compatibility with the rest of measures
#' @return A float number corresponding to the p-value of the K-S test
#' @seealso The atomic function \code{\link{measure.ks}}, returning the KS statistic
#' @details The two-sample Kolmogorov-Smirnov test has the null hypothesis (H0) that x and y were drawn from the same continuous distribution.
#' Therefore, the null hypothesis can be rejected only when p-values obtained are \dQuote{small} (i.e. < 0.05 with ci=0.95). Larger values will indicate
#' the H0 can't be rejected.
#' Since the daily time series often used are serially correlated, this function calculates their effective sample size before estimating the p value of the
#' KS statistic in order to avoid the inflation of type I error (i.e. erroneous rejection of the H0). Under the assumption that the underlying time
#' series follow a first-order autoregressive process (Wilks 2006), the effective sample size, neff is defined as follows: neff=n(1-p1)/(1+p1), where p1 is the 
#' lag-1 autocorrelation coefficient.
#'  
#' @author J. Bedia, S. Brands
#' @keywords internal
#' @references Wilks, D. (2006) Statistical methods in the atmospheric sciences, 2nd ed. Elsevier, Amsterdam
#' @import stats
#' @export

measure.ks.pval <- function(indexObs = NULL, indexPrd = NULL, obs, prd, dates = NULL, corrected = TRUE) {
    x <- prd[!is.na(prd)]
    y <- obs[!is.na(obs)]
    KSstatistic <- suppressWarnings(valueMeasure1D(obs = y, prd = x, measure.codes = "ts.ks"))
    if (corrected) {
        x.acf1 <- valueIndex1D(ts = x, index.codes = "AC1")
        n.x = unname(length(x)*((1 - x.acf1)/(1 + x.acf1)))
        y.acf1 <- valueIndex1D(ts = y, index.codes = "AC1")
        n.y = unname(length(y)*((1 - y.acf1)/(1 + y.acf1)))
        pval <- 1 - .Call(stats:::C_pSmirnov2x, KSstatistic, n.x, n.y)    
    } else {
        pval <- unname(ks.test(y, x)$p.value)
    }
    return(pval)
}
    
