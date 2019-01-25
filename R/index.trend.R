#' @title Interannual trend
#' @description Computes slope and significance of the interannual linear trend
#' @author J Bedia
#' @template templateIndexParams
#' @template templateDates
#' @param output Desired output. The options are \code{"coef"} for the slope
#' coefficient (default) or \code{"sig"}, which is a binary value indicating whether 
#' the slope coefficient is significant (1) or not (0) at the given \code{sig.level}.
#' @param sig.level Significance level. Default to 0.95. 
#' @return A numeric value with either the slope coefficient (for \code{output = "coef"})
#' or whether it is significant (1) or not (0) (\code{output = "sig"}) at the specified \code{sig.level}
#' @export
#' @keywords internal
#' @importFrom stats lm coef confint.lm qnorm
#' @importFrom RcppEigen fastLm

index.trend <- function(ts, dates, output = c("coef","sig"), sig.level = 0.95) {
    output <- match.arg(output, choices = c("coef","sig"))
    INDEX <- getYearsAsINDEX.VALUE(dates)
    ind <- which(!is.na(INDEX))
    y <- tapply(ts, INDEX = INDEX, FUN = mean, na.rm = TRUE)
    mod <- RcppEigen::fastLm(y ~ as.integer(names(y)), subset = ind)
    if (output == "coef") {
        unname(coef(mod)[2])
    } else {
        q <- mod$se[2] * qnorm(1 - (1 - sig.level) / 2) # SE x 1.96 (0.975 quantile of the normal distrib)
        ub <- coef(mod)[2] + q
        lb <- coef(mod)[2] - q
        ifelse(sign(prod(ub, lb)) == 1, 1, 0)
    }
}




