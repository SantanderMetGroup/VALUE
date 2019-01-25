#' @importFrom stats var
#' @keywords internal

index.internal.var <- function(ts, dates, type = c("monthly","interannual")) {
    type <- match.arg(type, choices = c("monthly", "interannual"))
    if (type == "monthly") {
        mon <- substr(dates, 1,7)
        ref <- var(tapply(ts, INDEX = mon, FUN = mean, na.rm = TRUE))
    }
    if (type == "interannual") {
        yea <- substr(dates, 1,4)
        ref <- var(tapply(ts, INDEX = yea, FUN = mean, na.rm = TRUE))  
    }
    return(ref)  
}
