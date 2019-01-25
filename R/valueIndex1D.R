#' @title VALUE index calculation
#' @description Atomic function to compute VALUE indices from a time series 
#' @template templateIndexParams
#' @param dates A character (or \code{POSIXct}) vector following the format \dQuote{YYYY-MM-DD}
#'  (i.e., \code{format = "\%Y-\%m-\%d"} as in \code{\link{strptime}}). Note that the dates are not required 
#'  by all indices, so the default is \code{NULL}. The function will yield an error if dates are required but not supplied.
#' @param indices A character vector of index codes to be computed.
#' @return A named list of output indices
#' @export
#' @examples \dontrun{
#' obs.dataset <- file.path(find.package("VALUE"), "example_datasets", "VALUE_ECA_86_v2.zip")
#' library(loadeR)
#' obs <- loadStationData(dataset = obs.dataset, var = "tmax", stationID = "000232")
#' ts <- as.vector(obs$Data)
#' dates <- obs$Dates$start
#' valueIndex1D(ts, dates = dates, indices = c("AC1", "AC2", "FA25", "FA30", "FB15", "AnnualCycleAmp"))
#' # In the following example the same set of indices are calculated for the VALUE-ECA86
#' # dataset of 86 stations across Europe:
#' obs2 <- loadStationData(dataset = obs.dataset, var = "tmax")
#' str(obs2$Data)
#' a <- apply(obs2$Data, MARGIN = 2, "valueIndex1D",
#'            dates = dates,
#'            indices = c("AC1", "AC2", "FA25", "FA30", "FB15", "AnnualCycleAmp"))
#' # The result is a matrix of 6 rows (indices) and 86 columns (stations):
#' str(a)            
#' }

valueIndex1D <- function(ts, dates = NULL, indices) {
    out.list <- vapply(1:length(indices), FUN.VALUE = numeric(1), FUN = function(i) {
        arg.list <- list()
        arg.list[["ts"]] <- ts
        meta <- show.indices(index.code = indices[i])
        fun <- gsub("\\(.*", "", meta$FUNCTION.CALL)
        args <- gsub(paste0(fun,"|\\(|\\)"), "", meta$FUNCTION.CALL)
        aux.string <- strsplit(args, split = ",") %>% unlist()
        for (j in 1:length(aux.string)) {
            aux <- unlist(strsplit(aux.string[j], split = "=")) %>% gsub(" ","", .)
            aux.val <- aux[2]
            if (aux[2] == "TRUE") aux.val <- TRUE
            if (aux[2] == "FALSE") aux.val <- FALSE
            if (!is.na(suppressWarnings(as.numeric(aux[2])))) aux.val <- as.numeric(aux[2])
            arg.list[[aux[1]]] <- aux.val
        }
        if (isTRUE(arg.list[["dates"]])) {
            if (is.null(dates)) stop("At least one of the requested indices (",
                                     indices[i],
                                     ") requires the argument 'dates'", call. = FALSE)
            arg.list[["dates"]] <- dates
        } 
        do.call(fun, arg.list)
    })
    names(out.list) <- indices
    return(out.list)
}

