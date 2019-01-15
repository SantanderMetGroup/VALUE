#' @title VALUE index calculation
#' @description Generic funtion to compute VALUE indices from a time series
#' @template templateIndexParams
#' @template templateDates
#' @param indices A character vector of index codes to be computed.
#' @return A named list of output indices
#' @export
#' @examples 
#' obs.dataset <- file.path(find.package("VALUE"), "example_datasets", "VALUE_ECA_86_v2.zip")
#' obs <- loadValueStations(dataset = obs.dataset, var = "tmax", stationID = "000232")
#' ts <- as.vector(obs$Data)
#' dates <- obs$Dates$start
#' valueIndex(ts, dates, indices = c("AC1", "AC2", "FA25", "FA30", "FB15", "AnnualCycleAmp"))

valueIndex <- function(ts, dates, indices) {
    out.list <- lapply(1:length(indices), function(i) {
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
            if (!is.na(as.numeric(aux[2]))) aux.val <- as.numeric(aux[2])
            arg.list[[aux[1]]] <- aux.val
        }
        if (isTRUE(arg.list[["dates"]])) arg.list[["dates"]] <- dates
        do.call(fun, arg.list)
    })
    names(out.list) <- indices
    return(out.list)
}

