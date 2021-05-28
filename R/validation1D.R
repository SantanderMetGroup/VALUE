#     validation1D. Atomic functions calling the VALUE validation routines
#     
#     Copyright (C) 2019 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.



#' @title VALUE index calculation
#' @description Atomic function to compute VALUE indices from a time series 
#' @template templateIndexParams
#' @param dates A character (or \code{POSIXct}) vector following the format \dQuote{YYYY-MM-DD}
#'  (i.e., \code{format = "\%Y-\%m-\%d"} as in \code{\link{strptime}}). Note that the dates are not required 
#'  by all indices, so the default is \code{NULL}. The function will yield an error if dates are required but not supplied.
#' @param index.codes A character vector of index codes to be computed.
#' @return A named list of output indices
#' @author J. Bedia
#' @export
#' @examples 
#' data(tmin.det)
#' valueIndex1D(tmin.det$obs, dates = tmin.det$dates,
#'              index.codes = c("AC1", "AC2", "FA25", "FA30", "FB15", "AnnualCycleAmp"))

valueIndex1D <- function(ts, dates = NULL, index.codes, ...) {
    out.list <- vapply(1:length(index.codes), FUN.VALUE = numeric(1), FUN = function(i) {
        arg.list <- list(...)
        arg.list[["ts"]] <- ts
        fun.list <- parseArgumentString(arg.list, code = index.codes[i], dates = dates)
        print(str(fun.list))
        do.call(fun.list$fun, fun.list$arg.list)
    })
    names(out.list) <- index.codes
    return(out.list)
}

#' @title VALUE measure calculation
#' @description Atomic function to compute VALUE measures from predicted and observed indices
#' @template templateMeasureParams
#' @param dates dates
#' @param measure.codes A character vector of measure codes to be computed.
#' @return A named list of output measures
#' @author J. Bedia
#' @export
#' @examples 
#' data(tmin.det)
#' # Some indices, like correlation, are computed upon the raw time series
#' # (i.e. no previous index is computed)
#' valueMeasure1D(indexObs = NULL,
#'                indexPrd = NULL,
#'                obs = tmin.det$obs,
#'                prd = tmin.det$pred,
#'                measure.codes = c("ts.rp", "ts.rs"))
#' 
#' # In some cases, the date information is required, with an error if omitted:
#' 
#' valueMeasure1D(indexObs = NULL,
#'                indexPrd = NULL,
#'                obs = tmin.det$obs,
#'                prd = tmin.det$pred,
#'                dates = tmin.det$dates,
#'                measure.codes = c("ts.rp", "ts.rs","ts.rpY"))
#' 
#' # However, most measures are computed upon previously calculated indices
#' indexObs <- valueIndex1D(ts = tmin.det$obs, index.codes = "AC1")
#' indexPrd <- valueIndex1D(ts = tmin.det$pred, index.codes = "AC1")
#' valueMeasure1D(indexObs = indexObs,
#'                indexPrd = indexPrd,
#'                measure.codes = c("bias", "biasRel","ratio"))



valueMeasure1D <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, dates = NULL, measure.codes) {
    if ((is.null(indexObs) & is.null(indexPrd)) && (is.null(obs) | is.null(prd))) {
        stop("Either obs/pred indices or raw time series must be introduced")        
    } 
    if ((is.null(obs) & is.null(prd)) && (is.null(indexObs) | is.null(indexPrd))) {
        stop("Either obs/pred indices or raw time series must be introduced")
    }
    out.list <- vapply(1:length(measure.codes), FUN.VALUE = numeric(1), FUN = function(i) {
        arg.list <- list("indexObs" = indexObs,
                         "indexPrd" = indexPrd,
                         "obs" = obs,
                         "prd" = prd,
                         "dates" = dates)
        fun.list <- parseArgumentString(arg.list, measure.codes[i], dates)
        do.call(fun.list$fun, fun.list$arg.list)
    })
    names(out.list) <- measure.codes
    return(out.list)
}


#' @title Argument list parsing utility
#' @description Internal helper to produce an argument list and a function call usable by \code{\link{do.call}} after reading metadata with
#' \code{\link{show.indices}} or \code{\link{show.measures}}.
#' @param arg.list A list of arguments in the form key=value
#' @param code Character string indicating the code of the target index or measure
#' @param dates Optional dates vector (depends of the selected index/measure)
#' @return A list with two items: \code{fun}, containing the name of the validation function and \code{arg.list}, a complete list of arguments in the form key=value.
#' @keywords internal
#' @details The function is internally used by \code{\link{valueIndex1D}} and \code{\link{valueMeasure1D}}
#' @importFrom magrittr %>% 
#' @author J Bedia
   
parseArgumentString <- function(arg.list, code, dates) {
    type <- ifelse(grepl("^ts\\.|^bias|^ratio", code), "measures", "indices")
    metadata <- if (type == "measures") {
        show.measures(measure.code = code)
    } else {
        show.indices(index.code = code)
    }
    fun <- gsub("\\(.*", "", metadata$FUNCTION.CALL)
    args <- gsub(paste0(fun,"|\\(|\\)"), "", metadata$FUNCTION.CALL)
    aux.string <- strsplit(args, split = ",") %>% unlist() %>% gsub("\\s", "", .)
    if (length(aux.string) > 0) {
        for (j in 1:length(aux.string)) {
            aux <- unlist(strsplit(aux.string[j], split = "=")) %>% gsub("\\s","", .)
            aux.val <- aux[2]
            if (aux[2] == "TRUE") aux.val <- TRUE
            if (aux[2] == "FALSE") aux.val <- FALSE
            if (!is.na(suppressWarnings(as.numeric(aux[2])))) aux.val <- as.numeric(aux[2])
            if(is.null(arg.list[[aux[1]]])) arg.list[[aux[1]]] <- aux.val
            print(arg.list[[aux[1]]])
        }
    }
    if (isTRUE(arg.list[["dates"]])) {
        if (is.null(dates)) stop("At least one of the requested ", type, " (", code,
                                 ") requires the argument 'dates'", call. = FALSE)
        arg.list[["dates"]] <- dates
    }
    return(list(arg.list = arg.list, fun = fun))
}

