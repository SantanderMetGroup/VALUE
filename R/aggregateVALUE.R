#     aggregateVALUE.R Grid aggregation along selected dimensions
#
#     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
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


#' @title Aggregation of value objects along selected dimensions
#' @description Aggregates a grid along the target dimensions through aggregation function specification.
#' @param valueObj A VALUE object to be aggregated.
#' @param aggr.m Monthly aggregation function. A list indicating the name of the
#'  aggregation function in first place, and other optional arguments to be passed to the aggregation function. See the examples.
#' @return A VALUE object aggregated along the chosen dimension(s).
#' @details
#'
#' Currently only the temporal monthly aggregation is implemented
#'
#' @author J. Bedia
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' obs.file <- file.path(find.package("VALUE"),
#'                       "example_datasets",
#'                       "VALUE_ECA_86_v2.zip")
#' tmin.DJF.daily <- loadValueStations(obs.file, "tmin",
#'                                     season = c(12,1,2), years = 1970:2008)
#' # Monthly means:
#' tmin.DJF.monthlyMean <- aggregateVALUE(tmin.DJF.daily, aggr.m = list(FUN = "mean", na.rm = TRUE))
#' # Other functions can be introduced by the user. E.g., monthly median:
#' tmin.DJF.monthlyMedian <- aggregateVALUE(tmin.DJF.daily,
#'                                        aggr.m = list(FUN = "quantile", probs = .5, na.rm = TRUE))
#'                                        }


aggregateVALUE <- function(valueObj, aggr.m = list("FUN" = "mean", na.rm = TRUE)) {
    o <- valueObj
    yrmon <- substr(o$Dates$start,1,7)
    yrmon <- factor(yrmon, levels = unique(yrmon), ordered = TRUE)
    dimNamesRef <- attr(o$Data, "dimensions")
    mar <- grep("^time", dimNamesRef, invert = TRUE)
    aggr.m[["INDEX"]] <- yrmon
    o$Data <- unname(apply(o$Data, MARGIN = mar, FUN = function(x) {
        aggr.m[["X"]] <- x
        do.call("tapply", aggr.m)
    }))
    attr(o$Data, "dimensions") <- c("time", dimNamesRef[mar])
    o <- dimFix(o)
    o$Data <- aperm(o$Data, perm = match(attr(o$Data, "dimensions"), c("member","time","station")))
    attr(o$Data, "dimensions") <- c("member","time","station")
    o$Dates$start <- unname(tapply(o$Dates$start, INDEX = yrmon, FUN = "min"))
    o$Dates$end <- unname(tapply(o$Dates$end, INDEX = yrmon, FUN = "max"))
    attr(o$Variable, "aggr.m") <-aggr.m$FUN
    return(o)
}
