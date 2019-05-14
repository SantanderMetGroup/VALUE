#     loadStationPredictions.R Load a user-defined spatio-temporal slice from a VALUE predictions dataset
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
#' 
#' @title Load VALUE predictions data
#' @description Load predictions data, either deterministic (one single txt file) or stochastic (several
#'  realizations stored in sepparate txt files, bundled in a single zip file).
#' @param stationObj A station data object returned by \code{\link{loadValueStations}}. See details.
#' @param predictions.file Path to the file (either text or zip) containing the predictions.
#' @param tz Optional. A time zone specification to be used for the conversion of dates. See more details in 
#' \code{\link{loadValueStations}}.
#' @param na.strings Optional. A character vector of strings which are to be interpreted as \code{\link{NA}} values.
#'  Blank fields are also considered to be missing values in logical, integer, numeric and complex fields. This argument is passed to read.csv. 
#' @param n.mem Optional An integer value greater than zero indicating the number of members to be loaded. Default to \code{NULL},
#' so all the realizations are loaded. If n.mem is specified, the first \code{n.mem} members will be returned
#' (i.e., from the 1st to the n.mem-th).
#' @return A predictions object. This is equivalent to the stations object (see \code{\link{loadValueStations}}),
#' but the data element may vary its shape to include the \code{"member"} dimension in case of stochastic
#' predictions with several realizations. Also, a global attribute \code{"datatype"} is set, and assigned 
#' the value \code{"predictions"} to differentiate it from observations data.
#' @details The idea of this function is to rely on the previously loaded observations data, so all
#'  the information required to define the predictions subset (time and station codes) is retrieved from 
#'  the observations data subset.
#' @export
#' @importFrom utils unzip read.csv 
#' @importFrom stats na.omit
#' @importFrom abind abind
#' @importFrom transformeR getSeason getYearsAsINDEX     
#' @importFrom loadeR string2date getTimeDomainStations timeBoundsValue
#' @family loading
#'@examples  \dontrun{
#' #Example predictions
#' obs.dataset <- file.path(find.package("VALUE"), "example_datasets", "VALUE_ECA_86_v2.zip")
#' obs <- loadStationData(obs.dataset, "tmin", season = 6:8, years = 2001)
#' # Loading deterministic predictions
#' pred.file1 <- file.path(find.package("VALUE"),
#'                         "example_datasets",
#'                         "example_predictions_tmin_portal_exp1a_deterministic.zip")
#' pred <- loadStationPredictions(obs, pred.file1)
#' str(pred$Data) # 2D array
#' # Loading stochastic predictions (several realizations)
#' pred.file2 <- file.path(find.package("VALUE"),
#'                         "example_datasets",
#'                         "example_predictions_tmin_portal_exp1a_stochastic.zip")
#' pred2 <- loadStationPredictions(obs, pred.file2)
#' str(pred2$Data) # 3D array with 'member' dimension
#' # Selecting the first 2 members using 'n.mem':
#' pred3 <- loadStationPredictions(obs, pred.file2, n.mem=2)
#' str(pred3$Data) # 3D array with 'member' dimension of length 2
#' }

# 
# library(loadeR)
# library(transformeR)
# library(abind)
# 
# stationObj <- obs
# predictions.file <- pred.file2
# tz = ""
# na.strings = "NA"
# n.mem = NULL

loadStationPredictions <- function(stationObj, predictions.file, tz = "", na.strings = "NA", n.mem = NULL) {
    if (!is.null(n.mem)) {
        stopifnot(n.mem >= 1)
    }
    message("[", Sys.time(), "] Loading prediction data ...", sep = "")
    stationObj$Data <- NULL
    season <- transformeR::getSeason(stationObj) ## transformer
    years <- unique(transformeR::getYearsAsINDEX(stationObj))
    stids <- stationObj$Metadata$station_id
    dataset <- predictions.file
    # Multimember case, ZIP file
    if (grepl("\\.zip$", dataset)) {
        zipFileContents <- unzip(dataset, list = TRUE)$Name
        present.files <- sapply(zipFileContents, function(zipFileContent) {
            con <- unz(dataset, zipFileContent)
            a = length(readLines(con, n = 1)) > 0
            close.connection(con)
            return(a)
        })
        zipFileContents  <- zipFileContents[present.files]
        n.members <- length(zipFileContents)
        if (!is.null(n.mem)) {
            if (n.mem > n.members) {
                warning("'n.mem' greater than actual number of realizations. The argument was ignored")
                n.mem <- n.members
            } else {
                n.members <- n.mem
            }
        }      
        timeString <- read.csv(unz(dataset, zipFileContents[1]), colClasses = "character")[ , 1]
        timeDates <- loadeR::string2date(timeString, tz) ############
        timeString <- NULL
        timePars <- loadeR::getTimeDomainStations(timeDates, season, years) #################
        # timePars <- getTimeDomainValueStations(timeDates, season, years)
        if (length(intersect(timePars$timeDates, as.POSIXlt(stationObj$Dates$start))) == 0) {
            stop("Temporal mismatch between predictions and observations")
        }
        con <- unz(dataset, zipFileContents[1])
        header <- readLines(con, n = 1)
        close.connection(con)
        header <- gsub("\\s|\"", "", header)
        header <- unlist(strsplit(header, split = ","))
        colNums <- match(header, stids)
        colNums <- which(!is.na(colNums))
        member.list <- lapply(1:n.members, function(x) {
            con <- unz(dataset, zipFileContents[x])
            read.csv(con, na.string = na.strings, strip.white = TRUE)[timePars$timeInd, colNums]
        })
        aux <- drop(do.call("abind", c(member.list, along = -1)))
        member.list <- NULL
    } else {# txt file      
        n.members = 1
        timeString <- read.csv(dataset, colClasses = "character")[ , 1]      
        timeDates <- loadeR::string2date(timeString, tz) #################
        timeString <- NULL
        timePars <- loadeR::getTimeDomainStations(timeDates, season, years)
        if (length(intersect(timePars$timeDates, as.POSIXlt(stationObj$Dates$start))) == 0) {
            stop("Temporal mismatch between predictions and observations")
        }
        header <- readLines(dataset, n = 1)
        header <- gsub("\\s", "", header)
        header <- unlist(strsplit(header, split = ","))
        colNums <- match(header, stids)
        colNums <- which(!is.na(colNums))
        aux <- as.matrix(read.csv(dataset, na.strings = na.strings)[timePars$timeInd, colNums])
    }
    # Set the dimensions attribute
    dimensions <- if (length(dim(aux)) == 2) {
        c("time", "loc")
    } else {
        c("member", "time", "loc")
    }
    aux <- unname(aux)
    aux[which(aux == suppressWarnings(as.numeric(na.strings)), arr.ind = TRUE)] <- NA
    attr(aux, "dimensions") <- dimensions
    stationObj$Data <- aux
    aux <- NULL
    gc()
    stationObj$Dates <- loadeR::timeBoundsValue(timePars$timeDates, tz)
    ind.st <- na.omit(match(header[-1], stationObj$Metadata$station_id))
    stationObj$xyCoords <- stationObj$xyCoords[ind.st, ]
    stationObj$Metadata <- sapply(names(stationObj$Metadata), function(x) stationObj$Metadata[[x]][ind.st],
                                  USE.NAMES = TRUE, simplify = FALSE)
    attr(stationObj, "datatype") <- "predictions"
    message(paste("[", Sys.time(), "] Prediction loaded.", sep = ""))
    return(stationObj)
}
# End
