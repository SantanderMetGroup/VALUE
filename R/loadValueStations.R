#     loadValueStations.R Load observations data from station datasets in the standard VALUE ASCII format
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

#' @title Load VALUE station data
#' @description Load observations data from station datasets in the standard VALUE ASCII format (See reference URL).
#' @template templateParams 
#' @param stationID Optional. A character vector indicating the code names of the stations to be loaded.
#' @param tz A time zone specification to be used for the conversion of dates, if one is required
#' (i.e., if the time zone of the dataset does not correspond to the system-specific one; see
#' \code{\link[base]{timezones}} for details). Default to unspecified (i.e. \code{tz = ""}).
#' @return a list with the following elements:
#' \itemize{
#' \item \code{Variable}. Name of the variable
#' \item \code{Data}. A 2-D matrix containing the data. Dates are ordered by rows and Stations by columns, 
#' following the order indicated in the \code{Metadata}.
#' \item \code{xyCoords}. A 2-D matrix with longitude and latitudes of the stations
#' \item \code{Dates}. A list with the verification time interval of each record in the time series.
#'  This is represented by a list with two elements: \code{start} and \code{end}, representing the
#'  lower and upper bounds of the verification period
#' \item \code{Metadata}. A list of variable length depending on the available metadata associated
#' to each observation. If no metadata are provided, at least the station codes (compulsory) are displayed.
#' }
#' @template templateGeolocation
#' @note Unlike gridded datasets, station data do not use a dictionary for variable homogenization. Thus, users
#' must take care of variable units and eventual conversions when necessary.
#' @references VALUE's standard ASCII format reference: 
#' \url{http://meteo.unican.es/work/loadeR/wiki/Rmd_html/OformatStationData.html} 
#' \url{http://stackoverflow.com/questions/12460938/r-reading-in-a-zip-data-file-without-unzipping-it}
#' @export
#' @importFrom utils unzip
#' @author J. Bedia 
#' @family loading
#' @examples \dontrun{
#' obs.dataset <- file.path(find.package("VALUE"), "example_datasets", "VALUE_ECA_86_v2.zip")
#' # All data
#' obs <- loadValueStations(dataset = obs.dataset, var = "tmean")
#' # Selection by lon-lat
#' obs1 <- loadValueStations(dataset = obs.dataset, var = "tmax", lonLim = -3, latLim = 42)
#' obs1$Metadata
#' # Selection by station code
#' obs2 <- loadValueStations(dataset = obs.dataset, var = "tmax", stationID = "000232")
#' obs2$Metadata
#' # Several stations within a rectangular domain
#' obs3 <- loadValueStations(obs.dataset, "precip", lonLim = c(-5, 2), latLim = c(38, 43))
#' obs3$Metadata
#' # Winter data for 1991-2000
#' djf <- loadValueStations(dataset = obs.dataset, var = "tmin", season = c(12, 1, 2),
#'  years = 1991:2000)
#' # Note that winter 1991 encompasses Dec 1990 + Jan 1991 + Feb 1991 (year-crossing season)
#' }

loadValueStations <- function(dataset, var, stationID = NULL, lonLim = NULL, latLim = NULL, season = NULL, years = NULL, tz = "") {
    .Deprecated("loadStationData", package = "loadeR", old = "loadValueStations",
                msg = "loadValueStations is deprecated and will be eventually removed from VALUE.\nPlease use loadStationData from package loadeR instead (<https://github.com/SantanderMetGroup/loadeR>)")
    zipFileContents <- unzip(dataset, list = TRUE)$Name
    if ((!is.null(lonLim) | !is.null(latLim)) & !is.null(stationID)) { 
        lonLim <- NULL 
        latLim <- NULL
        warning("lonLim/latLim arguments ignored as Station Codes have been specified.", call. = FALSE)
    }
    # Reading stations from zip file
    stations.file <- grep("stations\\.", zipFileContents, ignore.case = TRUE, value = TRUE)
    if (any(grepl("MACOSX", stations.file))) {
        stations.file <- stations.file[-grep("MACOSX", stations.file)]
    }      
    aux <- read.csv(unz(dataset, stations.file), stringsAsFactors = FALSE, strip.white = TRUE)
    # Station codes
    stids <- read.csv(unz(dataset, stations.file), colClasses = "character")[ ,grep("station_id", names(aux), ignore.case = TRUE)]
    if (!is.null(stationID)) {
        stInd <- match(stationID, stids)
        if (any(is.na(stInd))) {
            stop("'stationID' values not found.\nCheck data inventory", call. = FALSE)
        }
    } else {
        stInd <- 1:length(stids)
    }
    ## Longitude and latitude
    lons <- aux[ , grep("^longitude$", names(aux), ignore.case = TRUE)]
    lats <- aux[ , grep("^latitude$", names(aux), ignore.case = TRUE)]
    if (!is.null(lonLim)) {
        latLon <- getLatLonDomainValueStations(lonLim, latLim, lons, lats)
        if (length(latLon$stInd) == 0) {
            stop("No stations were found in the selected spatial domain", call. = FALSE)
        }
        stInd <- latLon$stInd
        coords <- latLon$stCoords
        latLon <- NULL
    } else {
        coords <- matrix(cbind(lons, lats)[stInd, ], ncol = 2)
    }
    stids <- stids[stInd]
    dimnames(coords) <- list(stids, c("longitude", "latitude"))
    ## Time dimension
    # TODO - fix potential MACOSX errors
    fileInd <- grep(paste(var, "\\.txt", sep = ""), zipFileContents)
    if (length(fileInd) == 0) {
        stop("[", Sys.time(),"] Variable requested not found", call. = FALSE)
    }
    timeString <- read.csv(unz(dataset, zipFileContents[fileInd]), colClasses = "character")[ ,1]
    timeDates <- string2date(timeString, tz = tz)
    timeString <- NULL
    timePars <- getTimeDomainValueStations(timeDates, season, years)
    ## missing data code
    vars <- read.csv(unz(dataset, zipFileContents[grep("variables", zipFileContents, ignore.case = TRUE)]))
    miss.col <- grep("missing_code", names(vars), ignore.case = TRUE)
    if (length(miss.col) > 0) {
        na.string <- vars[grep(var, vars[ , grep("variable", names(vars), ignore.case = TRUE)]), miss.col]
        vars <- NULL
        miss.col <- NULL
    } else {
        na.string <- NA
    }
    # Data retrieval
    message("[", Sys.time(), "] Loading data ...", sep = "")
    trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    var.stids <- lapply(strsplit(readLines(unz(dataset, zipFileContents[fileInd]), 1), split = ", "), FUN = trim)
    var.stids <- tail(unlist(var.stids), -1)
    closeAllConnections() 
    stInd.var <- match(stids, var.stids)
    Data <- unname(as.matrix(read.csv(unz(dataset, zipFileContents[fileInd]), na.strings = na.string)[timePars$timeInd, stInd.var + 1]))
    # Metadata
    message("[", Sys.time(), "] Retrieving metadata ...", sep = "")
    # Assumes that at least station ids must exist, and therefore meta.list is never empty
    ind.meta <- c(1:length(names(aux)))[-pmatch(c("longitude", "latitude", "station_id"), names(aux))]
    meta.list <- list()
    meta.list[[1]] <- stids
    for (i in 1:length(ind.meta)) {
        meta.list[[i + 1]] <- aux[stInd, ind.meta[i]]
    }
    names(meta.list) <- c("station_id", names(aux)[ind.meta])
    aux <- NULL  
    out <- list("Variable" = list("varName" = var), "Data" = Data, "xyCoords" = coords, "Dates" = timeBoundsValue(timePars$timeDates, tz), "Metadata" = meta.list)
    attr(out$Data, "dimensions") <- c("time", "station")
    message(paste("[", Sys.time(), "] Done.", sep = ""))
    return(out)
}
# End      



#' @title Define geolocation of station datasets
#' 
#' @description Given the geolocation arguments, finds the index positions and coordinates of the 
#' requested data, either single point loactions (finds closest --euclidean-- point) or
#' data within the given bounding box.
#' 
#' @param lonLim Numeric. Definition of geolocation in X 
#' @param latLim Numeric. Definition of geolocation in Y
#' @param lons Numeric. All available X coordinates in dataset 
#' @param lats Numeric. All available Y coordinates in dataset 
#' 
#' @return A list with index positions of the requested data and a 2D matrix
#' of XY coordinates
#' 
#' @author J. Bedia 
#' 
#' @keywords internal

getLatLonDomainValueStations <- function(lonLim, latLim, lons, lats) {
    if (length(lonLim) > 2 | length(latLim) > 2) {
        stop("Invalid definition of geographical position", call. = FALSE)
    }
    if (length(lonLim) != length(latLim)) {
        stop("Invalid definition of geographical position", call. = FALSE)
    }
    if (length(lonLim) == 2) {
        lonInd <- which(lons >= lonLim[1] & lons <= lonLim[2])      
        latInd <- which(lats >= latLim[1] & lats <= latLim[2])
        stInd <- intersect(lonInd, latInd)
    } else {
        stInd <- which.min(sqrt((lons - lonLim) ^ 2 + (lats - latLim) ^ 2))
        message("[", Sys.time(),"] Closest station located at ", 
                round(min(sqrt((lons - lonLim) ^ 2 + (lats - latLim) ^ 2)), digits = 4),
                " spatial units from the specified [lonLim,latLim] coordinate") 
        
    }
    return(list("stInd" = stInd, "stCoords" = as.matrix(cbind(lons[stInd], lats[stInd]))))
}
# End



#' @title Time  index positions for station dataset selections
#' @description Get time index positions for loading VALUE ascii station data
#' @param timeDates a POSIXlt vector of time dates
#' @param season A vector of months defining the season selected
#' @param years A vector of (continuous) year selection
#' @return A list with a vector of time index positions and the corresponding POSIXlt dates
#' @author J. Bedia 
#' @keywords internal

getTimeDomainValueStations <- function(timeDates, season, years) {
    if (is.null(season)) {
        season <- 1:12
    }
    allYears <- unique(timeDates$year + 1900)
    startYear <- head(allYears, 1L)
    endYear <- tail(allYears, 1L)
    if (is.null(years)) {
        years <- allYears
    } 
    if (years[1] < startYear & tail(years, 1L) > endYear) {
        warning("Year selection out of dataset range. Only available years will be returned", call. = FALSE)
        years <- allYears
    }
    if (years[1] < startYear) {
        warning("First year in dataset: ", startYear,". Only available years will be returned", call. = FALSE)
        years <- startYear:years[length(years)]
    }
    if (tail(years, 1L) > endYear) {
        warning("Last year in dataset: ", endYear,". Only available years will be returned", call. = FALSE)
        years <- years[1]:endYear
    }
    # Year-crossing seasons
    if (!identical(season, sort(season))) {
        if (years[1] == startYear) { 
            warning(paste("First forecast day in dataset: ", timeDates[1], ".\nRequested seasonal data for ", startYear," not available", sep = ""),
                    call. = FALSE)
            years <- years[-length(years)]
        } else {
            years <- append(years[1] - 1, years)
        }
        timeInd <- which((timeDates$year + 1900) %in% years & (timeDates$mon + 1) %in% season)
        crossSeason <- which(c(1, diff(season)) < 0)
        rm.ind <- which((timeDates$mon + 1) %in% season[1:(crossSeason - 1)] & (timeDates$year + 1900) %in% years[length(years)])
        if (length(years) > 1) {
            rm.ind <- c(rm.ind, which((timeDates$mon + 1) %in% season[crossSeason:length(season)] & (timeDates$year + 1900) %in% years[1]))
        }
        timeInd <- setdiff(timeInd, rm.ind)
    }else{
        timeInd <- which((timeDates$year + 1900) %in% years & (timeDates$mon + 1) %in% season)
    }  
    timeDates <- timeDates[timeInd]
    return(list("timeInd" = timeInd, "timeDates" = timeDates))
}
