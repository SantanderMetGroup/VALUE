#     subsetVALUE.R Select an arbitrary subset from a VALUE object
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
#
#
#' @title Select an arbitrary subset from a VALUE object along one or more of its dimensions
#' @description Creates a new predictions/observations VALUE object that is a subset of the input object 
#' along the selected dimensions
#' @param valueObj The input object to be subset. This is either an observation, as returned by \code{loadValueStations}, 
#' , or a predictions object as returned by \code{loadStationPredictions}.
#' @param stationID Character string. Station codes.
#' @param members An integer vector indicating \strong{the position} of the members to be subset. Default to NULL.
#' @param season An integer vector indicating the months to be subset. 
#' @param dates A vector of dates for subsetting on arbitrary dates. This is a character (or \code{POSIXct}) vector
#'  following the format \dQuote{YYYY-MM-DD} (i.e., \code{format = "\%Y-\%m-\%d"} as in \code{\link{strptime}}). 
#'  This is directly passed by the element \code{valueObject$Dates$start}, for instance.
#' @param years The years to be selected. Note that this can be either a continuous or discontinuous
#' series of years, the latter option often used in a cross-validation framework.
#'  See details for year-crossing seasons. Default to \code{NULL} (no subsetting is performed on the time dimension).
#' @param lonLim Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees,
#'  of the bounding box defining the subset. For single-station subsets, a numeric value with the
#'  longitude coordinate. If \code{NULL} (default), no subsetting is performed on the longitude dimension
#' @param latLim Same as \code{lonLim} argument, but for latitude.
#' @return A new VALUE stations/predictions object that is a logical subset of the input object along the specified dimensions.
#' @details
#' The attribute \code{subset} will be added to the different slots corresponding to the subset dimensions, taking
#' the value of the subroutine called in each case (e.g.: attribute subset will have the value \code{subsetSpatial}
#' in the xyCoords slot after spatial subsetting...).
#' 
#' \strong{Time slicing by years}
#' 
#' In case of year-crossing seasons (e.g. boreal winter (DJF), \code{season = c(12,1,2)}),
#' the season is assigned to the years of January and February 
#' (i.e., winter of year 2000 corresponds to Dec 1999, Jan 2000 and Feb 2000). Thus, 
#' the \code{years} argument must be introduced accordingly.
#' 
#'  \strong{Spatial slicing}
#'  
#'  Spatial subset definition is done via the \code{lonLim} and \code{latLim} arguments, in the same way as
#'   in the \code{\link{loadValueStations}} function. It is also possible to
#'   make single-station selections from a multi-station object, just by specifying a single coordinate instead of a range
#'    as the argument value. For instance \code{lonLim=c(-10,10)} and \code{latLim=c(35,45)} indicates a
#'  rectangular window centered in the Iberian Peninsula), and single grid-cell values
#'  (for instance \code{lonLim=-3.21} and \code{latLim=41.087} for retrieving the data in the closest grid
#'  point to the point coordinate -3.21E, 41.087N. In both cases, the function
#'  operates by finding the nearest (euclidean distance) stations to the coordinates introduced
#'   (either north/south or east/westward).

#' @author J. Bedia 
#' @export

subsetVALUE <- function(valueObj, 
                        stationID = NULL,
                        members = NULL,
                        season = NULL,
                        dates = NULL,
                        years = NULL,
                        lonLim = NULL,
                        latLim = NULL) {
    .Deprecated(new = "subsetGrid", old = "subsetVALUE",
                msg = "\'subsetVALUE\' is deprecated\nUse \'subsetGrid\' from package transformeR instead\n<https://github.com/SantanderMetGroup/transformeR>")
      if (!is.null(stationID)) valueObj <- subsetVALUE.stations(valueObj, stationID)
      if (!is.null(members)) valueObj <- subsetVALUE.members(valueObj, members)      
      if (!is.null(season)) valueObj <- subsetVALUE.season(valueObj, season)
      if (!is.null(dates)) valueObj <- subsetVALUE.dates(valueObj, dates)
      if (!is.null(years)) valueObj <- subsetVALUE.years(valueObj, years)
      if (!is.null(lonLim) | !is.null(latLim)) valueObj <- subsetVALUE.spatial(valueObj, lonLim, latLim)
      return(valueObj)
}

#'@keywords internal
#'@importFrom abind asub

subsetVALUE.members <- function(valueObj, members = NULL) {
      dimNames <- attr(valueObj$Data, "dimensions")
      if (!("member" %in% dimNames)) {
            warning("No members defined")
            return(valueObj)
      } else {
            mem.ind <- grep("member", dimNames)
            n.mem <- dim(valueObj$Data)[mem.ind]
            if (length(members) > n.mem) stop("Too many members selected")
            if (any(members > n.mem)) stop("Member index selection out of range")
      }
      valueObj$Data <- asub(valueObj$Data, idx = members, dims = mem.ind, drop = FALSE)
      attr(valueObj$Data, "dimensions") <- dimNames
      return(valueObj)
}


#'@keywords internal
#'@importFrom abind asub

subsetVALUE.years <- function(valueObj, years = NULL) {
      dimNames <- attr(valueObj$Data, "dimensions")
      all.years <- getYearsAsINDEX.VALUE(valueObj$Dates$start)
      # aux.year.ind <- match(years, unique(all.years))
      if (length(intersect(years, all.years)) == 0) {
            stop("No valid years for subsetting. The argument \'years\' was ignored")
      }
      if (any(years < min(all.years) | years > max(all.years))) {
            stop("Some subset year boundaries outside the current object extent")
      }
      time.ind <- which(all.years %in% years)
      valueObj$Data <- asub(valueObj$Data, time.ind, grep("time", dimNames), drop = FALSE)
      attr(valueObj$Data, "dimensions") <- dimNames
      valueObj$Dates <- sapply(names(valueObj$Dates), function(x) valueObj$Dates[[x]][time.ind],
                               USE.NAMES = TRUE, simplify = FALSE)
      attr(valueObj$Dates, "subset") <- "subsetYears"
      return(valueObj)
}
# End

#'@keywords internal
#'@importFrom abind asub

subsetVALUE.season <- function(valueObj, season) {
      dimNames <- attr(valueObj$Data, "dimensions")
      # date format yyyy-mm-dd hh:mm:ss is assumed
      # this speeds up the POSIXlt function which uses the slow strptime function
      # providing a format and timezone to POSIXlt helps but this approach is faster
      yrs <- as.numeric(substr(valueObj$Dates$start,1,4))
      mon <- as.numeric(substr(valueObj$Dates$start,6,7))
      season0 <- as.integer(unique(mon))
      season <- as.integer(season)
      if (!all(season %in% season0)) stop("Month selection outside original season values")
      time.ind <- which(mon %in% season)
      if (!identical(season, season0)) {
            # Quita los primeros meses del primer año
            mp <- season[(which(diff(season) < 1) + 1):length(season)]
            a <- which(mon %in% mp & yrs == unique(yrs[1]))
            # Quita los ultimos meses del ultimo año
            mp1 <- season[1:(which(diff(season) < 1))]
            b <- which(mon %in% mp1 & yrs == tail(unique(yrs),1))
            time.ind <- time.ind[-c(a,b)]
      }
      valueObj$Data <- asub(valueObj$Data, time.ind, which("time" == dimNames), drop = FALSE)
      attr(valueObj$Data, "dimensions") <- dimNames
      valueObj$Dates <- sapply(names(valueObj$Dates), function(x) valueObj$Dates[[x]][time.ind],
                               USE.NAMES = TRUE, simplify = FALSE)
      attr(valueObj$Dates, "subset") <- "subsetSeason"
      return(valueObj)
}
# End

#'@keywords internal
#'@importFrom abind asub

subsetVALUE.dates <- function(valueObj, dates = NULL) {
      dimNames <- attr(valueObj$Data, "dimensions")
      # date format yyyy-mm-dd hh:mm:ss is assumed
      filters = format(dates,'%Y-%m-%d %H:%M:%S')
      time.ind <- which(valueObj$Dates$start %in% filters)
      valueObj$Data <- asub(valueObj$Data, time.ind, which("time" == dimNames), drop = FALSE)
      attr(valueObj$Data, "dimensions") <- dimNames
      valueObj$Dates <- sapply(names(valueObj$Dates), function(x) valueObj$Dates[[x]][time.ind],
                               USE.NAMES = TRUE, simplify = FALSE)
      attr(valueObj$Dates, "subset") <- "subsetDates"
      return(valueObj)
}
# End

#'@keywords internal
#'@importFrom abind asub

subsetVALUE.spatial <- function(valueObj, lonLim = NULL, latLim = NULL) {
      dimNames <- attr(valueObj$Data, "dimensions")
      if (!is.null(lonLim)) {
            if (!is.vector(lonLim) | length(lonLim) > 2) {
                  stop("Invalid longitudinal boundary definition")
            }
            lons <- valueObj$xyCoords[ ,"longitude"]
            lon.ind <- which.min(abs(lons - lonLim[1]))
            if (length(lonLim) > 1) {
                  lon2 <- which.min(abs(lons - lonLim[2]))
                  lon.ind <- lon.ind:lon2
            }
            xy.ind <- lon.ind
      }
      if (!is.null(latLim)) {
            if (!is.vector(latLim) | length(latLim) > 2) {
                  stop("Invalid latitudinal boundary definition")
            }
            lats <- valueObj$xyCoords[ ,"latitude"]
            lat.ind <- which.min(abs(lats - latLim[1]))
            if (length(latLim) > 1) {
                  lat2 <- which.min(abs(lats - latLim[2]))
                  lat.ind <- lat.ind:lat2
            }
            xy.ind <- lat.ind
      }
      if (exists("lon.ind") & exists("lat.ind")) xy.ind <- intersect(lon.ind, lat.ind)
      valueObj$xyCoords <- valueObj$xyCoords[xy.ind, ]
      valueObj$Metadata <- sapply(names(valueObj$Metadata), function(x) valueObj$Metadata[[x]][xy.ind],
                                  USE.NAMES = TRUE, simplify = FALSE)
      valueObj$Data <- asub(valueObj$Data, idx = xy.ind, dims = grep("station", dimNames), drop = FALSE)
      attr(valueObj$Data, "dimensions") <- dimNames
      attr(valueObj$xyCoords, "subset") <- "subsetSpatial"
      return(valueObj)
}
# End


#'@keywords internal
#'@importFrom abind asub

subsetVALUE.stations <- function(valueObj, stationID = NULL) {
      dimNames <- attr(valueObj$Data, "dimensions")
      stids <- valueObj$Metadata[["station_id"]]
      if (any(!(stationID %in% stids))) stop("Unrecognized station codes: ",
                                             stationID[which(!(stationID %in% stids))])
      st.ind <- match(stationID, stids)
      valueObj$xyCoords <- valueObj$xyCoords[st.ind, ]
      valueObj$Metadata <- sapply(names(valueObj$Metadata), function(x) valueObj$Metadata[[x]][st.ind],
                                  USE.NAMES = TRUE, simplify = FALSE)
      valueObj$Data <- asub(valueObj$Data, idx = st.ind, dims = grep("station", dimNames), drop = FALSE)
      attr(valueObj$Data, "dimensions") <- dimNames
      attr(valueObj$xyCoords, "subset") <- "subsetStationIDs"
      return(valueObj)
}

