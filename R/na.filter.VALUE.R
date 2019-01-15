#     na.filter.VALUE.R Remove stations above a given proportion of missing data
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

#' @title Remove stations above a given proportion of missing data
#' @description Remove stations over a user-defined proportion of missing data from a VALUE multi-station object, 
#' @param valueObj A VALUE R object, but see Details.
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0, i.e., no missing values are allowed)
#' @return A VALUE object without the stations above the user-defined threshold of missing data
#' @details The function assumes that all dimensions are present (i.e., function dimFix has been applied)
#' @export
#' @author J Bedia

na.filter.VALUE <- function(valueObj, max.na.prop = 0) {
      dimNames <- attr(valueObj$Data, "dimensions")
      if (!identical(dimNames, c("member", "time", "station"))) {
            stop("Incompatible dimensions: consider using dimFix first", call. = FALSE)
      }
      n.stations <- dim(valueObj$Data)[3]
      rm.ind <- rep(NA, n.stations)
      for (i in 1:n.stations) {
            if ((length(which(is.na(valueObj$Data[1,,i]))) / dim(valueObj$Data)[2]) > max.na.prop) rm.ind[i] <- i
      }
      rm.ind <- na.omit(rm.ind)
      if (length(rm.ind) > 0) {
            if (length(rm.ind) == n.stations) stop("No stations available fulfilling the max.na.prop condition", call. = FALSE)
            valueObj$Data <- valueObj$Data[,,-rm.ind,drop = FALSE]
            valueObj$xyCoords <- valueObj$xyCoords[-rm.ind,]
            rm.stids <- valueObj$Metadata$station_id[rm.ind]
            valueObj$Metadata <- sapply(names(valueObj$Metadata), function(x) valueObj$Metadata[[x]][-rm.ind],
                                        USE.NAMES = TRUE,
                                        simplify = FALSE)
            attr(valueObj$Data, "dimensions") <- dimNames
            attr(valueObj, "na.omitted.stationIDs") <- rm.stids
            attr(valueObj, "max.na.prop") <- max.na.prop
            attr(valueObj$Data, "dimensions") <- dimNames
            message("NOTE: ", length(rm.ind), " stations were removed to fulfill the max.na.prop condition")
      }
      return(valueObj)
}
