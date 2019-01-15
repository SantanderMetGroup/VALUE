#     deseason.VALUE.R Remove seasonal cycle from a VALUE dataset
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
#' @title Remove seasonal cycle
#' @description Remove seasonal cycle from a VALUE dataset (possibly using a moving average filter for daily datasets)
#' @param valueObj Any VALUE object (as returned either by \code{\link{loadValueStations}} or \code{\link{loadValuePredictions}}).
#' @param window.width Integer number indicating the width, in days, of the window used for
#'  moving average computation of the reference daily climatology. Default to 31 days. See details.
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0.25). See details
#' @param monthly Logical flag indicating whether the input data is monthly (\code{FALSE}) or daily (\code{TRUE}, the default)
#' @param monthly.clim.fun Optional. Name of the function used to compute the monthly climatology (default to \code{"mean"}). Ignored 
#' if \code{monthly = FALSE}.
#' @param ... Further optional arguments passed to \code{monthly.clim.fun} that computes the climatology (only used if \code{monthly = TRUE}).
#' A typical optional argument is e.g. \code{na.rm = TRUE} passed to the default \code{"mean"}. 
#' @details A (circular) moving average daily climatology can be automatically calculated for each data series, considering 
#' a user-defined window width centered around lag 0. An additional argument allows to specify the maximum number of missing values allowed,
#'  being ommited from the analysis those data series above the threshold.
#' @return A VALUE object same as input but with seasonal cycles removed. 
#' @details For daily datasets, the daily climatology (i.e., one value per (julian) day of the year) is subtracted from each daily value in the time series.
#' This daily climatlogical value can be calculated using a moving average window as described above. FOr monthly datatasets, the monthly climatological mean is
#' computed and subtrated to each monthly record. In this case, the moving wndow average is not applied. 
#' A global attribute "deseason" is included and assigned a value of TRUE, to indicate that the dataset consist of anomalies.
#' An additional attribute "deseason::window.width" is appended to the output indicating the window width (in days) used, when applicable.
#' @note Although the function is insentitive to the input season, it is recommended to perform seasonal cycle removal on
#' the annual series, rather than in particular seasons. Seasonal slices can be selected afterwards using \code{\link{subsetVALUE}}.
#' @author J. Bedia 
#' @export
#' @examples \dontrun{
#' obs.file <- file.path(find.package("VALUE"),"example_datasets","VALUE_ECA_86_v2.zip")
#' # Load 1 station (Innsbruck) as example:
#' stationObj <- loadValueStations(obs.file, var = "tmin", stationID = "000013", years = 1991:1993)
#' # Test different window sizes:
#' widths <- c(31,45,60)
#' out <- lapply(widths, function(x) deseason.VALUE(stationObj, window.width = x))
#' dates <- as.Date(stationObj$Dates$start)
#' plot(dates, stationObj$Data, ty = 'l', ylab = "tmin (degC)")
#' for (i in 1:length(widths)) {
#'       lines(dates,out[[i]]$Data,col = i + 1)
#' }
#' legend("bottomright", legend = widths, lty = 1, col = (1:3)+1, title = "window.width")
#' # Monthly aggregation:
#' mon <- aggregateVALUE(stationObj, aggr.m = list(FUN = "mean", na.rm = TRUE))
#' # Compute anomalies w.r.t. the monthly means:
#' anom <- deseason.VALUE(mon, monthly = TRUE, monthly.clim.fun = "mean", na.rm = TRUE)
#' plot(mon$Data, ty = 'b')
#' lines(anom$Data[1,,], col = "red", ty = "b")
#' legend("topright",
#'        legend = c("monthly tmin", "monthly tmin anomalies"),
#'        lty = 1, col = c("black","red"))
#' }


deseason.VALUE <- function(valueObj, window.width = 31, max.na.prop = .25, monthly = FALSE, monthly.clim.fun = "mean", ...) {
      arg.list <- list(...)
      valueObj <- dimFix(valueObj)
      valueObj <- na.filter.VALUE(valueObj, max.na.prop)
      n.stations <- dim(valueObj$Data)[3]
      n.mem <- dim(valueObj$Data)[1]
      message("[", Sys.time(),"] - Removing seasonal cycle on ", n.stations, " locations...")
      if (!monthly) {
            dates <- as.POSIXlt(valueObj$Dates$start)
            doy <- format(dates, format = "%j")
            fil <- rep(1/window.width, window.width + 1)
            for (i in 1:n.mem) {
                  mat <- valueObj$Data[i,,,drop = FALSE]
                  for (j in 1:n.stations) {
                        # Moving average, circular, centered on lag 0
                        x <- filter(mat[,,j], filter = fil, circular = TRUE, sides = 2)
                        # Daily climatology
                        ref <- tapply(x, INDEX = doy, FUN = mean, na.rm = TRUE)
                        # Remove cycle
                        for (k in 1:length(ref)) {
                              ind <- which(doy == unique(doy)[k])
                              x[ind] <- mat[,ind,j] - ref[k]
                        }
                        valueObj$Data[i,,j] <- x
                  }
            }
            attr(valueObj, "deseason::window.width") <- window.width
      } else {
            dimNames <- attr(valueObj$Data, "dimensions")
            mon <- substr(valueObj$Dates$start, 6, 7)
            monfac <- factor(mon, levels = unique(mon), ordered = TRUE)
            ntimes <- dim(valueObj$Data)[grep("^time", dimNames)]
            for (i in 1:n.mem) {
                  mat <- valueObj$Data[i,,,drop = FALSE]
                  for (j in 1:n.stations) {
                        arg.list[["X"]] <- mat[,,j]
                        arg.list[["FUN"]] <- monthly.clim.fun
                        arg.list[["INDEX"]] <- monfac
                        ref <- do.call("tapply", arg.list)
                        x <- numeric(ntimes)
                        for (k in 1:length(ref)) {
                              ind <- which(monfac == levels(monfac)[k])                  
                              x[ind] <- mat[1,ind,j] - ref[k]
                        }
                        valueObj$Data[i,,j] <- x
                  }
            }
      }
      message("[", Sys.time(), "] - Done.")
      attr(valueObj, "deseason") <- TRUE
      return(valueObj)
}
# End

