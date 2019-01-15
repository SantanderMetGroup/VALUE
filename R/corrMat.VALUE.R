#     corrMat.VALUE.R Correlation matrix for paper Figures 4 and 5 on spatial validation
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
#' @title Correlation matrix for paper Figures 4 and 5 on spatial validation
#' @description Computes the cross correlation matrices between stations that serve as input for plotting functions
#' @param stationObj An R-VALUE object containing station data (as returned by \code{\link{loadValueStations}}).
#' @param predictionObj A R-VALUE predictions object as loaded by \code{\link{loadValuePredictions}}.
#'  Default to NULL, meaning that the correlation matrix is done on the observations.
#' @param season Character string indicating the target season. Accepted values are 
#' \code{c("annual", "DJF", "MAM", "JJA", "SON")}. Several choices in the same request are accepted (all are computed by default).
#' @param method Correlation method, passed to the \code{\link{cor}} function. Default is \code{"spearman"}, but
#' \code{"pearson"} and \code{"kendall"} are also accepted.
#' @param type Type of aggregation in the case of multiple realizations. Should the aggregation of 
#' multiple members be performed \code{"before"} or \code{"after"} (the default) computing the correlations?. Ignored
#'  in the case of observations and deterministic predictions.
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0.25). See details
#' @param deseason Logical. Should the seasonal cycle from the input data series be removed?. See details.
#' @param window.width Used if \code{deseason = TRUE}. Integer number indicating the width, in days, of the window used for
#'  moving average computation of the reference daily climatology. Default to 31 days. See details.
#' @details 
#' 
#' \strong{Seasonal cycle removal}
#' 
#' A (circular) moving average daily climatology can be automatically calculated for each data series, considering 
#' a specific window width (31 days by default, centered around lag 0). This is used for removing the seasonal cycle. 
#' Alternatively, the \code{predictionObj} or \code{stationObj} can be passed to the function adter applying \code{\link{deseason.VALUE}}.
#' 
#' \strong{Missing data treatment}
#' 
#' An additional argument, \code{max.na.prop}, allows to specify the maximum number of missing values allowed, being ommited from the analysis 
#' those data series above the threshold. Note that this argument is applied on a seasonal basis, so correlation
#' matrices for different seasons may differ in their dimension. Missing ata filering can be disabled by setting 
#' the maximum allowed proportion of missing data to 1.
#' 
#' 
#' @return A list of 2D matrices. The length of the list corresponds to the periods indicated in the \code{season} argument (default to 5,
#'  annual and the four standard WMO seasons). Matrix attributes indicate -in the row/column order they appear- the station names, id codes,
#'   and their geographical coordinates. Global attributes provide other method details.   
#' @author J. Bedia 
#' @export
#' @examples \dontrun{
#' obs.file <- file.path(find.package("VALUE"),"example_datasets","VALUE_ECA_86_v2.zip")
#' stationObj <- loadValueStations(obs.file, "precip")
#' predictions.file <- file.path(find.package("VALUE"),"example_datasets",
#'                                   "example_predictions_precip_portal_exp1a_deterministic.zip")
#' prd <- loadValuePredictions(stationObj, predictions.file)                                  
#' # Correlation matrix of annual data:                                  
#' annual <- corrMat.VALUE(stationObj, prd, season = "annual")
#' # Correlation matrix for winter and summer:
#'djfjja <- corrMat.VALUE(stationObj, prd, season = c("DJF","JJA"))
#' }

corrMat.VALUE <- function(stationObj,
                          predictionObj = NULL,
                          season = c("annual", "DJF", "MAM", "JJA", "SON"),
                          method = "pearson",
                          type = "after",
                          max.na.prop = 0.25,
                          deseason = TRUE,
                          window.width = 31) {
      season <- match.arg(season, choices = c("annual", "DJF", "MAM", "JJA", "SON"), several.ok = TRUE)
      method <- match.arg(method, choices = c("pearson", "kendall", "spearman"))
      type <- match.arg(type, choices = c("after", "before"))
      o <- dimFix(stationObj)
      stationObj <- NULL
      if (!is.null(predictionObj)) {
            ## message("[", Sys.time(), "] - Loading predictions...")
            o <- suppressWarnings(dimFix(predictionObj))
            ## message("[", Sys.time(), "] - Done.")            
      }
      n.mem <- dim(o$Data)[1]
      # Member aggregation before
      if (type == "before") {
            if (n.mem > 1) message("[", Sys.time(), "] - Aggregating members before computing correlation...")
            o$Data <- apply(o$Data, MARGIN = c(2,3), FUN = mean, na.rm = TRUE)
            attr(o$Data, "dimensions") <- c("time", "station")
            o <- dimFix(o)
            if (n.mem > 1) message("[", Sys.time(), "] - Done.")
            n.mem <- dim(o$Data)[1]
      }
      # Removing seasonal cycle
      if (deseason) o <- deseason.VALUE(o, window.width, max.na.prop)
      mat.list <- lapply(1:length(season),  function(i) {
            sea <- switch(season[i], 
                          "annual" = 1:12,
                          "DJF" = c(12,1,2),
                          "MAM" = 3:5,
                          "JJA" = 6:8,
                          "SON" = 9:11)
            aux <- na.filter.VALUE(subsetVALUE(o, season = sea), max.na.prop)
            # Correlation matrices for each member
            message("[", Sys.time(), "] - Computing correlation matrix for ", season[i], " period...")
            cor.mat.list <- lapply(1:n.mem, function(x) cor(aux$Data[x,,], use = "pairwise.complete.obs", method = method))
            aux$Data <- NULL
            arr <- do.call("abind", c(cor.mat.list, along = -1L))
            cor.mat.list <- NULL
            # Member aggregation "after"
            cormat <- unname(apply(arr, MARGIN = c(2,3), FUN = mean, na.rm = TRUE))
            arr <- NULL
            attr(cormat, "station_id") <- aux$Metadata$station_id
            attr(cormat, "station_names") <- aux$Metadata$name
            attr(cormat, "lat") <- unname(aux$xyCoords[,2])
            attr(cormat, "lon") <- unname(aux$xyCoords[,1])
            attr(cormat, "lat") <- unname(aux$xyCoords[,2])
            return(cormat)
      })
      names(mat.list) <- season
      message("[", Sys.time(), "] - Done.")
      attr(mat.list, "correlation.type") <- method
      attr(mat.list, "max.na.prop") <- max.na.prop
      attr(mat.list, "deseason") <- deseason
      if (deseason) attr(mat.list, "deseason:window") <- window.width
      return(mat.list)
}









