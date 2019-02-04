#     miMat.VALUE.R Mutual Information matrix
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
#' @title Mutual Information Matrix
#' @description Computes the mutual information between stations regarding different aspects of precipitation
#' @param stationObj An R-VALUE object containing station data (as returned by \code{\link[loadeR]{loadStationData}}).
#' @param predictionObj A R-VALUE predictions object as loaded by \code{\link{loadStationPredictions}}.
#'  Default to NULL, meaning that the matrix of joint probabilities is done on the observations.
#' @param season Character string indicating the target season. Accepted values are 
#' \code{c("annual", "DJF", "MAM", "JJA", "SON")}
#' @param aggr.type Type of aggregation in the case of multiple realizations. Should the aggregation of 
#' multiple members be performed \code{"after"} (the default) or \code{"before"} computing the joint probabilities?. Ignored
#'  in the case of observations and deterministic predictions.
#' @param prob Default to NULL (unused). Otherwise, a float number in the range (0,1) defining the quantile threshold to be calculated.
#' If \code{prob} is used, the joint exceedance probabilities will be calculated (i.e., the probability of an occurrence above the \code{prob} percentile
#' in location A given the same exceedance in location B...).
#' @param threshold Threshold above which values are used/discarded
#'  (i.e., values greater or equal than \code{threshold} are considered as Wet). Default to 1 (assuming mm)
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0.25). See details
#' @details The typical way to analyze dependencies is comparing the joint $P(wet_{i},wet_{j})$ (i.e. \code{output="jointProb"})
#' and the product of marginals $P(wet_{i}) * P(wet_{j})$. The difference is zero only in case that
#' $wet_i$ and $wet_j$ are independent and the larger the value, the more dependent they are.
#'  Using the joint probability alone would make comparisons a bit difficult since the final result would be
#' a combination of the dependency between both wet series and the marginal probabilities
#' in each of the stations (e.g. the joint for web values would be smaller in dry climates),
#' so the analysis of dependencies will be modulated by the different climatologies.
#' In order to avoid this, the mutual information of 
#' two random variables is a measure of the mutual dependence between the two variables.
#' MI = 0 if the two events are independent.
#' 
#'  MI is more general and determines how similar the joint distribution p(X,Y) is to the products
#'   of factored marginal distribution p(X)p(Y). Mutual information is nonnegative (i.e. \emph{MI(X,Y) >= 0}) 
#'   and symmetric (i.e. \emph{MI(X,Y) = MI(Y,X)}).
#' Note that when the joint probability is zero the returned value of MI is NaN.
#' @return A list of 2D matrices. The length of the list corresponds to the periods indicated in the \code{season} 
#' argument (default to 5, annual and the four standard WMO seasons). Attributes indicate the station names 
#' (in the row/column order they appear), and their geographical coordinates. 
#' @author J. Bedia 
#' @export
#' @references \url{https://en.wikipedia.org/wiki/Mutual_information}
#' @examples \dontrun{
#' obs.file <- file.path(find.package("VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
#' stationObj <- loadValueStations(obs.file, var = "precip")
#' # Mutual information (By default, computes the
#' # joint probabilities for each pair of stations for Dry-Dry, Dry-Wet,
#' # Wet-Wet and Wet-Dry), and applies the MI formula (See References)
#' 
#' mi.matrix <- miMat.VALUE(stationObj,
#'                   predictionObj = NULL,
#'                   season = "annual",
#'                   threshold = 1,
#'                   max.na.prop = 1)
#'                          
#' # Draw matrix - requires lattice!
#' station.labels <- attr(mi.matrix[[1]], "station_names")
#' scales.list <- list(x = list(labels = station.labels, rot = 90,
#'                            at = seq(1,ncol(mi.matrix[[1]]),1), cex = .5),
#'                     y = list(labels = station.labels,
#'                              at = seq(1,ncol(mi.matrix[[1]]),1), cex = .5))
#' # requires lattice package
#' # lattice::levelplot(mi.matrix[[1]], ylab = "", xlab = "",
#' #                      main = "Mutual Information Matrix", scales = scales.list)
#' 
#' # If 'prob' is given, then the same as before, but considering
#' # threshold exceedances instead of occurrence. Example using the threshold exceedance p90:
#' 
#' mi.matrix.p90 <- miMat.VALUE(stationObj,
#'                              predictionObj = NULL,
#'                              season = "annual",
#'                              threshold = 1,
#'                              max.na.prop = 1,
#'                              prob = .9)
#'                              
#' # requires lattice package
#' # lattice::levelplot(mi.matrix.p90[[1]], ylab = "", xlab = "",
#'                      main = "Mutual Information Matrix", scales = scales.list)
#' }

miMat.VALUE <- function(stationObj,
                     predictionObj = NULL,
                     season = c("annual", "DJF", "MAM", "JJA", "SON"),
                     aggr.type = c("after","before"),
                     prob = NULL, 
                     threshold = 1,
                     max.na.prop = 0.25) {
      season <- match.arg(season, choices = c("annual", "DJF", "MAM", "JJA", "SON"), several.ok = TRUE)
      aggr.type <- match.arg(aggr.type, choices = c("after", "before"))
      prob.types <- c("DD", "DW", "WW", "WD")
      if (!is.null(prob)) {
            if (length(prob) > 1) stop("Invalid 'prob' definition")
            if (prob <= 0 || prob >= 1) stop("Invalid 'prob' definition")
      }
      mi.list <- lapply(1:length(prob.types), function(t) {
            prob.type <- prob.types[t]
            message("**** ", prob.type, " ****")
            ineq1 <- substr(prob.type, 1, 1)
            ineq2 <- substr(prob.type, 2, 2)
            ineqs <- sapply(c(ineq1, ineq2), function(x) switch(x, "D" = "<", "W" = ">="))
            if (is.null(prob)) {
                  expr.PrA <- paste0("sum(A", ineqs[1], "threshold, na.rm = TRUE)/length(A)")
                  expr.PrB <- paste0("sum(B", ineqs[1], "threshold, na.rm = TRUE)/length(B)")
                  expr.AgivenB <- paste0("A[which(B", ineqs[2], "threshold)]")
                  expr.PrAgivenB <- paste0("sum(AgivenB", ineqs[1], "threshold,na.rm=TRUE)/length(AgivenB)")
            } else {## For a given threshold exceedance
                  expr.PrA <- paste0("sum(A",ineqs[1],"quantile(A,probs = prob,na.rm=TRUE),na.rm=TRUE)/length(A)")
                  expr.PrB <- paste0("sum(B",ineqs[1],"quantile(B,probs = prob,na.rm=TRUE),na.rm=TRUE)/length(B)")
                  expr.AgivenB <- paste0("A[which(B", ineqs[2], "quantile(B, probs = prob, na.rm=TRUE))]")
                  expr.PrAgivenB <- paste0("sum(AgivenB", ineqs[1], "quantile(A,probs=prob,na.rm=TRUE),na.rm=TRUE)/length(AgivenB)")
            }
            o <- stationObj
            stationObj <- NULL
            if (!is.null(predictionObj)) {
                  o <- suppressWarnings(dimFix(predictionObj))
            }
            mat.list <- lapply(1:length(season),  function(x) {
                  sea <- switch(season[x], 
                                "annual" = 1:12,
                                "DJF" = c(12,1,2),
                                "MAM" = 3:5,
                                "JJA" = 6:8,
                                "SON" = 9:11)
                  o <- na.filter.VALUE(dimFix(subsetVALUE(o, season = sea)), max.na.prop)
                  n.mem <- dim(o$Data)[1]
                  # Member aggregation before
                  if (aggr.type == "before") {
                        if (n.mem > 1) message("[", Sys.time(), "] - Aggregating members before computing joint probabilities...")
                        o$Data <- apply(o$Data, MARGIN = c(2,3), FUN = mean, na.rm = TRUE)
                        attr(o$Data, "dimensions") <- c("time", "station")
                        o <- dimFix(o)
                        if (n.mem > 1) message("[", Sys.time(), "] - Done.")
                        n.mem <- dim(o$Data)[1]
                  }
                  n.stations <- dim(o$Data)[3]
                  mat <- o$Data
                  o$Data <- NULL
                  message("[", Sys.time(), "] - Calculating probabilities for ", season[x], "...")
                  jp.list <- lapply(1:n.mem, function(i) {
                        jpmat <- matrix(nrow = n.stations, ncol = n.stations)
                        for (j in 1:n.stations) {
                              na.a <- which(is.na(mat[i,,j]))
                              ind.diff <- setdiff(1:n.stations, j)
                              for (k in ind.diff) {
                                    # Filter the common missing data in A and B:
                                    na.b <- which(is.na(mat[i,,k]))
                                    na.ind <- union(na.a, na.b)      
                                    if (length(na.ind) > 0) {
                                          A <- mat[i,-na.ind,j]
                                          B <- mat[i,-na.ind,k]
                                    } else {
                                          A <- mat[i,,j]
                                          B <- mat[i,,k]
                                    }
                                    # Marginal probabilities
                                    PrA <- eval(parse(text = expr.PrA))
                                    PrB <- eval(parse(text = expr.PrB))
                                    # Conditional probability Pr(A|B)
                                    AgivenB <- eval(parse(text = expr.AgivenB))
                                    PrAgivenB <- eval(parse(text = expr.PrAgivenB))                        
                                    # Joint probability P(A,B) = P(B,A) = P(A)*P(B|A) P(B)*P(A|B)
                                    PrAB <- PrB*PrAgivenB
                                    # Mutual information 
                                    jpmat[j,k] <- PrAB*log((PrAB/(PrA*PrB))) # P(A,B)*log[P(A,B)/P(A)*P(B)]
                              }
                        }
                        jpmat[which(jpmat < 0)] <- 0 # small negatives may appear due to rounding errors
                        return(jpmat)
                  })
                  arr <- do.call("abind", c(jp.list, along = -1L))
                  jp.list <- NULL
                  # Member aggregation "after"
                  if (aggr.type == "after") message("[", Sys.time(), "] - Aggregating members...")
                  mimat <- unname(apply(arr, MARGIN = c(2,3), FUN = mean, na.rm = TRUE))
                  arr <- NULL
                  attr(mimat, "station_names") <- o$Metadata$name
                  attr(mimat, "lon") <- unname(o$xyCoords[,1])
                  attr(mimat, "lat") <- unname(o$xyCoords[,2])
                  return(mimat)
            })
            names(mat.list) <- season
            return(mat.list)
      })
      names(mi.list) <- prob.types
      out.list <- lapply(1:length(mi.list[[1]]), FUN = function(x) {
            mi.list[[1]][[x]] + mi.list[[2]][[x]] + mi.list[[3]][[x]] + mi.list[[4]][[x]]
      })
      names(out.list) <- season
      mi.list <- NULL
      attr(out.list, "threshold_exceedance") <- prob
      message("[", Sys.time(), "] - Finished.")
      return(out.list)
}
