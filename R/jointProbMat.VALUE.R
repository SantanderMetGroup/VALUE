#     jointProbMat.VALUE.R Joint probability analysis
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
#' @title Joint probability analysis
#' @description Computes different joint (precipitation) probabilities between stations 
#' @param stationObj An R-VALUE object containing station data (as returned by \code{\link{loadValueStations}}).
#' @param predictionObj A R-VALUE predictions object as loaded by \code{\link{loadValuePredictions}}.
#'  Default to NULL, meaning that the matrix of joint probabilities is done on the observations.
#' @param season Character string indicating the target season. Accepted values are 
#' \code{c("annual", "DJF", "MAM", "JJA", "SON")}
#' @param aggr.type Type of aggregation in the case of multiple realizations. Should the aggregation of 
#' multiple members be performed \code{"after"} (the default) or \code{"before"} computing the joint probabilities?. Ignored
#'  in the case of observations and deterministic predictions.
#' @param prob.type Character vector indicating the type of probability to be computed. Currently accepted values are:
#' \code{"DD"} for dry-dry, \code{"DW"} for dry-wet, \code{"WW"} for wet-wet and \code{"WD"} for wet-dry.
#' @param prob Default to NULL (unused). Otherwise, a float number in the range (0,1) defining the quantile threshold to be calculated.
#' If \code{prob} is used, the joint exceedance probabilities will be calculated (i.e., the probability of an occurrence above the \code{prob} percentile
#' in location A given the same exceedance in location B).
#' @param output Character string indicating the type of measure to be retained. Default to \code{"MI"} for the mutual information criterion.
#' For the joint probabilities use \code{"jointProb"}. See details.
#' @param threshold Threshold above which values are used/discarded (i.e., values greater or equal than \code{threshold} are considered as Wet).
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
#' In addition, the type of joint probability, as coded in argument \code{prob.type} is indicated in the global 
#' attribute \code{"joint_prob_type"}.   
#' @author J. Bedia 
#' @export
#' @references \url{https://en.wikipedia.org/wiki/Mutual_information}
#' @examples \dontrun{
#' obs.file <- file.path(find.package("VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
#' stationObj <- loadValueStations(obs.file, var = "precip")
#' # Wet-wet probability (precip >= 1mm)
#' ww <- jointProbMat.VALUE(stationObj,
#'                          predictionObj = NULL,
#'                          season = "annual",
#'                          threshold = 1,
#'                          max.na.prop = 1,
#'                          aggr.type = "after",
#'                          prob.type = "WW",
#'                          output = "MI")
#'
#' # Dry-dry joint probability (precip < 1mm)
#' dd <- jointProbMat.VALUE(stationObj,
#'                          predictionObj = NULL,
#'                          season = "annual",
#'                          threshold = 1,
#'                          max.na.prop = 1,
#'                          aggr.type = "after",
#'                          prob.type = "DD",
#'                          output = "MI")
#'                          
#' # Draw matrix - requires lattice!
#' mat <- matrix(ncol = ncol(ww[[1]]), nrow = nrow(ww[[1]]))
#' mat[upper.tri(mat)] <- ww[[1]][upper.tri(ww[[1]])]
#' mat[lower.tri(mat)] <- dd[[1]][upper.tri(dd[[1]])]
#' station.labels <- attr(ww[[1]], "station_names")
#' scales.list <- list(x = list(labels = station.labels, rot = 90,
#'                            at = seq(1,ncol(ww[[1]]),1), cex = .5),
#'                     y = list(labels = station.labels,
#'                              at = seq(1,ncol(ww[[1]]),1), cex = .5))
#' # lattice::levelplot(mat, xlab = "P(DRY,DRY)", ylab = "P(WET,WET)",
#' # main = "Mutual Information Matrix", scales = scales.list)
#' }

jointProbMat.VALUE <- function(stationObj,
                               predictionObj = NULL,
                               season = c("annual", "DJF", "MAM", "JJA", "SON"),
                               aggr.type = c("after","before"),
                               prob.type = c("DD", "DW", "WW", "WD"),
                               prob = NULL, 
                               output = c("MI","jointProb"),
                               threshold = 1,
                               max.na.prop = 0.25) {
      season <- match.arg(season, choices = c("annual", "DJF", "MAM", "JJA", "SON"), several.ok = TRUE)
      aggr.type <- match.arg(aggr.type, choices = c("after", "before"))
      prob.type <- match.arg(prob.type, choices = c("DD", "DW", "WW", "WD"))
      if (!is.null(prob)) {
            if (length(prob) > 1) stop("Invalid 'prob' definition")
            if (prob <= 0 || prob >= 1) stop("Invalid 'prob' definition")
            if (prob.type != "WW") warning("A 'prob' value has been introduced\nNote that joint threshold exceedances can be only calculated for wet-wet probabilities (prob.type=\"WW\")")
            prob.type <- "WW"
      }
      output <- match.arg(output, choices = c("MI","jointProb"))
      ineq1 <- substr(prob.type, 1, 1)
      ineq2 <- substr(prob.type, 2, 2)
      ineqs <- sapply(c(ineq1, ineq2), function(x) switch(x, "D" = "<", "W" = ">="))
      if (is.null(prob)) {
            exprPB <- paste0("sum(x", ineqs[2], "threshold,na.rm=TRUE) / length(which(!is.na(x)))") # Calculates P(B) matrix
            expr1 <- paste("which(mat[i,,j]", ineqs[1], "threshold)") # index for conditioning
            expr2 <- paste("which(mat[i,ind,k]", ineqs[2], "threshold)") # index for conditioning
      } else {## For a given threshold exceedance
            exprPB <- paste0("sum(x > quantile(x[x > threshold], probs = ",
                             prob, ", na.rm = TRUE), na.rm = TRUE) / length(which(!is.na(x)))")  ## Probability of exceedance in B
            expr1 <- paste0("which(mat[i,,j] > quantile(mat[i,,j][mat[i,,j] > threshold], probs = ",
                            prob, ", na.rm = TRUE))") # index for conditioning on B
            expr2 <- paste0("which(mat[i,ind,k] > quantile(mat[i,ind,k][mat[i,ind,k] > threshold], probs = ",
                            prob, ", na.rm = TRUE))") # index for conditioning on A
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
            # Joint probability ------------------------
            message("[", Sys.time(), "] - Calculating probabilities for ", season[x], "...")
            jp.list <- lapply(1:n.mem, function(i) {
                  jpmat <- matrix(nrow = n.stations, ncol = n.stations)
                  if (output == "MI") pb <- apply(mat[i,,], MARGIN = 2, FUN = function(x) eval(parse(text = exprPB))) # P(B)
                  for (j in 1:n.stations) {
                        ind <- eval(parse(text = expr1))
                        PrA <- length(na.omit(ind)) / length(na.omit(mat[i,,j])) # P(A)
                        ind.diff <- setdiff(1:n.stations, j)
                        for (k in ind.diff) {
                              ind.aux <- eval(parse(text = expr2))
                              PrBA <- length(ind.aux)/length(ind) # P(B|A)
                              # Joint probability -----------------------
                              out <- PrA*PrBA # P(A,B) = P(A)*P(B|A)
                              # Mutual information ---------------------
                              if (output == "MI") {
                                    PrB <- pb[k] # P(B)
                                    out <- out*log((out/(PrA*PrB))) # P(A,B)*log[P(A,B)/P(A)*P(B)]
                              }
                              jpmat[j,k] <- out
                        }
                  }
                  jpmat[which(jpmat < 0)] <- 0 # small negatives may appear due to rounding errors
                  return(jpmat)
            })
            arr <- do.call("abind", c(jp.list, along = -1L))
            jp.list <- NULL
            # Member aggregation "after"
            if (aggr.type == "after") message("[", Sys.time(), "] - Aggregating members...")
            jpmat <- unname(apply(arr, MARGIN = c(2,3), FUN = mean, na.rm = TRUE))
            arr <- NULL
            attr(jpmat, "station_names") <- o$Metadata$name
            attr(jpmat, "lon") <- unname(o$xyCoords[,1])
            attr(jpmat, "lat") <- unname(o$xyCoords[,2])
            message("[", Sys.time(), "] - OK")
            return(jpmat)
      })
      names(mat.list) <- season
      attr(mat.list, "joint_prob_type") <- prob.type
      attr(mat.list, "joint_prob_output") <- output
      attr(mat.list, "joint_prob_threshold_exceedance") <- prob
      message("[", Sys.time(), "] - Finished.")
      return(mat.list)
}





