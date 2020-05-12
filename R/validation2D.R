#     validation2D. Atomic functions calling several VALUE spatial validation routines
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



#' @title VALUE spatial index calculation
#' @description (non-atomic) Function to compute spatial VALUE indices from series. 
#' @param tsl List of time series vectors.
#' @param index.code A character index code to be computed.
#' @param dates A character (or \code{POSIXct}) vector following the format
#'  \dQuote{YYYY-MM-DD} (i.e., \code{format = "\%Y-\%m-\%d"} as in 
#'  \code{\link{strptime}}). Note that the dates are not required 
#'  by all indices, so the default is \code{NULL}. The function will yield an 
#'  error if dates are required but not supplied.
#' @param parallel Use parallel computation.
#' @param max.ncores Maximum number of cores for parallel computing.
#' @param ncores Number of threads for parallel computation. Default to ncores - 1.
#' @return index.code matrix
#' @author M. N. Legasa
#' @export


valueIndex2D <- function(tsl, index.code, dates = NULL,
                         parallel = FALSE, max.ncores = 16, ncores = NULL){
  arg.list <- list()
  arg.list[["tsl"]] <- tsl
  fun.list <- parseArgumentString(arg.list, 
                                  code = index.code,
                                  dates = dates)
  
  if ("parallel" %in% 
      formalArgs(eval(parse(text = paste0("VALUE:::", fun.list$fun))))){
    fun.list$arg.list[["parallel"]] <- parallel
    fun.list$arg.list[["max.ncores"]] <- max.ncores
    fun.list$arg.list[["ncores"]] <- ncores
  }
  
  out <- do.call(fun.list$fun, fun.list$arg.list)
  return(out)
}
