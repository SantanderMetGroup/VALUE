#' @title Display a summary table of VALUE indices
#' @description Lists the available indices from the VALUE Framework. A table with all indices is shown on screen by default.
#' #' Indices can be also queried by their VALUE codes, function name or other keywords passed to argument \code{filter}. See Details
#' @param index.code String. Index code
#' @param function.name String. Name of the function
#' @param filter Optional character string containing a pattern to be matched 
#' @details The original table withh all available indices can be found in the following link: \url{http://www.value-cost.eu/validationportal/app/#!indices}
#' @references Gutiérrez, J.M., Maraun, D., Widmann, M., Huth, R., Hertig, E., Benestad, R., Roessler, O., Wibig, J., Wilcke, R., Kotlarski, S., San Martín, D., Herrera, S., Bedia, J., Casanueva, A., Manzanas, R., Iturbide, M., Vrac, M., Dubrovsky, M., Ribalaygua, J., Pórtoles, J., Räty, O., Räisänen, J., Hingray, B., Raynaud, D., Casado, M.J., Ramos, P., Zerenner, T., Turco, M., Bosshard, T., Štěpánek, P., Bartholy, J., Pongracz, R., Keller, D.E., Fischer, A.M., Cardoso, R.M., Soares, P.M.M., Czernecki, B., Pagé, C., 2018. An intercomparison of a large ensemble of statistical downscaling methods over Europe: Results from the VALUE perfect predictor cross-validation experiment. International Journal of Climatology. https://doi.org/10.1002/joc.5462
#' @export



summary.indices <- function(index.code = NULL, function.name = NULL, filter = NULL) {
  df <- read.lookup.table("indices")
  if (!is.null(index.code)) {
    grep(index.code, df$CODE)
  }
}

#' @title Read lookup tables
#' @description Read the internal lookup tables of indices and diagnostics
#' @param table.type Character string indicating what table to read. Two possible values, either
#' \code{"diagnostics"} or \code{"indices"}.
#' @return The lookup table requested, as a \code{data.frame}.
#' @keywords internal
#' @importFrom magrittr %>% 

 #library(magrittr)
 #table.type = "indices"
read.lookup.table <- function(table.type) {
  table.type <- match.arg(table.type, choices = c("diagnostics", "indices"))
  table.type <- paste0("lookup_tables/", table.type, ".csv") 
  system.file(table.type, package = "VALUE") %>% read.table(header = TRUE,
                                                            sep = ";",
                                                            stringsAsFactors = FALSE,
                                                            na.strings = "")
}
 



