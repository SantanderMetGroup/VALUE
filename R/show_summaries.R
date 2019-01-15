#' @title Display a summary table of VALUE indices
#' @description Lists the available indices from the VALUE Framework. A table with all indices is shown on screen by default.
#' #' Indices can be also queried by their VALUE codes, function name or other keywords passed to argument \code{filter}. See Details
#' @param index.code String. Index code
#' @param function.name String. Name of the function
#' @param filter Optional character string containing a pattern to be matched 
#' @details The option \code{filter} allows to search by 'keywords' or 'topics'. Internally, it justs performs pattern matching on the COMMENT field 
#' of the corresponding lookup table. 
#' 
#' The original table with all available indices can be found in the following link: \url{http://www.value-cost.eu/validationportal/app/#!indices}
#' @references Gutiérrez, J.M., Maraun, D., Widmann, M., Huth, R., Hertig, E., Benestad, R., Roessler, O., Wibig, J., Wilcke, R., Kotlarski, S., San Martín, D., Herrera, S., Bedia, J., Casanueva, A., Manzanas, R., Iturbide, M., Vrac, M., Dubrovsky, M., Ribalaygua, J., Pórtoles, J., Räty, O., Räisänen, J., Hingray, B., Raynaud, D., Casado, M.J., Ramos, P., Zerenner, T., Turco, M., Bosshard, T., Štěpánek, P., Bartholy, J., Pongracz, R., Keller, D.E., Fischer, A.M., Cardoso, R.M., Soares, P.M.M., Czernecki, B., Pagé, C., 2018. An intercomparison of a large ensemble of statistical downscaling methods over Europe: Results from the VALUE perfect predictor cross-validation experiment. International Journal of Climatology. https://doi.org/10.1002/joc.5462
#' @export
#' @seealso show.diagnostics
#' @examples 
#' # Get the full table of indices:
#' index.table <- show.indices()
#' str(index.table)
#' # Search by index code:
#' show.indices(index.code = "R01")
#' # Search by filters:
#' show.indices(filter = "90th percentile")
#' show.indices(filter = "spell length")

show.indices <- function(index.code = NULL, function.name = NULL, filter = NULL) {
    df <- read.lookup.table("indices")
    ind <- 1:nrow(df)
    if (!is.null(index.code)) {
        ind <- grep(paste0("^", index.code, "$"), df$CODE, ignore.case = TRUE)
        if (length(ind) == 0) stop("Index code '", index.code, "' not found. Check the complete list of index codes by typing 'show.indices()$CODE'", call. = FALSE)
    }
    if (!is.null(function.name)) {
        ind <- grep(paste0("^", function.name), df$FUNCTION.CALL)
    }
    if (!is.null(filter)) {
        ind <- grep(filter, df$DESCRIPTION, ignore.case = TRUE)  
    }
    return(df[ind, ])
}

#' @title Display a summary table of VALUE diagnostics
#' @description Lists the available diagnostics from the VALUE Framework. A table with all diagnostics is shown on screen by default.
#' Diagnostics can be also queried by their VALUE codes, function name or other keywords passed to argument \code{filter}. See Details
#' @param diagnostic.code String. Diagnostic code
#' @param function.name String. Name of the function
#' @param filter Optional character string containing a pattern to be matched 
#' @details The option \code{filter} allows to search by 'keywords' or 'topics'. Internally, it justs performs pattern matching on the COMMENT field 
#' of the corresponding lookup table. 
#' 
#' The original table with all the available diagnostics can be found in the following link: \url{http://www.value-cost.eu/validationportal/app/#!indices}
#' 
#' @references Gutiérrez, J.M., Maraun, D., Widmann, M., Huth, R., Hertig, E., Benestad, R., Roessler, O., Wibig, J., Wilcke, R., Kotlarski, S., San Martín, D., Herrera, S., Bedia, J., Casanueva, A., Manzanas, R., Iturbide, M., Vrac, M., Dubrovsky, M., Ribalaygua, J., Pórtoles, J., Räty, O., Räisänen, J., Hingray, B., Raynaud, D., Casado, M.J., Ramos, P., Zerenner, T., Turco, M., Bosshard, T., Štěpánek, P., Bartholy, J., Pongracz, R., Keller, D.E., Fischer, A.M., Cardoso, R.M., Soares, P.M.M., Czernecki, B., Pagé, C., 2018. An intercomparison of a large ensemble of statistical downscaling methods over Europe: Results from the VALUE perfect predictor cross-validation experiment. International Journal of Climatology. https://doi.org/10.1002/joc.5462
#' @seealso show.indices
#' @export
#' @examples 
#' # Get the full table of indices:
#' diagnostic.table <- show.diagnostics()
#' str(diagnostic.table)
#' # Search by index code:
#' show.diagnostics(diagnostic.code = "ts.BSFrost")
#' # Search by filters:
#' show.diagnostics(filter = "Brier")

show.diagnostics <- function(diagnostic.code = NULL, function.name = NULL, filter = NULL) {
    df <- read.lookup.table("diagnostics")
    ind <- 1:nrow(df)
    if (!is.null(diagnostic.code)) {
        ind <- grep(paste0("^", diagnostic.code, "$"), df$CODE, ignore.case = TRUE)
        if (length(ind) == 0) stop("Diagnostic code '", diagnostic.code, "' not found. Check the complete list of diagnostic codes by typing 'show.diagnostics()$CODE'", call. = FALSE)
    }
    if (!is.null(function.name)) {
        ind <- grep(paste0("^", function.name), df$FUNCTION.CALL)
    }
    if (!is.null(filter)) {
        ind <- grep(filter, df$DESCRIPTION, ignore.case = TRUE)  
    }
    return(df[ind, ])
}


#' @title Read lookup tables
#' @description Read the internal lookup tables of indices and diagnostics
#' @param table.type Character string indicating what table to read. Two possible values, either
#' \code{"diagnostics"} or \code{"indices"}.
#' @return The lookup table requested, as a \code{data.frame}.
#' @keywords internal
#' @importFrom magrittr %>% 

read.lookup.table <- function(table.type) {
    table.type <- match.arg(table.type, choices = c("diagnostics", "indices"))
    table.type <- paste0("lookup_tables/", table.type, ".csv") 
    system.file(table.type, package = "VALUE") %>% read.table(header = TRUE,
                                                              sep = ";",
                                                              stringsAsFactors = FALSE,
                                                              na.strings = "")
}
 



