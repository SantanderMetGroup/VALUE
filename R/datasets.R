#' @title Observations and deterministic predictions of daily (minimum) temperature
#' @description A dataset containing daily observations and deterministic predictions of minimum temperature, for the period 1979-2000
#' @format A data frame with 8036 rows (days) and 3 variables:
#' \describe{
#'   \item{dates}{Record date, in POSIXct}
#'   \item{obs}{Observed records}
#'   \item{pred}{Predicted records. See details}
#' }
#' @details Data correspond to the NAVACERRADA station (~1800 m.a.s.l.), part of the ECA-VALUE-86 dataset. The deterministic predictions have
#' been generated with the ANALOG method (see Gutiérrez et al 2018 -Appendix A- and references therein for further details)
#' @references Gutiérrez, J.M., Maraun, D., Widmann et al., 2018. An intercomparison of a large ensemble of statistical downscaling methods over Europe: Results from the VALUE perfect predictor cross-validation experiment. International Journal of Climatology. \url{https://doi.org/10.1002/joc.5462}
#' @source The VALUE Validation Portal: \url{http://www.value-cost.eu/validationportal}
"tmin.det"
