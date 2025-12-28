#' heaRt: Heart Disease Modelling and Visualisation
#'
#' The \pkg{heaRt} package provides tools for loading, analysing and visualising
#' clinical data related to heart disease diagnoses.
#'
#' The package is built around a simple three-step workflow:
#' \enumerate{
#'   \item Load and preprocess clinical data using \code{\link{load_heaRt}}
#'   \item Fit statistical or machine learning models using \code{\link{fit}}
#'   \item Visualise fitted models using \code{\link{plot.heaRt_fit}}
#' }
#'
#' Available models include linear models, logistic regression,
#' decision trees and random forests.
#'
#' @section Main functions:?
#' \describe{
#'   \item{\code{\link{load_heaRt}}}{Load and preprocess heart disease clinical data}
#'   \item{\code{\link{fit}}}{Fit statistical and machine learning models}
#'   \item{\code{\link{plot.heaRt_fit}}}{Visualise fitted models}
#' }
#'
#' @section Data source:
#' The data are obtained from the UCI Machine Learning Repository
#' (Cleveland Heart Disease dataset).
#'
#' @seealso
#' \code{\link{load_heaRt}}, \code{\link{fit}}, \code{\link{plot.heaRt_fit}}
#'
"_PACKAGE"
