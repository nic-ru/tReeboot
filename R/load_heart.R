#' Load in medical data about the diagnosis of a heart disease
#'
#' Load in medical data about medical exams done by patients in order to find out if they have an heart
#' disease and if it's severe or no.
#'
#' @param vars Either \code{"a-s"}, \code{"rbp-restECG"} or \code{"chol-mhr"} for age(1=M, 0=F)-sex,
#' resting blood pressure-resting ECG and serum cholesterol-maximum heart rate.
#'
#' @param severe_diag Either \code{"TRUE"} or \code{"FALSE"}, to select the severe
#' diagnosis or the normal diagnosis. Defaults to \code{"FALSE"}.
#'
#' @returns An object of class \code{"heaRt"} which is a list of the chosen variables and the results
#' of the diagnosis (1 = positive, 0 = negative)
#'
#' @note A dedicated \code{\link{fit}} function is provided for objects of class \code{"heaRt"}.
#'
#' @export
#'
#' @importFrom dplyr "filter" "mutate" "select"
#'
#' @seealso \code{\link{fit}}, \code{\link{plot.heaRt_fit}}
#'
#' @examples
#' dat <- load_heaRt(vars = "rbp-restECG", sever_diag = TRUE)
load_heaRt <- function(vars = c("a-s", "rbp-restECG", "chol-mhr"), severe_diag = FALSE){

  # the user can choose which of variables he wants to analyze from the original data

  vars <-  match.arg(vars)

  heart_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

  # getting the data and organizing them in a nice way
  heart_dat <- read.csv(heart_url)

  colnames(heart_dat) <- c("age", "sex", "cp", "rbp", "chol", "fbs",
                             "restECG", "mhr", "eia", "oldpeak",
                             "slope", "ca", "thal", "diagnosis")

  # filtering the data, the user can choose if he wants to analyze more or less severe diagnosis
  if(severe_diag == FALSE)
    heart_dat <- heart_dat |> dplyr::filter(diagnosis != 3 & diagnosis != 4)
  else
    heart_dat <- heart_dat |> dplyr::filter(diagnosis != 1 & diagnosis != 2)

  # extracting the variable the user choose from the original data
  heart_dat <- switch(vars,
                      "a-s" = {
                        heart_dat |>
                          dplyr::mutate(y = dplyr::case_when(
                            diagnosis != 0 ~ 1,
                            .default = 0
                          ))
                          dplyr::select(y, age, sex)
                      },
                      "rbp-restECG" = {
                        heart_dat |>
                          dplyr::mutate(y = dplyr::case_when(
                            diagnosis != 0 ~ 1,
                            .default = 0
                          ))
                          dplyr::select(y, rbp, restECG)
                      },
                      "chol-mhr" = {
                        heart_dat |>
                          dplyr::mutate(y = dplyr::case_when(
                            diagnosis != 0 ~ 1,
                            .default = 0
                          ))
                          dplyr::select(y, chol, mhr)
                      })

  output <- heart_dat
  attr(output, "source") <- type
  class(output) <- c("heaRt", "listof")

  return(output)

}
