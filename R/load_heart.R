#' Load heart disease data from clinical examinations
#'
#' Loads medical data derived from clinical examinations performed on patients,
#' with the aim of analysing the presence or absence of heart disease.
#' The function allows the user to select different pairs of clinical
#' predictors and to focus on either mild or severe diagnoses.
#'
#' @usage
#' load_heaRt(
#'   vars = c("a-s", "rbp-restECG", "chol-mhr"),
#'   severe_diag = FALSE
#' )
#'
#' @arguments
#' \item{vars}{A character string specifying which pair of predictors to load.
#' Possible choices are \code{"a-s"} (age and sex),
#' \code{"rbp-restECG"} (resting blood pressure and resting ECG),
#' or \code{"chol-mhr"} (serum cholesterol and maximum heart rate).}
#'
#' \item{severe_diag}{Logical. If \code{FALSE} (default), the response variable
#' distinguishes between healthy subjects and patients with mild heart disease.
#' If \code{TRUE}, the response focuses on severe diagnoses only.}
#'
#' @value
#' An object of class \code{"heaRt"} containing the selected predictors and
#' a binary response variable \code{y}, where \code{1} indicates the presence
#' of heart disease and \code{0} indicates its absence.
#'
#' @description
#' Downloads and preprocesses heart disease data from the UCI Machine Learning
#' Repository. The function performs basic data cleaning, constructs a binary
#' diagnosis variable, and returns a structured object suitable for further
#' modelling with the \code{\link{fit}} function.
#'
#' @note
#' A dedicated \code{\link{fit}} method is provided for objects of class
#' \code{"heaRt"}.
#'
#' @examples
#' dat <- load_heaRt(vars = "rbp-restECG")
#' dat_severe <- load_heaRt(vars = "chol-mhr", severe_diag = TRUE)
#'
#' @seealso
#' \code{\link{fit}}, \code{\link{plot.heaRt_fit}}
#'
#' @export



load_heaRt <- function(vars = c("a-s", "rbp-restECG", "chol-mhr"), severe_diag = FALSE){

  # the user can choose which of variables he wants to analyze from the original data

  vars <-  match.arg(vars)

  heart_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

  # getting the data and organizing them in a nice way
  heart_dat <- readr::read_csv(heart_url, col_names = FALSE, na = "?")

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
                          )) |>
                          dplyr::select(y, age, sex)
                      },
                      "rbp-restECG" = {
                        heart_dat |>
                          dplyr::mutate(y = dplyr::case_when(
                            diagnosis != 0 ~ 1,
                            .default = 0
                          )) |>
                          dplyr::select(y, rbp, restECG)
                      },
                      "chol-mhr" = {
                        heart_dat |>
                          dplyr::mutate(y = dplyr::case_when(
                            diagnosis != 0 ~ 1,
                            .default = 0
                          )) |>
                          dplyr::select(y, chol, mhr)
                      })

  output <- heart_dat
  attr(output, "source") <- vars
  class(output) <- c("heaRt", class(output))

  return(output)

}
