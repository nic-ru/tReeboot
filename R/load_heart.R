
load_heaRt <- function(vars = c("a-s", "rbp-restECG", "chol-mhr"), severe_diag = FALSE){

  vars <-  match.arg(vars)

  heart_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

  heart_dat <- read.csv(heart_url)

  colnames(heart_dat) <- c("age", "sex", "cp", "rbp", "chol", "fbs",
                             "restECG", "mhr", "eia", "oldpeak",
                             "slope", "ca", "thal", "diagnosis")

  if(severe_diag == FALSE)
    heart_dat <- heart_dat |> filter(diagnosis != 3 & diagnosis != 4)
  else
    heart_dat <- heart_dat |> filter(diagnosis != 1 & diagnosis != 2)

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
  class(output) <- c("heart", "listof")

  return(output)

}
