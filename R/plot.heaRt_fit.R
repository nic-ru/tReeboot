#' Plot heaRt output
#'
#' Plot the different type of model fro sintoms related to the diagnosis of heart disease
#'
#' @param x An abject of class \code{"heaRt_fit"}.
#'
#' @param ... Catches unused arguments to \code{plot}
#'
#' @returns Either a ggplot, a prp plot or partial plot
#'
#' @export
#'
#' @importFrom ggplot2 "ggplot" "geom_point" "geom_line" "theme" "aes"
#' @importFrom dplyr "mutate"
#' @importFrom rpart "prp"
#' @importFrom randomForest "partialPlot"
#' @importFrom tibble "tibble"
#' @importFrom stats "predict"
#'
#' @examples
#' dat <- load_heaRt(vars = "rbp-restECG")
#' mod1 <- fit(dat, num_var = "2", fit_type = "deicision.tree")
#' mod4 <- fit(dat, num_var = "2", fit_type = "logistic.reg", consider_first = FALSE)
#' plot(mod1)
#' plot(mod2)
plot.heaRt_fit <- function(x, ...){

  # checking if the object is either a decision tree or a random forest
  if(x$fit_type == "decision.tree")
    return(rpart.plot:::prp(x))

  # it'll print a partial plot considering always the first variable
  if(x$fit_type == "random.forest"){
    dat <- x$data
    randomForest:::partialPlot(x, dat, var1)
    }


  # checking with how many variables we are working

  dat <- x$data


  if(ncol(dat) == 2) {
    dat <- dat |>
      dplyr::mutate(x = dat[[2]])

    predictor <- pretty(x$data[[2]])

    fits <- switch(x$fit_type,
                  lm = {
                    tibble::tibble(predictor, pred = stats::predict(x$model,
                                                           newdata = tibble::tibble(x=predictor)))
                  },
                  logistic.reg = {
                    tibble::tibble(predictor, pred = stats::predict(x$model,
                                                           newdata = tibble::tibble(x=predictor)))
                  })


    p <- ggplot2::ggplot(data = dat, aes(x = x, y = y, color = y)) +
          ggplot2::geom_point() +
          ggplot2::geom_line(data = fits, aes(x = predictor, y = pred)) +
          ggplot2::theme(legend.position = "None")

    return(p)
  }

  if(ncol(dat) == 3){

    fits <- switch(x$fit_type,
                   lm = {
                     tibble::tibble(predictor, pred=predict(x$model,
                                                            newdata = tibble::tibble(x=predictor)))
                   },
                   logistic.reg = {
                     tibble::tibble(predictor, pred=predict(x$model,
                                                            newdata = tibble::tibble(x=predictor)))
                   })


    p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = var1, y = var2, color = y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(data = fits, ggplot2::aes(x = predictor, y = pred)) +
      ggplot2::theme(legend.position = "None")

    return(p)
  }
}
