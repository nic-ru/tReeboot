#' Plot heaRt output
#'
#' Plot the different type of model from symptoms related to the diagnosis
#' of heart disease.
#'
#' @param x An abject of class \code{"heaRt_fit"}.
#'
#' @param ... Catches unused arguments to \code{plot}.
#'
#' @returns Either a ggplot, a prp plot or partial plot.
#'
#' @method plot heaRt_fit
#' @export
#'
#' @export
#'
#' @importFrom ggplot2 "ggplot" "geom_point" "geom_line" "theme" "aes"
#' @importFrom dplyr "mutate"
#' @importFrom rpart.plot "prp"
#' @importFrom randomForest "partialPlot"
#' @importFrom tibble "tibble"
#' @importFrom stats "predict"
#'
#' @examples
#' \dontrun{
#' dat <- load_heaRt(vars = "rbp-restECG")
#' mod1 <- fit(dat, num_var = "2", fit_type = "decision.tree")
#' mod2 <- fit(dat, num_var = "2", fit_type = "logistic.reg", consider_first = FALSE)
#' plot(mod1)
#' plot(mod2)
#' }
plot.heaRt_fit <- function(x, ...){

  # Checking if the object is a decision tree or a random forest
  if(x$fit_type == "decision.tree")
    return(rpart.plot::prp(x$model, type = 2, extra = 0, roundint=FALSE))

  # It prints a partial dependence plot, always considering the first variable
  if(x$fit_type == "random.forest"){
    dat <- x$data
    return(randomForest::partialPlot(x$model, dat, var1))
    }

  # Determining the number of variables in the model
  dat <- x$data

  if(ncol(dat) == 2) {

    predictor <- dat$var1

    fits <- switch(x$fit_type,
                  lm = {
                    tibble::tibble(var1 = predictor,
                                   pred = as.numeric(stats::predict(x$model,
                                                                    newdata = tibble::tibble(var1=predictor))))
                  },
                  logistic.reg = {
                    tibble::tibble(var1 = predictor,
                                   pred = as.numeric(stats::predict(x$model,
                                                                    newdata = tibble::tibble(var1=predictor),
                                                                    type = "response")))
                  })


    p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = var1, y = y, color = factor(y))) +
          ggplot2::geom_point() +
          ggplot2::geom_line(data = fits, ggplot2::aes(x = var1, y = pred), inherit.aes = FALSE) +
          ggplot2::theme(legend.position = "none")

    return(p)
  }

  if(ncol(dat) == 3){

    predictor <- dat$var1

    fits <- switch(x$fit_type,
                   lm = {
                     tibble::tibble(var1 = predictor,
                                    pred = as.numeric(stats::predict(x$model,
                                                            newdata = tibble::tibble(var1=predictor,
                                                                                     var2=mean(dat$var2)))))
                   },
                   logistic.reg = {
                     tibble::tibble(var1 = predictor,
                                    pred = as.numeric(stats::predict(x$model,
                                                            newdata = tibble::tibble(var1=predictor,
                                                                                     var2=mean(dat$var2)),
                                                            type = "response")))
                   })


    p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = var1, y = var2, color = factor(y))) +
      ggplot2::geom_point() +
      ggplot2::geom_line(data = fits, ggplot2::aes(x = var1, y = pred), inherit.aes = FALSE) +
      ggplot2::theme(legend.position = "none")

    return(p)
  }
}
