#' Fit some machine learning algorithm
#'
#' Fit either decision tree, logistic regression, random forest or linear model of heart data with the
#' variable diagnosis(y) as the response.
#'
#' @param obj An object of class \code{heaRt}
#'
#' @param num_var The number of variable the function will consider when fitting the model, either
#' 1 or 2.
#'
#' @param fit_type The type of algorithm requested, either decision tree (\code{"decision.tree"}),
#' logistic regression (\code{"logistic.reg"}), random forest (\code{"random.forest"}) or
#' linear model (\code{"lm"}).
#'
#' @param consider_first The option of considering the first of the two variables, either
#' \code{"TRUE"} or \code{"FALSE"}, Defaults to \code{"TRUE"}.
#'
#' @param ... Catches unused arguments to \code{fit},
#'
#' @returns An object of class \code{"heaRt_fit"} which includes the model details
#' as well as the data set and \code{fit_type} used.
#'
#' @note A dedicated \code{\link{plot.heaRt_fit}} method is provided
#' for objects of class \code{"heaRt_fit"}.
#'
#' @seealso \code{\link{load_climr}}, \code{\link{plot.climr_fit}}, \code{\link[stats]{lm}},
#' \code{\link[rpart]{rpart}}, \code{\link[stats]{glm}}, \code{\link[randomForest]{randomForest}}.
#'
#' @export
#'
#' @importFrom dplyr "select" "rename"
#' @importFrom rpart "rpart"
#' @importFrom randomForest "randomForest"
#' @importFrom stats "lm" "glm" "binomial"
#'
#' @examples
#' dat <- load_heaRt(vars = "rbp-restECG")
#' mod1 <- fit(dat, num_var = "2", fit_type = "deicision.tree")
#' mod2 <- fit(dat, num_var = "2", fit_type = "random.forest")
#' mod3 <- fit(dat, num_var = "1", fit_type = "lm")
#' mod4 <- fit(dat, num_var = "2", fit_type = "logistic.reg", consider_first = FALSE)
fit.tReeboot <- function(obj, num_var = c("1", "2"),
                         fit_type = c("decision.tree", "logistic.reg", "random.forest", "lm"),
                         consider_first = TRUE, ...) {

  # with the param "num_var" the user can choose the number of variable in the model(up to two)
  # and with the param "consider_first" can choose if considering the first of the two or no

  if(!inherits(obj, "heaRt"))
    stop("This function only works on objects of class \"heaRt\"")

  # choosing the type of fitting model
  fit_type = match.arg(fit_type)

  y <- obj$y
  var1 <- obj[[2]]
  var2 <- obj[[3]]

  data <- switch(num_var,
                 "1" = {
                   data.frame(y = y, var1 = var1)},
                 "2" = {
                   data.frame(y = y, var1 = var1, var2 = var2)})

  if(consider_first == FALSE)
    data <- data |> dplyr::select(y, var2) |> dplyr::rename("var1" == "var2")
  # easier to plot the random forest

  # fitting the model

  if(num_var == "1")
    mod <- switch(fit_type,
                  decision.tree = {
                    data |> rpart::rpart(y ~ var1, data = _)
                  },
                  logostic.reg = {
                    data |> stats::glm(y ~ var1, family = stats::binomial(link = "logit"), data = _)
                  },
                  random.forest = {
                    data |> randomForest::randomForest(y ~ var1, data = _)
                  },
                  lm = {
                    data |> stats::lm(y ~ var1, data = _)
                  })

  if(num_var == "2" & consider_first == TRUE)
    mod <- switch(fit_type,
                  decision.tree = {
                    data |> rpart::rpart(y ~ var1 + var2, data = _)
                  },
                  logostic.reg = {
                    data |> stats::glm(y ~ var1 + var2, family = binomial(link = "logit"), data = _)
                  },
                  random.forest = {
                    data |> randomForest::randomForest(y ~ var1 + var2, data = _)
                  },
                  lm = {
                    data |> stats::lm(y ~ var1 + var2, data = _)
                  })

  if(num_var == "2" & consider_first == FALSE)
    mod <- switch(fit_type,
                  decision.tree = {
                    data |> rpart::rpart(y ~ var2, data = _)
                  },
                  logostic.reg = {
                    data |> stats::glm(y ~ var2, family = binomial(link = "logit"), data = _)
                  },
                  random.forest = {
                    data |> randomForest::randomForest(y ~ var2,data = _)
                  },
                  lm = {
                    data |> stats::lm(y ~ var2, data = _)
                  })


  print(mod)

  output <- list(model = mod,
                 data = data,
                 fit_type = fit_type)
  attr(output, "source") <- attr(obj, "source")
  class(output) <- c("heaRt_fit","listof")

  # checking if the fitted model is either a decision tree or a random forest,
  # it'll be useful when plotting

  if(fit_type == "decision.tree")
    class(output) <- c("heaRt_fit", "rpart", "listof")

  if(fit_type == "random.forest")
    class(output) <- c("heaRt_fit", "randomForest", "listof")

  invisible(output)

}
