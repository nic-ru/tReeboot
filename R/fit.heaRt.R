#' Fit statistical and machine learning models to heart disease data
#'
#' Fits several statistical and machine learning models to heart disease
#' data previously loaded with \code{\link{load_heaRt}}. The response
#' variable is the binary diagnosis indicator \code{y}, while one or two
#' clinical predictors are used as explanatory variables.
#'
#' The function is implemented as an S3 generic, with a dedicated method
#' for objects of class \code{"heaRt"}.
#'
#' @param obj An object of class \code{heaRt}.
#'
#' @param num_var The number of variable the function will consider when
#' fitting the model, either 1 or 2.
#'
#' @param fit_type The type of algorithm requested, either decision tree
#' (\code{"decision.tree"}), logistic regression (\code{"logistic.reg"}),
#' random forest (\code{"random.forest"}) or linear model (\code{"lm"}).
#'
#' @param consider_first The option of considering the first of the two
#' variables, either \code{"TRUE"} or \code{"FALSE"}, defaults to \code{"TRUE"}.
#'
#' @param ... Catches unused arguments to \code{fit}.
#'
#' @returns An object of class \code{"heaRt_fit"} which includes the model details
#' as well as the data set and \code{fit_type} used.
#'
#' @note A dedicated \code{\link[=plot.heaRt_fit]{plot}} method is provided
#' for objects of class \code{"heaRt_fit"}.
#'
#' @seealso \code{\link[=load_heaRt]{load_heaRt}},
#' \code{\link[=plot.heaRt_fit]{plot.heaRt_fit}}, \code{\link[stats]{lm}},
#' \code{\link[rpart]{rpart}}, \code{\link[stats]{glm}},
#' \code{\link[randomForest]{randomForest}}
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
#' mod1 <- fit(dat, num_var = "2", fit_type = "decision.tree")
#' mod2 <- fit(dat, num_var = "2", fit_type = "random.forest")
#' mod3 <- fit(dat, num_var = "1", fit_type = "lm")
#' mod4 <- fit(dat, num_var = "2", fit_type = "logistic.reg", consider_first = FALSE)
fit <- function(obj, num_var = c("1", "2"),
                fit_type = c("decision.tree", "logistic.reg", "random.forest", "lm"),
                consider_first = TRUE, ...) {
  UseMethod("fit")
}

#' @export
fit.heaRt <- function(obj, num_var = c("1", "2"),
                         fit_type = c("decision.tree", "logistic.reg", "random.forest", "lm"),
                         consider_first = TRUE, ...) {

  # ensure the input is of class "heaRt"
  if(!inherits(obj, "heaRt"))
    stop("This function only works on objects of class \"heaRt\"")

  # With the parameter "num_var" the user can choose the number of variables in the model (up to two)
  # the parameter "consider_first" specifies whether the first of the two variables should be included

  num_var <- match.arg(num_var)

  # Choosing the type of fitting model
  fit_type <- match.arg(fit_type)

  y <- obj$y
  var1 <- obj[[2]]
  var2 <- obj[[3]]

  data <- switch(num_var,
                 "1" = {
                   data.frame(y = y, var1 = var1)},
                 "2" = {
                   data.frame(y = y, var1 = var1, var2 = var2)})

  if(consider_first == FALSE)
    data <- data |> dplyr::select(y, var2) |> dplyr::rename(var1 = var2)
  # this step makes random forest models easier to plot

  # Fitting the model:

  if(num_var == "1")
    mod <- switch(fit_type,
                  decision.tree = {
                    data |> rpart::rpart(y ~ var1, data = _)
                  },
                  logistic.reg = {
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
                  logistic.reg = {
                    data |> stats::glm(y ~ var1 + var2, family = stats::binomial(link = "logit"),
                                       data = _)
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
                  logistic.reg = {
                    data |> stats::glm(y ~ var2, family = stats::binomial(link = "logit"), data = _)
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
                 fit_type = fit_type,
                 method = NULL)
  attr(output, "source") <- attr(obj, "source")

  # Checking if the fitted model is a decision tree or a random forest,
  # this information is used later for plotting
  if(fit_type %in% c("decision.tree", "random.forest"))
    output$method <- "class"

  class(output) <- c("heaRt_fit",
                     class(mod),
                     "listof")

  invisible(output)

}
