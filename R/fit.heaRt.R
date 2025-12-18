
fit.tReeboot <- function(obj, num_var = c("1", "2"),
                         fit_type = c("decision.tree", "logistic.reg", "random.forest", "lm"),
                         consider_first = TRUE, ...) {

  # with the param "num_var" the user can choose the number of variable in the model(up to two)
  # and with the param "consider_first" can choose if considering the first of the two or no

  if(!inherits(obj, "heart"))
    stop("This function only works on objects of class \"heart\"")

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
    data <- data |> dplyr::select(y, var2) |> dplyr::rename("var1" == "var2") #easier to plot the randomf


  # fitting the model

  if(num_var == "1")
    mod <- switch(fit_type,
                  decision.tree = {
                    data |> rpart::rpart(y ~ var1, data = _)
                  },
                  logostic.reg = {
                    data |> glm(y ~ var1, family = binomial(link = "logit"), data = _)
                  },
                  random.forest = {
                    data |> randomForest::randomForest(y ~ var1, data = _)
                  },
                  lm = {
                    data |> lm(y ~ var1, data = _)
                  })

  if(num_var == "2" & consider_first == TRUE)
    mod <- switch(fit_type,
                  decision.tree = {
                    data |> rpart::rpart(y ~ var1 + var2, data = _)
                  },
                  logostic.reg = {
                    data |> glm(y ~ var1 + var2, family = binomial(link = "logit"), data = _)
                  },
                  random.forest = {
                    data |> randomForest::randomForest(y ~ var1 + var2, data = _)
                  },
                  lm = {
                    data |> lm(y ~ var1 + var2, data = _)
                  })

  if(num_var == "2" & consider_first == FALSE)
    mod <- switch(fit_type,
                  decision.tree = {
                    data |> rpart::rpart(y ~ var2, data = _)
                  },
                  logostic.reg = {
                    data |> glm(y ~ var2, family = binomial(link = "logit"), data = _)
                  },
                  random.forest = {
                    data |> randomForest::randomForest(y ~ var2,data = _)
                  },
                  lm = {
                    data |> lm(y ~ var2, data = _)
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
