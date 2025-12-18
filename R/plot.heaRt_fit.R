
plot.heaRt_fit <- function(x, decision.tree = FALSE, random.forest = FALSE, ...){

  # checking if the object is either a decision tree or a random forest
  if(decision.tree == TRUE)
    return(rpart.plot::prp(x))

  if(random.forest == TRUE) #risolvere questo facendo in modo che printi qualcosa
    stop("This tiper of algorith is not printable")

  # checking with how many variables we are working

  dat <- x$data


  if(ncol(dat) == 2) {
    dat <- dat |>
      dplyr::mutate(x = dat[[2]])

    predictor <- pretty(x$data[[2]])

    fits <- switch(x$fit_type,
                  lm = {
                    tibble::tibble(predictor, pred=predict(x$model,
                                                           newdata = tibble::tibble(x=predictor)))
                  },
                  logistic.reg = {
                    tibble::tibble(predictor, pred=predict(x$model,
                                                           newdata = tibble::tibble(x=predictor)))
                  })


    p <- ggplot2::ggplot(data = dat, aes(x = x, y = y, color = y)) +
          ggplot2::geom_point() +
          ggplot2::geom_line(data = fits, aes(x = predictor, y = pred)) +
          theme(legend.position = "None")

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


    p <- ggplot2::ggplot(data = dat, aes(x = var1, y = var2, color = y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(data = fits, aes(x = predictor, y = pred)) +
      theme(legend.position = "None")

    return(p)
  }








}
