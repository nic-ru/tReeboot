test_that("fit.heaRt error on non-heaRt input", {

  df <- data.frame(x = 1:10, y = 1:10)

  expect_error(fit.heaRt(df),
               "class \"heaRt\"")
})



test_that("fit.heaRt drops first variable when consider_first = FALSE", {

  dat <- load_heaRt("chol-mhr")
  mod <- fit.heaRt(dat,
                   num_var = "2",
                   fit_type = "lm",
                   consider_first = FALSE)

  expect_equal(colnames(mod$data), c("y", "var1"))
})



test_that("fit.heaRt with num_var = '2' uses two predictors", {

  dat <- load_heaRt("a-s")
  mod <- fit.heaRt(dat, num_var = "2", fit_type = "decision.tree")

  expect_equal(ncol(mod$data), 3)
})



test_that("fit.heaRt returns a heaRt_fit object", {

  dat <- load_heaRt("a-s")
  mod <- fit.heaRt(dat, num_var = "1", fit_type = "logistic.reg")

  expect_s3_class(mod, "heaRt_fit")
  expect_s3_class(mod$model, "glm")
})
