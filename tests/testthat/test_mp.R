test_that("marginalPrediction works", {
  X = replicate(3, rnorm(50))
  y = X %*% runif(3)
  data = data.frame(X, y)
  fit = lm(y ~ -1 + X1 + X2 + X3, data)
  tmp = marginalPrediction(data, c("X1", "X2"), c(10, 25), fit, FALSE,
    aggregate.fun = function(x) var(x))

  expect_that(tmp, is_a("list"))
  expect_that(names(tmp), equals(c("prediction", "points")))
  expect_that(tmp[["prediction"]], is_a("numeric"))
  expect_that(all(tmp[["prediction"]] > 0), is_true())
  expect_that(tmp[["points"]], is_a("data.frame"))
  expect_that(nrow(tmp[["points"]]), equals(10))

  tmp = marginalPrediction(data, c("X1", "X2"), c(10, 25), fit, TRUE,
    aggregate.fun = identity)
  expect_that(dim(tmp[["prediction"]]), equals(c(100, 25)))

  tmp = marginalPrediction(data, c("X1", "X2"), c(10, 25), fit, TRUE,
    aggregate.fun = function(x) c("mean" = mean(x), "variance" = var(x)))
  expect_that(colnames(tmp[["prediction"]]), equals(c("mean", "variance")))
})
