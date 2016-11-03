test_that("variableGrid works", {
  data <- data.frame(
    w = seq(0, 1, length.out = 5),
    x = factor(letters[1:5]),
    y = ordered(1:5),
    z = 1:5,
    r = letters[1:5],
    stringsAsFactors = FALSE)

  n <- 3
  tmp <- variableGrid(data, n)
  expect_that(tmp, is_a("data.frame"))
  expect_that(sapply(data, class), equals(sapply(tmp, class)))

  tmp <- variableGrid(data$w, n)
  expect_that(tmp, is_a("numeric"))
  expect_that(range(tmp), equals(range(data$w)))
  expect_that(length(tmp), equals(n))

  tmp <- variableGrid(data$x, n)
  expect_that(tmp, is_a("factor"))
  expect_that(length(tmp), equals(n))

  tmp <- variableGrid(data$y, n)
  expect_that(tmp, is_a("ordered"))
  expect_that(range(tmp), equals(range(data$y)))
  expect_that(length(tmp), equals(n))

  tmp <- variableGrid(data$z, n)
  expect_that(tmp, is_a("integer"))
  expect_that(range(tmp), equals(range(data$z)))
  expect_that(length(tmp), equals(n))

  tmp <- variableGrid(data$r, n)
  expect_that(tmp, is_a("character"))
  expect_that(length(tmp), equals(n))
})

test_that("cartesianExpand works", {
  x <- data.frame("a" = 1:5, "b" = 6:10)
  y <- data.frame("z" = letters[1:5], "y" = letters[6:10])
  tmp <- cartesianExpand(x, y)
  expect_that(dim(tmp), equals(c(dim(x)[1] * dim(y)[1], 4)))
})

test_that("makeGrid works", {
  data <- data.frame(w = seq(0, 1, length.out = 5),
    x = factor(letters[1:5]),
    y = ordered(1:5),
    z = 1:5,
    r = letters[1:5],
    stringsAsFactors = FALSE)
  tmp <- makeGrid(data, 1, c(10, 5), TRUE)

  expect_that(dim(tmp), equals(c(10 * 5, dim(data)[2])))
  expect_that(length(unique(tmp[, 1])), equals(10))
  expect_that(all(!is.na(tmp)), is_true())
})
