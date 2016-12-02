#' @title marginalizes prediction functions
#' @description monte-carlo integration of prediction functions
#'
#' @importFrom stats predict
#' @import checkmate
#' 
#' @param data a \code{data.frame} which contains the columns specified by \code{vars} and at least one additional column. should correspond to the set of columns used to train the \code{model}.
#' @param vars a character vector corresponding to a strict subset of the columns in \code{data}.
#' @param model an object which can be passed to \code{predict.fun} to compute predictions. presumably this object represents a model fit.
#' @param n an integer vector of length two giving the resolution of the uniform or random grid on \code{vars} for the first element, and the number of the rows of the \code{data} to be sampled without replacement for the second element.
#' @param uniform logical indicating whether to create the grid on \code{vars} uniformly or to sample without replacement from the empirical distribution of those \code{vars}.
#' @param points a named list which gives specific points for \code{vars}. specifying this argument overrides \code{uniform}.
#' @param int.points a integer vector giving indices of the points in \code{data} to marginalize over.
#' @param aggregate.fun what function to aggregate the predictions with. this function takes a single argument \code{x} and returns a vector. the default is \code{sum(x) / length(x)}. If \code{weight.fun} is used, this function must also take a numeric parameter \code{w}.
#' @param predict.fun what function to generate predictions using \code{model}. default is the predict method for \code{model}. this function must have two arguments, \code{object} and \code{newdata}.
#' @param weight.fun a function to construct weights for \code{aggregate.fun}. this allows Monte-Carlo integration on a grid without assuming a uniform distribution for said grid. the function should take two arguments, \code{design} and \code{data}, both of which are \code{data.frame}s of the same column (but different row) dimension, and should return a numeric vector of the same length as the number of rows in \code{design}. If this argument is used \code{aggregate.fun} must also have an argument \code{w} which is the result of \code{weight.fun}.
#'
#' @return a \code{data.table} with columns for predictions and \code{vars}.
#'
#' @examples
#' X = replicate(3, rnorm(100))
#' y = X %*% runif(3)
#' data = data.frame(X, y)
#' fit = lm(y ~ ., data)
#' 
#' marginalPrediction(data.frame(X), "X2", c(10, 25), fit,
#'   aggregate.fun = function(x) c("mean" = mean(x), "variance" = var(x)))
#' @export
marginalPrediction = function(data, vars, n, model, uniform = TRUE, points, int.points,
  aggregate.fun = function(x) sum(x) / length(x),
  predict.fun = function(object, newdata) predict(object, newdata = newdata),
  weight.fun = NULL) {

  assertFunction(aggregate.fun, args = "x")
  assertFunction(predict.fun, args = c("object", "newdata"))

  design = makeDesign(data, vars, n, uniform, points, int.points)
  preds = predict.fun(model, design)

  if (!is.null(weight.fun)) {
    assertFunction(weight.fun, args = c("design", "data"))
    assertFunction(aggregate.fun, args = c("x", "w"))
    w = weight.fun(design, data)
    mp = data.table(w, preds, design[, vars, drop = FALSE], key = vars)
    mp[, as.list(unlist(lapply(.SD[, !"w", with = FALSE], aggregate.fun, w = w))), by = vars]
  } else {
    mp = data.table(preds, design[, vars, drop = FALSE], key = vars)
    mp[, as.list(unlist(lapply(.SD, aggregate.fun))), by = vars]
  }
}
