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
#' @param aggregate.fun what function to aggregate the predictions with. this function takes a single argument \code{x} and returns a vector. the default is \code{sum(x) / length(x)}.
#' @param predict.fun what function to generate predictions using \code{model}. default is the predict method for \code{model}. this function must have two arguments, \code{object} and \code{newdata}.
#'
#' @return a named list with an element "prediction" which contains an array, matrix, or vector of dimension \code{n[1]}, the column dimension of the output of \code{predict.fun}, and the dimension of the output from \code{aggregate.fun}.
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
 aggregate.fun = function(x) sum(x) / length(x), predict.fun = function(object, newdata)
   predict(object, newdata = newdata)) {

  assertFunction(aggregate.fun, args = "x", nargs = 1L)
  assertFunction(predict.fun, args = c("object", "newdata"), nargs = 2L)

  design = makeDesign(data, vars, n, uniform, points, int.points)
  n[1] = nrow(unique(design[, vars, drop = FALSE])) ## improve
  preds = predict.fun(model, design)
    
  if (is.matrix(preds) | is.data.frame(preds)) {
    save = colnames(preds)
    preds = array(preds, c(n, ncol(preds)))
    mp = apply(preds, c(1, 3), aggregate.fun)
    colnames(mp) = save
  } else {
    preds = array(preds, n)
    mp = apply(preds, 1, aggregate.fun)
    if (!is.vector(mp)) {
      ## apply with a vector valued fun needs a transpose
      mp = t(mp)
    }
  }

  list(
    "prediction" = mp,
    "points" = unique(design[, vars, drop = FALSE]) ## improve
  )
}
