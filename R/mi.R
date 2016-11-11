#' @title computes permutation importance
#' @description computes the change in prediction error from permuting variables.
#'
#' @import checkmate
#'
#' @param data a \code{data.frame} including both \code{y} and \code{vars}.
#' @param vars a character vector specifying columns of \code{data} to permute.
#' @param y a character vector giving the name of the target/outcome variable.
#' @param model an object with a predict method which returns a vector or matrix. presumably this object represents a model fit.
#' @param nperm positive integer giving the number of times to permute the indicated variables (default is 100).
#' @param predict.fun what function to generate predictions using \code{model}. default is the predict method for \code{model}. the function must take two arguments, \code{object} and \code{newdata} and should return a vector or matrix.
#' @param loss.fun what loss function to use to measure prediction errors. default is mean squared-error for ordered predictions and mean misclassification error for unordered prediction errors. this function must take two arguments, \dQuote{x} and \dQuote{y}, which operate on the output of \code{predict.fun} and \code{data[, y]}.
#' @param contrast.fun what function to use to contrast the permuted and unpermuted predictions. default is the difference. this function takes two arguments \dQuote{x} and \dQuote{y}, which are the output of the \code{loss.fun}.
#'
#' @return a numeric vector or matrix, depending on \code{contrast.fun} and \code{loss.fun}, giving the change in prediction error from \code{nperm} permutations of \code{vars}.
#'
#' @examples
#' X = replicate(3, rnorm(100))
#' y = X %*% runif(3)
#' data = data.frame(X, y)
#' fit = lm(y ~ -1 + X1 + X2 + X3, data)
#'
#' permutationImportance(data, "X1", "y", fit)
#' @export
permutationImportance = function(data, vars, y, model,
  nperm = 100L,
  predict.fun = function(object, newdata) predict(object, newdata = newdata),
  loss.fun = function(x, y) defaultLoss(x, y),
  contrast.fun = function(x, y) x - y) {

  assertCharacter(vars, min.len = 1L, max.len = ncol(data) - 1L)
  assertCharacter(y, len = 1L)
  assertDataFrame(data, min.rows = 1L, min.cols = length(vars) + 1L)
  assertSubset(c(vars, y), colnames(data), FALSE)
  assertCount(nperm, positive = TRUE)
  assertFunction(predict.fun, args = c("object", "newdata"), nargs = 2L)
  assertFunction(loss.fun, args = c("x", "y"), ordered = TRUE)
  assertFunction(contrast.fun, args = c("x", "y"), ordered = TRUE)
  
  design = makePermutedDesign(data, vars, nperm)
  unpermuted = loss.fun(predict.fun(model, data), data[, y])
  permuted.predictions = predict.fun(model, design)
  permuted = loss.fun(permuted.predictions, design[, y])
  if (length(permuted) == nperm * nrow(data)) {
    permuted <- array(permuted, c(nrow(data), nperm))
  }
  contrast.fun(permuted, unpermuted)
}

#' @title creates a \code{data.frame} with some columns permuted
#' @description takes an input data.frame, permutes some variables, and stacks the resulting \code{data.frame}s.
#'
#' @import checkmate
#' 
#' @param data a \code{data.frame} a subset of which must be \code{vars}.
#' @param vars a character vector indicating columns in \code{data} to permute.
#' @param nperm an integer specifying the number of times to permute the columns indicated by \code{vars}.
#'
#' @return a \code{data.frame} with number of rows equal to \code{nrow(data) * nperm}
#'
#' @examples
#' data = data.frame(x = 1:3, y = letters[1:3])
#' makePermutedDesign(data, "x", 3)
#' @export
makePermutedDesign = function(data, vars, nperm) {
  assertCharacter(vars, min.len = 1L, max.len = ncol(data) - 1L)
  assertDataFrame(data, min.rows = 1L, min.cols = length(vars) + 1L)
  assertSubset(vars, colnames(data), FALSE)
  assertCount(nperm, positive = TRUE)
  
  design = data[rep(1:nrow(data), times = nperm), ]
  idx = lapply(1:nperm, function(x) sample(1:nrow(data)) + (x - 1) * nrow(data))
  idx = unlist(idx)
  design[, vars] = design[idx, vars]
  design
}
