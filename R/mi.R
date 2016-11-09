#' @title computes permutation importance
#' @description computes the change in prediction error from permuting variables
#'
#' @param data a data.frame including the target variable
#' @param vars a character vector specifying columns of \code{data} to permute
#' @param y a character vector giving the name of the target/outcome variable
#' @param model an object with a predict method which returns a vector or matrix. presumably this object represents a model fit.
#' @param nperm positive integer giving the number of times to permute the indicated variables (default 100)
#' @param predict.fun what function to generate predictions using \code{model}. default is the predict method for \code{model}.
#' @param loss.fun what loss function to use to measure prediction errors. default is MSE for ordered predictions and mean misclassification error for unordered prediction errors.
#' @param contrast.fun what function to use to contrast the permuted and unpermuted predictions. default is the difference.
#'
#' @return a numeric vector of length one giving the change in prediction error from \code{nperm} permutations of \code{vars}.
#'
#' @examples
#' X <- replicate(3, rnorm(100))
#' y <- X %*% runif(3)
#' data <- data.frame(X, y)
#' fit <- lm(y ~ -1 + X1 + X2 + X3, data)
#'
#' permutationImportance(data, "X1", "y", fit)
#' @export
permutationImportance <- function(data, vars, y, model,
  nperm = 100L,
  predict.fun = function(object, newdata) predict(object, newdata = newdata),
  loss.fun = function(x, y) defaultLoss(x, y),
  contrast.fun = function(x, y) mean(x - y)) {
  
  design <- makePermutedDesign(data, vars, nperm)
  unpermuted <- loss.fun(predict.fun(model, data), data[, y])
  permuted.predictions <- predict.fun(model, design)
  permuted.predictions <- array(permuted.predictions, c(nrow(data), nperm))
  permuted <- loss.fun(permuted.predictions, data[, y])
  contrast.fun(permuted, unpermuted)
}

#' @title creates a data.frame with some variables permuted
#' @description takes an input data.frame, permutes some variables, and stacks the resulting data.frames
#'
#' @param data a data.frame
#' @param vars a character vector indicating columns in \code{data} to permute
#' @param nperm an integer specifying the number of times to permute the columns indicated by \code{vars}
#'
#' @return a data.frame with number of rows equal to \code{nrow(data) * nperm}
#'
#' @examples
#' data <- data.frame(x = 1:3, y = letters[1:3])
#' makePermutedDesign(data, "x", 3)
#' @export
makePermutedDesign <- function(data, vars, nperm) {
  design <- data[rep(1:nrow(data), times = nperm), ]
  idx <- lapply(1:nperm, function(x) sample(1:nrow(data)) + (x - 1) * nrow(data))
  idx <- unlist(idx)
  design[, vars] <- design[idx, vars]
  design
}
