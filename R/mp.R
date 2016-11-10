#' @title marginalizes prediction functions
#' @description monte-carlo integration of prediction functions
#'
#' @importFrom stats predict
#' 
#' @param data a \code{data.frame} containing the training data excluding the target column(s)
#' @param vars a character vector corresponding to columns in \code{data}.
#' @param model an object with a predict method which returns a vector or matrix. presumably this object represents a model fit.
#' @param n an integer vector of length two giving the resolution of the uniform or random grid on \code{vars} for the first element, and the number of the rows of the training data that are sample for the second element.
#' @param uniform logical indicating whether to create the grid on \code{var} uniformly or to sample from the empirical distribution.
#' @param aggregate.fun what function to aggregate the predictions with
#' @param predict.fun what function to generate predictions using \code{model}. default is the predict method for \code{model}.
#'
#' @return a named list with an element "prediction" which contains an array, matrix, or vector of dimension \code{n[1]}, the column dimension of the output of \code{predict.fun}, and the dimension of the output from \code{aggregate.fun}.
#'
#' @examples
#' X <- replicate(3, rnorm(100))
#' y <- X %*% runif(3)
#' data <- data.frame(X, y)
#' fit <- lm(y ~ -1 + X)
#' 
#' marginalPrediction(data.frame(X), "X2", c(10, 25), fit,
#'   aggregate.fun = function(x) c("mean" = mean(x), "variance" = var(x)))
#'
#' @export
marginalPrediction <- function(data, vars, n, model, uniform = TRUE,
 aggregate.fun = mean, predict.fun = function(object, newdata)
   predict(object, newdata = newdata)) {

  stopifnot(class(data) == "data.frame")
  stopifnot(class(vars) == "character")

  design <- makeGrid(data, vars, n, uniform)
  n[1] <- nrow(unique(design[, vars, drop = FALSE]))
  preds <- predict.fun(model, design)
    
  if (is.matrix(preds) | is.data.frame(preds)) {
    preds <- array(preds, c(n, ncol(preds)))
    mp <- apply(preds, c(1, 3), aggregate.fun)
  } else {
    preds <- array(preds, n)
    mp <- apply(preds, 1, aggregate.fun)
    if (!is.vector(mp)) {
      ## apply with a vector valued fun needs a transpose
      mp <- t(mp)
    }
  }

  list(
    "prediction" = mp,
    "points" = unique(design[, vars, drop = FALSE])
  )
}
