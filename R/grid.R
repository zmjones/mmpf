#' @title method to create an evenly spaced grid on a variable
#'
#' @description generates and evenly spaced grid given an input vector or \code{data.frame} which has size \code{length.out}
#'
#' @param x a vector or \code{data.frame} to create a grid on
#' @param length.out an integer giving the length of the grid
#'
#' @return an object of the same type as \code{x}, with \code{length.out} unique values
#' @examples
#'
#' data = data.frame(
#'   w = seq(0, 1, length.out = 5),
#'   x = factor(letters[1:5]),
#'   y = ordered(1:5),
#'   z = 1:5
#' )
#' variableGrid(data, 3)
#'
#' @export
variableGrid <- function(x, length.out) UseMethod("variableGrid")

#' @export
variableGrid.numeric <- function(x, length.out) {
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)
}

#' @export
variableGrid.integer <- function(x, length.out) {
  as.integer(seq.int(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
    length.out = length.out))
}

#' @export
variableGrid.factor <- function(x, length.out, ...) {
  if (is.ordered(x)) {
    sort(unique(x))[as.integer(seq.int(1, length(unique(x)), length.out = length.out))]
  } else {
    ## impossible to order selection if length.out < length(unique(x)) w/o ordering
    sample(unique(x), size = length.out)
  }
}

#' @export
variableGrid.character <- function(x, length.out, ...) {
  sample(unique(x), size = length.out)
}

#' @export
variableGrid.data.frame <- function(x, length.out) {
  as.data.frame(lapply(x, variableGrid, length.out = length.out),
    stringsAsFactors = FALSE)
}

#' @export
variableGrid.matrix <- function(x, length.out) {
  apply(x, 2, variableGrid, length.out = length.out)
}

#' @title expands two data.frames using the cartesian product
#' @description takes the cartesian product of two data.frames
#'
#' @param x a \code{data.frame}
#' @param y a \code{data.frame}
#' @return a \code{data.frame}
#'
#' @examples
#' x <- data.frame("a" = 1:5, "b" = 6:10)
#' y <- data.frame("z" = letters[1:5], "y" = letters[6:10])
#' cartesianExpand(x, y)
#' 
#' @export
cartesianExpand <- function(x, y) {
  A <- as.data.frame(array(dim = c(dim(x)[1] * dim(y)[1], dim(x)[2] + dim(y)[2])))
  idx <- rep(1:dim(x)[1], dim(y)[1])
  A[, 1:dim(x)[2]] <- x[idx,, drop = FALSE]

  idy <- rep(1:dim(y)[1], dim(x)[1])
  for (j in 1:dim(y)[2]) {
    A[, dim(x)[2] + j] <- y[idy, j, drop = FALSE]
  }
  A
}
#' @title make a uniform or random grid over some columns of a data.frame
#' @description makes a uniform or random grid over some columns of a data.frame and takes their cartesian product with the other columns
#'
#' @param data a \code{data.frame}
#' @param vars integer indices indicating the variables to create the grid for
#' @param n two dimensional integer vector giving the resolution of the grid. the first element gives the grid on \code{vars} and the second on the other columns, which are subsampled.
#' @param uniform logical, indicates whether a uniform or random grid is to be constructed.
#' @return a \code{data.frame} with \code{n} dimensions.
#'
#' @examples
#'
#' data <- data.frame(w = seq(0, 1, length.out = 5),
#'                    x = factor(letters[1:5]),
#'                    y = ordered(1:5),
#'                    z = 1:5,
#'                    r = letters[1:5],
#'                    stringsAsFactors = FALSE)
#' makeGrid(data, 1, c(10, 5), TRUE)
#'
#' @export
makeGrid <- function(data, vars, n, uniform) {
  ## arg checks
  stopifnot(is.numeric(n) && all.equal(n, round(n, 0)) && length(n) == 2L)
  stopifnot(is.logical(uniform))
  stopifnot(class(data) %in% c("data.frame", "matrix"))
  stopifnot(is.numeric(vars) && all.equal(round(vars, 0), vars) &&
              max(vars) <= ncol(data) && min(vars) >= 1)

  ## create points for grid or sample from training data
  if (uniform) {
    points <- variableGrid(data[, vars, drop = FALSE], n[1])
  } else {
    id <- sample(1:nrow(data), n[1])
    points <- data[id, vars]
  }

  ## subsample training data, combine
  nvars <- seq_len(ncol(data))
  nvars <- nvars[!nvars %in% vars]

  design <- cartesianExpand(points,
    data[sample(seq_len(nrow(data)), n[2]), -vars, drop = FALSE])
  design <- design[, order(c(vars, nvars), seq_len(ncol(data)))]
  colnames(design) <- colnames(data)

  ## check names and classes match
  if (is.data.frame(data)) {
    classes <- sapply(data, class)
    new_classes <- sapply(design, class)
    stopifnot(identical(classes, new_classes))
  }

  design
}
