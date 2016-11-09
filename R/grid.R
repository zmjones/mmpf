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
#'
#' lapply(data, variableGrid, length.out = 5)
#' @export
variableGrid <- function(x, length.out) UseMethod("variableGrid")

#' @export
variableGrid.numeric <- function(x, length.out) {
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)
}

#' @export
variableGrid.integer <- function(x, length.out) {
  min.x <- min(x, na.rm = TRUE)
  max.x <- max(x, na.rm = TRUE)
  x.length <- max.x - min.x
  if (length.out > x.length) {
    min.x:max.x
  } else {
    as.integer(round(seq.int(min.x, max.x, length.out = length.out)), 0)
  }
}

#' @export
variableGrid.factor <- function(x, length.out, ...) {
  x.length <- length(unique(x))
  if (length.out >= x.length) {
    sort(unique(x))
  } else {
    if (is.ordered(x)) {
      unique(x)[variableGrid(seq_len(x.length), length.out)]
    } else {
      warning("length.out is less than the number of levels")
      sort(sample(unique(x), size = length.out))
    }
  }
}

#' @export
variableGrid.character <- function(x, length.out, ...) {
  x.length <- length(unique(x))
  if (length.out < x.length) {
    warning("length.out is less than the number of unique values")
  }
  sample(unique(x), size = min(length.out, x.length))
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
  colnames(A) <- c(colnames(x), colnames(y))
  A
}
#' @title make a uniform or random grid over some columns of a data.frame
#' @description makes a uniform or random grid over some columns of a data.frame and takes their cartesian product with the other columns
#'
#' @param data a \code{data.frame}
#' @param vars character vector the columns in data to create the grid for
#' @param n two dimensional integer vector giving the resolution of the grid. the first element gives the grid on \code{vars} and the second on the other columns, which are subsampled.
#' @param uniform logical, indicates whether a uniform or random grid is to be constructed.
#' @param points a named list which gives specific points for \code{vars}.
#' @return a \code{data.frame} with at most \code{n} dimensions.
#'
#' @examples
#' data <- data.frame(w = seq(0, 1, length.out = 5),
#'                    x = factor(letters[1:5]),
#'                    y = ordered(1:5),
#'                    z = 1:5,
#'                    r = letters[1:5],
#'                    stringsAsFactors = FALSE)
#' makeGrid(data, "z", c(10, 5), TRUE)
#'
#' @export
makeGrid <- function(data, vars, n, uniform = TRUE, points) {
  ## arg checks
  stopifnot(is.numeric(n) && all.equal(n, round(n, 0)) && length(n) == 2L)
  stopifnot(is.logical(uniform))
  stopifnot(class(data) %in% c("data.frame", "matrix"))
  stopifnot(class(vars) == "character")

  if (missing(points)) {
     ## create points for grid or sample from training data
    if (uniform) {
      if (length(vars) > 1) {
        ## combine individual grids
        points <- expand.grid(sapply(vars,
          function(x) variableGrid(data[[x]], length.out = n[1]), simplify = FALSE),
          stringsAsFactors = FALSE)
      } else {
        points <- variableGrid(data[[vars]], n[1])
        points <- as.data.frame(points, stringsAsFactors = FALSE)
        colnames(points) <- vars
      }
    } else {
      ## randomly sample points w/o replacement
      id <- sample(1:nrow(data), n[1])
      points <- data[id, vars]
    }
  } else {
    uniform <- FALSE
    ## combine user specified points
    stopifnot(all(names(points) %in% vars))
    stopifnot(all(sapply(points, class) %in%
                    sapply(data[, vars, drop = FALSE], class)))
    points <- expand.grid(points, stringsAsFactors = FALSE)
  }

  ## subsample training data, combine
  nvars <- colnames(data)[!colnames(data) %in% vars]

  ## combine points with sampled points
  design <- cartesianExpand(points,
    data[sample(seq_len(nrow(data)), min(n[2], nrow(data))),
      !colnames(data) %in% vars, drop = FALSE])
  design <- design[, colnames(data)]

  ## check names and classes match
  if (is.data.frame(data)) {
    classes <- sapply(data, class)
    new_classes <- sapply(design, class)
    stopifnot(identical(classes, new_classes))
  }

  design
}

