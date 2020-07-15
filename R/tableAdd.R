#' @title Add info (total, mean, sd, etc.) to matrix or table
#'
#' @description This functions add values from a certain function to the beggining or
#' end of a table.
#'
#' @param x \code{data.frame} or \code{matrix} object. All values must be \code{numeric}.
#' @param side Indicates the side (1: bottom, 2: left, 3: top, 4: right) for adding info.
#' @param FUN a function (or name of a function) to be applied. The indicated function
#' must return \code{atomic} vectors.
#' @param label What name do you want to use for extra column/row?
#' @param ... Extra arguments passed to \code{FUN.}
#'
#' @return Same class object of \code{x}.
#' @export
#'
#' @examples
#' tableAdd(x = mtcars)
tableAdd <- function(x, side = c(1, 4), FUN = sum, label = "Total", ...){
  # Get function
  FUN <- match.fun(FUN)

  output <- list(x = apply(x, 2, FUN, ...),
                 y = apply(x, 1, FUN, ...))

  if(any(is.element(c(1, 3), side)) & any(is.element(c(2, 4), side))){
    output$x <- c(output$x, sum(output$x))
    output$y <- c(output$y, sum(output$y))
  }

  # Add info to the bottom
  if(is.element(1, side)){
    x <- rbind(x, output$x)
    rownames(x)[nrow(x)] <- label
  }

  # Add info to the left
  if(is.element(2, side)){
    x <- cbind(output$y, x)
    colnames(x)[1] <- label
  }

  # Add info to the top
  if(is.element(3, side)){
    x <- rbind(output$x, x)
    rownames(x)[1] <- label
  }

  # Add info to the right
  if(is.element(4, side)){
    x <- cbind(x, output$y)
    colnames(x)[ncol(x)] <- label
  }

  return(x)
}
