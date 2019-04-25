#' @title Calculate the moving average for a numeric vector
#'
#' @param x Numeric vector.
#' @param n Size for grouping.
#' @param circular If \code{TRUE}, wrap the filter around the ends of the series,
#' otherwise assume external values are missing (\code{NA}).
#' @param ... Extra arguments passed to \code{\link{filter}} function.
#'
#' @return A numeric vector with the same length of the original vector.
#' @export
#'
#' @examples
#' exampleVector <- runif(n = 20, min = 0, max = 100)
#' movingAverage(x = exampleVector)
movingAverage <- function(x, n = 3, circular = TRUE, ...)
{
  output <- filter(x, rep(1/n, n), circular = circular, ...)

  return(an(output))
}
