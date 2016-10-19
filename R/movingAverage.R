#' Title
#'
#' @param x
#' @param n
#' @param circular
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
movingAverage <- function(x, n = 3, circular = TRUE, ...)
{
  output <- filter(x, rep(1/n, n), circular = circular, ...)

  return(an(output))
}
