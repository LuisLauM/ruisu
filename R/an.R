#' @title Abbreviation for \code{as.numeric}
#'
#' @param x Object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.numeric}.
#'
#' @export
#'
#' @examples
#' as.numeric("02.33")
#' an("02.33")
an <- function(x, ...){
  return(as.numeric(x, ...))
}
