#' @title Abbreviation for \code{as.character}
#'
#' @param x object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.character}.
#'
#' @export
#'
#' @examples
#' as.character(TRUE)
#' ac(TRUE)
ac <- function(x, ...){
  return(as.character(x, ...))
}
