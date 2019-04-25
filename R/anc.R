#' @title Abbreviation for \code{as.numeric(as.character(x))}
#'
#' @param x object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.character}.
#'
#' @export
#'
#' @examples
#' exampleVector <- runif(n = 20, min = 0, max = 100)
#' anc(cut(x = exampleVector, breaks = seq(0, 100, 20), labels = 1:5))
anc <- function(x, ...){
  return(as.numeric(as.character(x, ...)))
}
