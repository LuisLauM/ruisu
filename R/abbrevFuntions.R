#' Abbreviation for \code{as.numeric}
#'
#' @param x Object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.numeric}.
#'
#' @return
#' @export
#'
#' @examples
an <- function(x, ...){
  return(as.numeric(x, ...))
}

#' Abbreviation for \code{as.character}
#'
#' @param x object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.character}.
#'
#' @return
#' @export
#'
#' @examples
ac <- function(x, ...){
  return(as.character(x, ...))
}

#' Abbreviation for \code{as.numeric(as.character(x))}
#'
#' @param x object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.character}.
#'
#' @return
#' @export
#'
#' @examples
anc <- function(x, ...){
  return(as.numeric(as.character(x, ...)))
}
