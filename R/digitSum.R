#' @name digitsum
#' @aliases digitsum
#' @title Devuelve la suma de los dígitos de un número.
#'
#' @description Función para separar los dígitos de un número y obtener la suma de ellos.
#' Si \code{recursive = TRUE}, la suma se realizará hasta que el resultado final conste de una sola cifra.
#'
#' @usage digitsum(x, recursive = FALSE)
#' @param x Número o vector de números enteros o fraccionarios.
#' @param recursive ¿Desea que la suma se realice hasta que el resultado conste de un dígito?
#'
#' @example digitsum(1516)
#' # [1] 13
#'
#' digitsum(1516, recursive = TRUE)
#' # [1] 4
digitsum <- function(x, recursive = FALSE)
{
  .digitSum <- function(x, recursive = FALSE)
    if(recursive)
      while(nchar(x) > 1)
        x <- sum(as.numeric(unlist(strsplit(as.character(x), ""))), na.rm = TRUE) else
          x <- sum(as.numeric(unlist(strsplit(as.character(x), ""))), na.rm = TRUE)

  if(length(x) == 1)
    x <- .digitSum(x = x, recursive = recursive) else
      x <- sapply(x, .digitSum, recursive = recursive)

  return(x)
}
