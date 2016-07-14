#' Función para obtener proporciones a partir de una tabla
#'
#' @description Toma una tabla y devuelve los valores proporcionales ya sea por fila,
#' columna o la tabla completa.
#'
#' @param table Tabla en modo \code{data.frame} o \code{matrix}. Todos los valores deben
#' ser numéricos.
#' @param group Desea que las proporciones se tomen por fila (\code{group = 1}), columna
#' (\code{group = 2}) o la tabla completa (\code{group = 3}, valor por defecto).
#'
#' @return Una objeto (clase \code{matrix}) de las mismas dimensiones que el original con
#' las proporciones solicitadas
#' @export
#'
#' @examples
#' myTable <- matrix(data = runif(n = 15, min = 0, max = 30), ncol = 3)
#' getProportion(table = myTable)
getProportion <- function(table, group = 3)
{
  table <- as.matrix(table)

  if(group == 1){
    output <- t(apply(table, group, function(x) x/sum(x, na.rm = TRUE)))
  }else if(group == 2){
    output <- apply(table, group, function(x) x/sum(x, na.rm = TRUE))
  }else if(group == 3){
    output <- table/sum(table, na.rm = TRUE)
  }

  return(output)
}
