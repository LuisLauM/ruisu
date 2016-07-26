#
#' Title plotIsoparalitoral
#'
#' @param codeList Vector conteniendo los códigos de AIP a plotear.
#' @param add ¿Desea agregar las AIP a un gráfico presente (\code{TRUE}) o desea plotear un mapa
#' con líneas de costa (\code{FALSE})?
#' @param old ¿Desea utilizar las AIP anteriores (\code{AIPShapefile_old}) o las nuevas (\code{AIPShapefile_new})?
#' @param plotParams Objeto \code{lista} conteniendo parámetros adicionales pasados a la función \code{plot}. Se
#' usará solo si \code{add = FALSE}.
#' @param mapParams Objeto \code{lista} conteniendo parámetros adicionales pasados a la función \code{map}. Se
#' usará solo si \code{add = FALSE}.
#' @param ... Parámetros adicionales pasados a la función \code{\link{plot.SpatialPolygons}}.
#'
#' @export
plotIsoparalitoral <- function(codeList = NULL, add = FALSE, old = TRUE,
                               plotParams = list(xlim = c(-90, -70), ylim = c(-18, -2), axes = TRUE),
                               mapParams = list(database = "world"), ...)
{

  # Select shape
  referenceShapefile <- get(ifelse(test = isTRUE(old), yes = "AIPShapefile_old", no = "AIPShapefile_new"))

  if(!isTRUE(add)){
    # Make plot
    do.call(what = plot, args = c(list(x = 1, y = 1), plotParams))

    # Make coast line
    do.call(what = map, args = c(list(add = TRUE), mapParams))
  }


  if(is.null(codeList)){
    codeList <- referenceShapefile$code
  }

  index <- match(codeList, referenceShapefile$code)

  if(sum(is.na(index)) == length(index)){
    stop("Ninguno de los valores de 'codeList' pertenece a un Área Isoparalitoral válida.")
  }

  codeList <- index

  plot(referenceShapefile[codeList, 1], add = TRUE, ...)

  return(invisible())
}
