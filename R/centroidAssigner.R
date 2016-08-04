#' @name centroidAssigner
#' @aliases centroidAssigner
#' @title Returns centroid values from Isoparalitoral-area codes.
#'
#' @description Esta función toma un vector de códigos de AIP y retorna las coordenadas de los  centroides
#' (longitud, latitud). El parámetro \code{old} controla la base de AIP a utilizar: Si \code{old = TRUE},
#' se usará la base de AIP creada en llos 90'; de otro modo, se utilizará la creada en 2016.
#'
#' @usage centroidAssigner(isoCode, old = TRUE)
#'
#' @param isoCode Vector de códigos de AIP de los que se desea obtener  los centroides.
#' @param old \code{logical}. ¿Desea utilizar la base de datos antigua (\code{old = TRUE}) o nueva \code{old = FALSE}?
#'
#' @export
#'
#' @examples
#' areaCodes <- c(1050, 4043, 17073, 27103)
#' centroidAssigner(isoCode = areaCodes)
centroidAssigner <- function(isoCode, old = TRUE)
{
  isoAreas <- ifelse(isTRUE(old), "AIPData_old", "AIPData_new")

  isoAreas <- get(isoAreas)

  output <- data.frame(lon = isoAreas[match(isoCode, isoAreas$code), "lon"],
                       lat = isoAreas[match(isoCode, isoAreas$code), "lat"],
                       stringsAsFactors = FALSE)

  return(output)
}
