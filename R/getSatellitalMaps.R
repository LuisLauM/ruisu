#' @title Get satellital information for oceanographic variables
#' @description This function uses date and space range and get maps for Sea Surface Temperature (SST),
#' Sea Surface Salinity (SSS), Chlorophyll-a (Chl), Topography (topo), Sea level (sealevel) and SODA
#' estimations for Temperature (sst_soda) and Salinity (sss_soda) by depth (\code{atDepth} parameter).
#'
#' @param initialDate Initial date Fecha inicial para la extracción de mapas.
#' @param finalDate Fecha final para la extracción de mapas.
#' @param timeRes Resolución temporal para la obtención de mapas: Mensual ('month') o diario ('day').
#' @param what ¿Qué variable desea descargar? Ver detalles.
#' @param dateList Lista de fechas para la extracción de mapas.
#' @param lonRange Rango de coordenadas de longitud. Si es \code{NULL}, tomará valores entre 85°W y 90°W.
#' @param latRange Rango de coordenadas de latitud. Si es \code{NULL}, tomará valores entre 20°S y 2°S.
#' @param outputFormat Formato de descarga de archivos: NetCDF (nc), Valores separados por comas (csv),
#' image (png).
#' @param outputDir Directorio para la descarga de archivos.
#' @param atDepth Define depth value for getting info.
#' @param showURL Do you want to show the URL each time?
#'
#' @export
#'
#' @examples
#' getSatellitalMaps(initialDate = "2014-3-26", finalDate = "2014-4-17", what = "sst", timeRes = "day",
#' lonRange = c(-90, -85), latRange = c(-5, 0), outputFormat = "png",
#' outputDir = "../")

getSatellitalMaps <- function(initialDate = NULL, finalDate = NULL, timeRes = "month", what = "SST", atDepth = NULL,
                              dateList = NULL, lonRange = c(-85, -70), latRange = c(-20, -2), outputFormat = "png",
                              outputDir = ".", showURL = FALSE){

  # if(what != "png" && isTRUE(compile)){
  #   outputDir <- tempdir()
  # }

  getSatellitalMaps_internal(initialDate, finalDate, timeRes, what, atDepth, dateList, lonRange, latRange, outputFormat,
                             outputDir, showURL)

  cat(paste0("\n Download finished! There were ", length(errorList), " problems. \n"))

  # if(what != "png" && isTRUE(compile)){
  #
  #   cat("\nMaking compilation.. .")
  #
  #
  #
  #   cat("\nCompilation finished.")
  # }

  # Final output will be a list with failed URLs
  return(if(is.null(errorList)) invisible() else errorList)
}
