#' Title Function to get AIP code from lon-lat information.
#'
#' @param dataPoints \code{data.frame} which has Longitude and Latitude information.
#' @param colLon Name of column which contains Longitude info.
#' @param colLat Name of column which contains Latitude info.
#' @param old \code{logical}. ¿Desea utilizar la base de datos antigua (\code{old = TRUE}) o nueva \code{old = FALSE}?
#'
#' @export
isopArea.assigner <- function(dataPoints, colLon = "lon", colLat = "lat", old = TRUE){

  referenceShapefile <- get(ifelse(test = isTRUE(old), yes = "AIPShapefile_old", no = "AIPShapefile_new"))

  if(class(dataPoints) == "data.frame"){
    dataPoints <- as.data.frame(dataPoints[,c(colLon, colLat)])
  }else if(class(dataPoints) == "numeric"){
    dataPoints <- data.frame(lon = dataPoints[1], lat = dataPoints[2])
  }

  dataPoints <- switch(class(dataPoints),
                       "data.frame" = as.data.frame(dataPoints[,c(colLon, colLat)]),
                       "numeric" = data.frame(lon = dataPoints[1], lat = dataPoints[2]))

  coordinates(dataPoints) <- dataPoints

  proj4string(dataPoints) <- proj4string(referenceShapefile)

  dataPoints <- over(x = dataPoints, y = referenceShapefile)

  return(dataPoints$code)
}
