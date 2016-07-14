isopArea.assigner <- function(dataPoints, colLon = "lon", colLat = "lat", old = TRUE){

  referenceShapefile <- get(ifelse(test = isTRUE(old), yes = "AIPShapefile_old", no = "AIPShapefile_new"))

  dataPoints <- as.data.frame(dataPoints[,c(colLon, colLat)])

  coordinates(dataPoints) <- dataPoints

  proj4string(dataPoints) <- proj4string(referenceShapefile)

  dataPoints <- over(x = dataPoints, y = referenceShapefile)

  return(dataPoints$code)
}
