getSatellitalValue <- function(data, colLon = "lon", colLat = "lat", what = NULL){

  # what: sst, sss, chl, topo

  data <- data[,c(colLon, colLat)]

  if(!is.null(depthDatafile))
    depthData <- open.ncdf(depthData) else
      depthData <- data10

  lonData <- depthData$dim$longitude$vals
  latData <- depthData$dim$latitude$vals
  depthData <- t(data11)

  data <- data[data[,1] >= min(lonData) & data[,1] <= max(lonData) &
                 data[,2] >= min(latData) & data[,2] <= max(latData),]

  depthData[depthData > 0] <- NA

  depthData <- raster(depthData, xmn = min(lonData), xmx = max(lonData),
                      ymn = min(latData), ymx = max(latData))

  return(extract(depthData, data))
}
