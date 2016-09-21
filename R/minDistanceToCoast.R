#' Title
#'
#' @param data \code{data.frame} with coords that will be used to calculate min distance to coast line.
#' @param colLon Name or position of column for longitude. As default, it will be \code{lon}.
#' @param colLat Name or position of column for latitude As default, it will be \code{lat}.
#' @param countryFilter Select the country for make comparation
#' @param unit Define the unit for outputs: nm (nautical miles), degrees, m (meters).
#'
#' @export
#'
#' @examples
#' n <- 10000
#' allData <- data.frame(lon = runif(n, -80, -70), lat = runif(n, -18, -2),
#'                       stringsAsFactors = FALSE)
#'
#' # Only consider data on the sea
#' allData <- allData[!is.na(isopArea.assigner(allData)),]
#'
#' # Get distances
#' minValues <- minDistanceToCoast(allData)
minDistanceToCoast <- function(data, colLon = "lon", colLat = "lat", countryFilter = "peru", unit = "mn"){

  pointsRange <- range(data[,colLat], na.rm = TRUE)

  if(is.null(countryFilter)){
    refLines <- coastline[tolower(coastline$country) == tolower(countryFilter),]
  }else{
    index <- range(data[,colLat]) + c(-20, 20)
    index <- coastline$lat > index[1] & coastline$lat < index[2]
    refLines <- coastline[index,]
  }

  allDistances <- spDists(x = as.matrix(coastline[index, c("lon", "lat")]),
                          y = as.matrix(data[,c(colLon, colLat)]), longlat = TRUE)
  minDistancesValue <- apply(allDistances, 2, min)


  unitFactor <- switch(tolower(unit),
                       mn = 1/1.852,
                       degrees = 1,
                       m = 1/1852)

  return(minDistancesValue*unitFactor)
}
