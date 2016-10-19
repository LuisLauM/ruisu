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
#'
#' n <- 100
#'
#' allData <- data.frame(lon = runif(n, -80, -70), lat = runif(n, -18, -2),
#'                       stringsAsFactors = FALSE)
#'
#' allData <- allData[!is.na(isopArea.assigner(allData)),]
#'
#' minValues <- minDistanceToCoast(allData)
#'
#' xlim <- c(-85, -70)
#' ylim <- c(-20, -2)
#'
#' x11()
#' par(mar = c(2, 3, 1, 1), xaxs = "i", yaxs = "i")
#' plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)
#'
#' points(allData$lon, allData$lat, pch = 16, cex = 0.5, xlim = c(-85, -70), ylim = c(-20, -2))
#' lines(coastline$lon, coastline$lat)
#'
#'
#' for(i in 1:nrow(minValues$position)){
#'   lines(c(allData$lon[i], minValues$position$lon[i]), c(allData$lat[i], minValues$position$lat[i]),
#'         lty = "dotted", col = "red")
#' }
#'
#' axis(side = 1, at = seq(xlim[1], xlim[2], length.out = 4),
#'      labels = getCoordsAxes(seq(xlim[1], xlim[2], length.out = 4), "lon"))
#' axis(side = 2, at = seq(ylim[1], ylim[2], length.out = 10),
#'      labels = getCoordsAxes(seq(ylim[1], ylim[2], length.out = 10), "lat"), las = 2)
#' box()
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
  minDistancesPosition <- coastline[index & apply(allDistances, 2, which.min), c("lon", "lat")]

  unitFactor <- switch(tolower(unit),
                       mn = 1/1.852,
                       degrees = 1,
                       m = 1/1852)

  return(list(value = minDistancesValue*unitFactor,
              position = minDistancesPosition))
}
