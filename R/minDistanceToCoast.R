#' Title
#'
#' @param data
#' @param colLon
#' @param colLat
#'
#' @return
#' @export
#'
#' @examples
minDistanceToCoast <- function(data, colLon = "lon", colLat = "lat"){

  allDistances <- spDists(x = as.matrix(coastline[,c("lon", "lat")]),
                          y = as.matrix(data[,c(colLon, colLat)]), longlat = TRUE)
  minDistancesValue <- apply(allDistances, 2, min)

  return(minDistancesValue)
}
