#' Wrapper function peruvianGrid.assigner
#'
#' @name isopArea.assigner
#' @rdname peruvianGrid.assigner
#' @export
#'
#' @examples
#' ## isopArea.assigner
#' exampleCoords <- data.frame(lon = runif(n = 10, min = -80, max = -78),
#'                             lat = runif(n = 10, min = -14, max = -12))
#'
#' isopArea.assigner(dataPoints = exampleCoords)
isopArea.assigner <- function(dataPoints, colLon = NULL, colLat = NULL, old = FALSE){
  output <- peruvianGrid.assigner(dataPoints = dataPoints, colLon = colLon, colLat = colLat, what = "isoparalitoral", old = old)

  return(output)
}
