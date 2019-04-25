#' Wrapper function peruvianGrid.assigner
#'
#' @name marsdenSquare.assigner
#' @rdname peruvianGrid.assigner
#' @export
#'
#' @examples
#' ## marsdenSquare.assigner
#' marsdenSquare.assigner(dataPoints = c(-84.32, -11.87))
marsdenSquare.assigner <- function(dataPoints, colLon = NULL, colLat = NULL){
  output <- peruvianGrid.assigner(dataPoints = dataPoints, colLon = colLon, colLat = colLat, what = "marsden")

  return(output)
}
