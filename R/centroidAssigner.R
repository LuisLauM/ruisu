#' @name centroidAssigner
#' @aliases centroidAssigner
#' @title Returns centroid values from Isoparalitoral-area codes.
#'
#' @description This function takes a vector of AIP codes and returns centroids (center of mass) in lon-lat values.
#'
#' @usage centroidAssigner(isoCode, old = TRUE)
#'
#' @param isoCode Vector with AIP codes.
#' @param old \code{logical}. Specifying whether to use old AIP shape (\code{AIPShapefile_old}) o the new (\code{AIPShapefile_new}).
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

  index <- match(isoCode, isoAreas$code)
  output <- data.frame(isoCode, isoAreas[index, c("x", "y")])
  colnames(output) <- c("area", "lon", "lat")
  rownames(output) <- seq(nrow(output))

  return(output)
}
