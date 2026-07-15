#' @title Select Points Inside an Interactive Polygon
#'
#' @description
#' Interactively draws a polygon on a scatter plot and returns a binary
#' vector indicating whether each point lies inside the selected polygon.
#'
#' @param x A \code{data.frame} containing the point coordinates.
#' @param n_polygon Integer. Number of vertices to use when drawing the
#'   polygon. Passed to \code{\link[graphics]{locator}}.
#' @param coords_x A character vector of length two giving the names of the
#'   columns in \code{x} containing the x- and y-coordinates, respectively.
#'   Defaults to \code{c("x", "y")}.
#' @param crs Coordinate reference system of the coordinates, given as an
#'   EPSG code or any value accepted by \code{\link[sf:st_crs]{sf::st_crs}}.
#'   Defaults to \code{4326} (WGS84).
#' @param ... Additional graphical arguments passed to
#'   \code{\link[graphics]{plot}}.
#'
#' @details
#' After plotting the points, the user is prompted to click
#' \code{n_polygon} vertices to define a polygon. The polygon is closed
#' automatically, and the function uses spatial intersections from the
#' \pkg{sf} package to determine which points are located inside it.
#'
#' @return
#' An integer vector of length \code{nrow(x)} with values:
#' \describe{
#'   \item{0}{Point lies outside the polygon.}
#'   \item{1}{Point lies inside the polygon.}
#' }
#'
#' @seealso
#' \code{\link[graphics]{locator}},
#' \code{\link[sf:st_intersects]{sf::st_intersects}}
#'
#' @examples
#' \dontrun{
#' # Define number op points
#' n <- 50
#'
#' # Create a table with coordinates for n points
#' sampleData <- data.frame(
#'   x = runif(n = 50, min = 0.5, max = 1.5),
#'   y = runif(n = 50, min = 0.5, max = 1.5)
#' )
#'
#' # Plot points and allow the user to draw the polygon (interactive)
#' index <- selectPtsByPolygon(x = sampleData, n_polygon = 8, asp = 1)
#'
#' # Plot polygon and points classified by its status of out or within the
#' # polygon
#' plot(
#'   x = sampleData$x, y = sampleData$y,
#'   col = ifelse(index == 0, "black", "blue"),
#'   asp = 1
#' )
#' }
selectPtsByPolygon <- function(x, n_polygon, coords_x = c("x", "y"), crs = 4326, ...){

  plot(x = x[,coords_x[1]], y = x[,coords_x[2]], pch = 4,
       xlab = coords_x[1], ylab = coords_x[2], ...)

  selectPoly <- locator(n = n_polygon, type = "o", pch = 16, col = "blue") |>

    as.data.frame()
  selectPoly$id <- 1

  index <- c(1, length(selectPoly$x))
  lines(x = selectPoly$x[index], y = selectPoly$y[index])

  selectPoly <- selectPoly |>

    st_as_sf(coords = c("x", "y"), crs = crs) |>

    group_by(id) |>

    summarise(geometry = st_combine(geometry)) |>

    st_cast(to = "POLYGON")

  out <- rep(x = 0, times = nrow(x))
  index <- st_as_sf(x = x, coords = coords_x, crs = crs) |>

    st_intersects(x = selectPoly) |> unlist()

  out[index] <- 1

  out
}
