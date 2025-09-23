#' @title Function to obtain an index of points inside and outside a polygon
#'
#' @param x \code{data.frame} with coordinates to be classified.
#' @param n_polygon Number of points to define polygon.
#' @param coords_x Names of coordinates on \code{x}.
#' @param crs Coordinate reference system
#' @param ... Extra arguments passed to the main plot.
#'
#' @returns A numeric vector: out of (\code{0}) or within (\code{1}) the polygon.
#'
#' @examples
#' n <- 50
#' sampleData <- data.frame(x = runif(n = 50, min = 0.5, max = 1.5),
#'                          y = runif(n = 50, min = 0.5, max = 1.5))
#'
#' index <- selectPtsByPolygon(x = sampleData, n_polygon = 8, asp = 1)
#'
#' plot(x = sampleData$x, y = sampleData$y,
#'      col = ifelse(index == 0, "black", "blue"), asp = 1)
selectPtsByPolygon <- function(x, n_polygon, coords_x = c("x", "y"), crs = 4326, ...){

  plot(x = x[,coords_x[1]], y = x[,coords_x[2]], pch = 4,
       xlab = coords_x[1], ylab = coords_x[2], ...)

  selectPoly <- locator(n = n_polygon, type = "o", pch = 16, col = "blue") |> as.data.frame()
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
