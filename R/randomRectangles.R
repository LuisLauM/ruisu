#' @title Draw colorful squares
#'
#' @param nsquares \code{numeric} How much squares do you desire? (500, as default).
#' @param borders \code{logical} Do you want yo show borders around rectangles?
#' @param colPalette \code{character} Vector of colors that will be used for plotting.
#'
#' @export
#'
#' @examples
#' randomRectangles(nsquares = 120)
randomRectangles <- function(nsquares = 500, borders = TRUE, colPalette = rainbow(n = 2e3)){
  dev.new()

  allCols <- sample(x = colPalette, size = nsquares, replace = TRUE)

  squareData <- data.frame(center_x = runif(n = nsquares),
                           center_y = runif(n = nsquares),
                           size_x   = rbeta(n = nsquares, shape1 = 1, shape2 = 50),
                           size_y   = rbeta(n = nsquares, shape1 = 1, shape2 = 50),
                           color    = allCols,
                           border   = if(isTRUE(borders)) rev(allCols) else NA,
                           stringsAsFactors = FALSE)

  par(bg = "black", mar = rep(0, 4))
  plot(1, 1, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0, 1), ylim = c(0, 1))

  for(i in seq(nsquares)){
    with(squareData, rect(xleft = center_x[i] - size_x[i],
                          ybottom = center_y[i] - size_y[i],
                          xright = center_x[i] + size_x[i],
                          ytop = center_y[i] + size_y[i],
                          col = color[i], border = border[i], lwd = 1.5))
  }

  return(invisible())
}
