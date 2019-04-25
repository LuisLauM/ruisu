#' @title Plot typical wind arrows from wind velocity data
#'
#' @param uComponent Matrix with values of velocity in x component See Details.
#' @param vComponent Matrix with values of velocity in y component See Details.
#' @param maxLength For changing length of arrows (\code{numeric}).
#' @param densityfactor For changing the quantity of arrows on plot. Value from 0 to 1.
#' @param arrowCol Color of arrows.
#' @param add \code{logical}; if \code{TRUE}, add to current plot and \code{...} will not be considered.
#' @param includeRaster If \code{TRUE}, the final plot will show a raster plot on the background.
#' @param xInterval If \code{add = TRUE}, it works for specifing the interval of marks in X axis.
#' @param yInterval If \code{add = TRUE}, it works for specifing the interval of marks in Y axis.
#' @param col Color table to use for image.
#' @param ... Extra arguments passed to \code{image} function. It will only be used if \code{add = FALSE}.
#'
#' @details Both \code{uComponent} and \code{vComponent} must be list objects with levels x (vector of longitudes),
#' y (vector of latitudes) and z (matrix of values).
#'
#' @return A plot of arrows indicating the intensity and direction of winds.
#' @export
makeWindPlot <- function(uComponent, vComponent, maxLength = 1, densityfactor = 0.98, arrowCol = "black",
                         add = TRUE, includeRaster = TRUE, col = tim.colors(1e3),
                         xInterval = NULL, yInterval = NULL, ...){

  intensityMatrix <- list(x = uComponent$x,
                          y = uComponent$y,
                          z = sqrt(uComponent$z^2 + vComponent$z^2))

  angleMatrix <- list(x = uComponent$x,
                      y = uComponent$y,
                      z = atan(vComponent$z/uComponent$z))

  if(is.null(list(...)$xlim)){
    if(is.list(intensityMatrix)){
      xlim <- range(intensityMatrix$x)
    }else if(is.list(angleMatrix)){
      xlim <- range(angleMatrix$x)
    }else{
      xlim <- c(0, 1)
    }
  }

  if(is.null(list(...)$ylim)){
    if(is.list(intensityMatrix)){
      ylim <- range(intensityMatrix$y)
    }else if(is.list(angleMatrix)){
      ylim <- range(angleMatrix$y)
    }else{
      ylim <- c(0, 1)
    }
  }

  if(is.null(list(...)$zlim)){
    zlim <- c(0, max(an(intensityMatrix$z), na.rm = TRUE))
  }

  if(is.matrix(intensityMatrix)){
    intensityMatrix <- list(x = seq(xlim[1], xlim[2], length.out = nrow(intensityMatrix)),
                            y = seq(ylim[1], ylim[2], length.out = ncol(intensityMatrix)),
                            z = intensityMatrix)
  }

  if(is.matrix(angleMatrix)){
    angleMatrix <- list(x = seq(xlim[1], xlim[2], length.out = nrow(angleMatrix)),
                        y = seq(ylim[1], ylim[2], length.out = ncol(angleMatrix)),
                        z = angleMatrix)
  }

  if(!isTRUE(add)){
    plot(1, 1, pch = NA, axes = FALSE, xlim = xlim, ylim = ylim, xlab = NA, ylab = NA)
  }

  if(isTRUE(includeRaster)){
    image(intensityMatrix, add = TRUE, col = col, ...)
  }

  densityfactor <- floor((nrow(intensityMatrix$z) - 1)*(1 - densityfactor) + 1)

  for(i in seq(from = 1, to = nrow(intensityMatrix$z), by = densityfactor)){
    for(j in seq(from = 1, to = ncol(intensityMatrix$z), by = densityfactor)){

      intensityValue <- intensityMatrix$z[i, j]
      angleValue <- angleMatrix$z[i, j]

      if(!is.na(intensityValue) || !is.na(angleValue)){
        arrows(x0 = intensityMatrix$x[i], y0 = intensityMatrix$y[j],
               x1 = intensityMatrix$x[i] + sin(angleValue)*intensityMatrix$z[i, j]*maxLength,
               y1 = intensityMatrix$y[j] + cos(angleValue)*intensityMatrix$z[i, j]*maxLength,
               length = 0.05, angle = 30, col = arrowCol)
      }
    }
  }

  xInterval <- if(is.null(xInterval) || is.na(xInterval)) diff(xlim)/5 else xInterval
  yInterval <- if(is.null(yInterval) || is.na(yInterval)) diff(ylim)/5 else yInterval

  if(!isTRUE(add)){
    xAxis <- seq(xlim[1], xlim[2], xInterval)
    yAxis <- seq(ylim[1], ylim[2], yInterval)

    axis(side = 1, at = xAxis, labels = getCoordsAxes(xAxis, "lon"))
    axis(side = 2, at = yAxis, labels = getCoordsAxes(yAxis, "lat"), las = 2)
    box()
  }

  if(isTRUE(includeRaster)){
    image.plot(intensityMatrix, add = TRUE, col = col, legend.only = TRUE, ...)
  }


  return(invisible())
}
