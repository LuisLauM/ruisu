#' Add labels for XY axis on a map
#'
#' @param xParams Parameters for X axis.
#' @param yParams Parameters for Y axis.
#' @param where \code{character} vector indicating where to show the labels for axis: 1 (bottom), 2 (left),
#' 3 (top) or 4 (right).
#' @param las \code{numeric} in {0,1,2,3}; the style of axis labels. See \link{par}.
#' @param labels \code{character} vector setting the labels for axis.
#' @param ... Extra arguments passed to \link{axis} function (e.g. \code{cex.axis}), which is used internaly for ploting.
#'
#' @details \code{xParams} and \code{yParams} must contain axis information as a 3-length vector:
#' \code{c(from, to, by)}. For instance, to indicate from -100 S to -70 S by 5, the vector will be c(-100, -70, 5).
#'
#' If \code{labels} is specified at \code{...}, the function will consider this values for both \code{xParams} and
#' \code{yParams}, so be carefull and use it separately (run the function twice for lon and lat).
#'
#' @export
#'
#' @examples
#' par(mar = c(2, 3, 1, 3), xaxs = "i", yaxs = "i")
#'
#' xlim <- c(-85, -70)
#' ylim <- c(-20, -2)
#'
#' plot(1, 1, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)
#'
#' addCoordsAxes(xParams = c(xlim, 5), yParams = c(ylim, 2), where = c(1, 2, 4))
#'
#' box()
addCoordsAxes <- function(xParams = NULL, yParams = NULL, where = c(1, 2), las = 1, labels = NULL,
                          dms = "d", ...){

  xParams <- if(is.null(xParams)) c(-180, 180, 5) else c(sort(xParams[1:2]), xParams[3])
  yParams <- if(is.null(yParams)) c(-90, 90, 5) else c(sort(yParams[1:2]), yParams[3])

  for(i in c("xParams", "yParams")){
    msg <- sprintf("'%s' must be a numeric vector of length 3. See ?addCoordsAxes", i)
    if(!is.numeric(get(i)) || length(get(i)) != 3) stop(msg)
  }

  if(length(where) > 4 | length(where) < 1 | any(!is.element(where, 1:4))){
    stop("'where' must be an integer between 1 and 4.")
  }

  for(i in seq_along(where)){
    if(is.element(where[i], c(1, 3))){
      xCoords <- seq(from = xParams[1], to = xParams[2], by = xParams[3])
      tempLabs <- if(is.null(labels)) getCoordsAxes(coord = xCoords, what = "lon", dms = dms) else labels
      axis(side = where[i], at = xCoords, labels = tempLabs, las = las, ...)
    }else{
      yCoords <- seq(from = yParams[1], to = yParams[2], by = yParams[3])
      tempLabs <- if(is.null(labels)) getCoordsAxes(coord = yCoords, what = "lat", dms = dms) else labels
      axis(side = where[i], at = yCoords, labels = tempLabs, las = las, ...)
    }
  }

  return(invisible())
}
