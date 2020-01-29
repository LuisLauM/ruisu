#' Add labels for XY axis on a map
#'
#' @param xParams Parameters for X axis.
#' @param yParams Parameters for Y axis.
#' @param where \code{character} vector indicating where to show the labels for axis: 1 (bottom), 2 (left),
#' 3 (top) or 4 (right).
#' @param las \code{numeric} in {0,1,2,3}; the style of axis labels. See \link{par}.
#' @param labels \code{character} vector setting the labels for axis.
#' @param ... Extra arguments passed to \link{axis} function (e.g. \code{cex.axis}), which is used internaly for ploting.
#' @param dms A \code{character} strings indicating what to show (d: degrees,
#' m: minutes, s: seconds).
#'
#' @details \code{xParams} and \code{yParams} must contain axis information as a 3 or 4 length vector.
##' \itemize{
##'  \item{"length 3:"}{to indicate from 100 S to 70 S by 5, the vector will be c(-100, -70, 5).}
##'  \item{"length 4:"}{to indicate from 100 S to 70 S by 5 and draw a second set of ticks from 100 S to 70 S by 1, the
##'  vector will be c(-100, -70, 1, 5).}
##' }
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

  if(is.null(xParams) && is.null(yParams)) stop("You must indicate at least one xParams or yParams.")

  for(i in c("xParams", "yParams")){
    if(is.null(get(i))) next

    msg <- sprintf("'%s' must be a numeric vector of length 3 or 4. See ?addCoordsAxes", i)
    if(!is.numeric(get(i)) || !is.element(length(get(i)), c(3, 4))) stop(msg)
  }

  if(length(where) > 4 | length(where) < 1 | any(!is.element(where, 1:4))){
    stop("'where' must be an integer between 1 and 4.")
  }

  for(i in seq_along(where)){
    if(is.element(where[i], c(1, 3))){
      if(isTRUE(all.equal(length(xParams), 4))){
        xCoords <- seq(from = xParams[1], to = xParams[2], by = xParams[3])
        axis(side = where[i], at = xCoords, labels = NA, tcl = -0.25)

        xParams[3] <- xParams[4]
      }

      xCoords <- seq(from = xParams[1], to = xParams[2], by = xParams[3])
      tempLabs <- if(is.null(labels)) getCoordsAxes(coord = xCoords, what = "lon", dms = dms) else labels
      axis(side = where[i], at = xCoords, labels = tempLabs, las = las, ...)
    }else{
      if(isTRUE(all.equal(length(yParams), 4))){
        yCoords <- seq(from = yParams[1], to = yParams[2], by = yParams[3])
        axis(side = where[i], at = yCoords, labels = NA, tcl = -0.25)

        yParams[3] <- yParams[4]
      }

      yCoords <- seq(from = yParams[1], to = yParams[2], by = yParams[3])
      tempLabs <- if(is.null(labels)) getCoordsAxes(coord = yCoords, what = "lat", dms = dms) else labels
      axis(side = where[i], at = yCoords, labels = tempLabs, las = las, ...)
    }
  }

  return(invisible())
}
