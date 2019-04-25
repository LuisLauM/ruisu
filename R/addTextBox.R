#' @title Add a box with a text inside
#'
#' @param xLimits x-Axis limits for box.
#' @param yLimits y-Axis limits for box.
#' @param text A character or \link{expression} vector specifying the text to be written.
#' @param border the color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders. See Details.
#' @param col The color for filling the box polygon.
#' @param lty The line type to be used, as in par.
#' @param ... Extra arguments passed from \link{text} function.
#'
#' @details For extra details about \code{border}, \code{col} and \code{lty}, check the description of \link{polygon}.
#'
#' @export
#'
#' @examples
#' plot(1, 1, pch = NA)
#' addTextBox(xLimits = c(0.9, 1.1), yLimits = c(0.9, 1.1),
#'            text = "Hello World!", col = "indianred1", font = 2)
addTextBox <- function(xLimits, yLimits, text, border = NULL, col = "white", lty = par("lty"), ...){

  polygon(x = c(xLimits, rev(xLimits)), y = rep(yLimits, each = 2), border = border, col = col)
  text(x = mean(xLimits), y = mean(yLimits), labels = text, ...)

  return(invisible())
}
