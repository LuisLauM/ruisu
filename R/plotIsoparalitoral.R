#' @title Plot Isoparalitoral shapes
#'
#' @param codeList AIP codes to plot.
#' @param add logical flag that specifies whether to add to the current plot. If FALSE, a new plot is begun, using
#' coordinates for Peruvian shore.
#' @param old \code{logical}. Specifying whether to use old AIP shape (\code{AIPShapefile_old}) o the new (\code{AIPShapefile_new}).
#' @param plotParams Extra parameters passed to \code{\link{plot}} function. This argument is ignored if \code{add = TRUE}.
#' @param mapParams Extra parameters passed to \code{\link{map}} function. This argument is ignored if \code{add = TRUE}.
#' @param ... Extra parameters passed to \code{plot.SpatialPolygons} function.
#'
#' @details \code{plot.SpatialPolygons} is an internal function of \code{\link{sp}}. Some important parameters are: \code{border} which
#' allows to specify color (or colors if a vector) of each AIP. \code{col} is useful to modify fill color of AIP.
#'
#' @export
plotIsoparalitoral <- function(codeList = NULL, add = FALSE, old = TRUE,
                               plotParams = list(xlim = c(-90, -70), ylim = c(-18, -2), axes = TRUE, xlab = NA, ylab = NA),
                               mapParams = list(database = "world"), ...){

  # Select shape
  referenceShapefile <- get(ifelse(test = isTRUE(old), yes = "AIPShapefile_old", no = "AIPShapefile_new"))

  if(!isTRUE(add)){
    # Make plot
    do.call(what = "plot", args = c(list(x = 1, y = 1), plotParams))

    # Make coast line
    do.call(what = "map", args = c(list(add = TRUE), mapParams))
  }


  if(is.null(codeList)){
    codeList <- referenceShapefile$code
  }

  index <- match(codeList, referenceShapefile$code)

  if(sum(is.na(index)) == length(index)){
    stop("None of values in 'codeList' is a valid AIP code.")
  }

  codeList <- index

  plot(referenceShapefile[codeList, 1], add = TRUE, ...)

  return(invisible())
}
