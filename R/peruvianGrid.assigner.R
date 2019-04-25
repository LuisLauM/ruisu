
# Main fx -----------------------------------------------------------------

#' Function to get main grid codes used at IMARPE from lon-lat information.
#'
#' @param dataPoints \code{data.frame} which has Longitude and Latitude information.
#' @param colLon Name of column which contains Longitude info.
#' @param colLat Name of column which contains Latitude info. See Details for default values.
#' @param what \code{character} string indicating the grid (i.e. the method) that will be used for the matching.
#' @param old Do you prefer to use the old version of AIP (with errors, but historicaly used) or a new one
#' obtained by applying a buffer over a coastline.
#' @param ... Extra arguments passed to the selected method.
#'
#' @details If \code{colLon} or \code{colLat} were \code{NULL}, the default values will depend on the class of
#' \code{dataPoints}. So, if it was a \code{matrix} or a \code{numeric} vector, \code{colLon} and \code{colLat}
#' will take values 1 or 2, respectively. Otherwise, if \code{dataPoints} is a \code{data.frame}, the values will
#' be "lon" and "lat", respectively.
#'
#' @return A \code{character} vector with the code for ,depending on 'what', the assigned Marsden/Isoparalitoral grid.
#' @rdname peruvianGrid.assigner
#' @export
peruvianGrid.assigner <- function(dataPoints, colLon = NULL, colLat = NULL, what = "isoparalitoral", ...){
  if(is.null(colLon)){
    colLon <- switch(class(dataPoints),
                     "matrix" = 1,
                     "data.frame" = "lon",
                     "numeric" = 1)
  }

  if(is.null(colLat)){
    colLat <- switch(class(dataPoints),
                     "matrix" = 2,
                     "data.frame" = "lat",
                     "numeric" = 2)
  }

  dataPoints <- switch(class(dataPoints),
                       "matrix" = dataPoints[,c(colLon, colLat)],
                       "data.frame" = data.frame(lon = dataPoints[,colLon], lat = dataPoints[,colLat], stringsAsFactors = FALSE),
                       "numeric" = data.frame(lon = dataPoints[colLon], lat = dataPoints[colLat], stringsAsFactors = FALSE))

  output <- rep(NA, nrow(dataPoints))

  index <- complete.cases(dataPoints)
  dataPoints <- dataPoints[index,]

  output[index] <- switch(what,
                          isoparalitoral = assigner_isoparalitoral(dataPoints = dataPoints, ...),
                          marsden = assigner_marsdenSquare(dataPoints = dataPoints, ...),
                          "Incorrect value for 'what', check available methods in help.")

  return(output)
}


# Auxiliar fx -------------------------------------------------------------

assigner_isoparalitoral <- function(dataPoints, old = FALSE){
  referenceShapefile <- get(ifelse(test = isTRUE(old), yes = "AIPShapefile_old", no = "AIPShapefile_new"))

  coordinates(dataPoints) <- dataPoints

  proj4string(dataPoints) <- proj4string(referenceShapefile)

  dataPoints <- over(x = dataPoints, y = referenceShapefile)

  return(dataPoints$code)
}

assigner_marsdenSquare <- function(dataPoints){
  output <- paste0(ac(cut(x = abs(dataPoints$lat), breaks = seq(3, 20), labels = LETTERS[seq(17)])),
                   anc(cut(x = abs(dataPoints$lon), breaks = seq(70, 180), labels = seq(110))))

  return(output)
}
