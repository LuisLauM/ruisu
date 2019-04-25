#' @title Determine the proximity to coast of points
#'
#' @param dataPoints \code{data.frame} which has Longitude and Latitude information.
#' @param colLon Name of column which contains Longitude info.
#' @param colLat Name of column which contains Latitude info.
#' @param units Which units do you want to use for measuring the proximity to coast.
#' @param distance Maximum distance (in degrees) to make the searching.
#'
#' @return A \code{logical} vector indicating whether the coordinates belong to the area between
#' the coast line and a buffer of the selected distance.
#' @export
#'
#' @examples
#' isNearCoast(c(-81.191, -5.211), distance = 20)
#' isNearCoast(c(-81.191, -5.211), distance = 10)
isNearCoast <- function(dataPoints, colLon = "lon", colLat = "lat", units = "m", distance = 20){

  # Check values for aguments
  if(tolower(units) == "m"){
    posibleValues <- c(10, 20, 30, 50, 100, 150, 200, 300)
  }else if(tolower(units) == "nm"){
    posibleValues <- seq(0.2, 2, 0.2)
  }else{
    stop("'units' must be m (meters) or nm (nautical miles).")
  }

  if(length(distance) != 1 || !is.numeric(an(distance)) || !is.element(an(distance), posibleValues)){
    stop("'distance' must be numeric, length 1 and values ", paste(posibleValues, collapse = ", "), ".")
  }

  # Select the reference shapefile
  referenceShapefile <- get(ifelse(test = tolower(units) == "m", yes = "coastlineBuffer_m", no = "coastlineBuffer_nm"))

  # If the input data is a vector, rearrange in a data.frame
  dataPoints <- switch(class(dataPoints),
                       "data.frame" = as.data.frame(dataPoints[,c(colLon, colLat)]),
                       "numeric" = data.frame(lon = dataPoints[1], lat = dataPoints[2], stringsAsFactors = FALSE),
                       sprintf("%s class is not a valid class. Please, just includes data.frame or numeric vector object.",
                               class(dataPoints)))

  # Select just points with valid values for both lon and lat
  index <- complete.cases(dataPoints)
  dataPoints <- dataPoints[index,]

  # Define projection
  coordinates(dataPoints) <- dataPoints
  proj4string(dataPoints) <- proj4string(referenceShapefile)

  # Make the intersection
  dataPoints <- over(x = dataPoints, y = referenceShapefile)

  # Build the output vector (logical)
  output <- rep(FALSE, nrow(dataPoints))
  output[index & dataPoints$distance <= distance] <- TRUE

  return(output)
}
