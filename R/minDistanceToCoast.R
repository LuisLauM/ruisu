
# Main fx -----------------------------------------------------------------

#' @title Get the minumim distance to coast
#'
#' @param data \code{data.frame} with coords that will be used to calculate min distance to coast line.
#' @param colLon Name or position of column for longitude. As default, it will be \code{lon}.
#' @param colLat Name or position of column for latitude. As default, it will be \code{lat}.
#' @param countryFilter Select the country for make comparation
#' @param unit Define the unit for outputs: nm (nautical miles), kilometers (km), m (meters).
#' @param multicore \code{logical} indicating whether to use multiple cores for calculate distances.
#' See Details.
#' @param ncores If \code{multicore = TRUE}, how many cores are you going to use?
#' @param out \code{character} indicating what products are going to be returned: \code{value} or
#' \code{position}. By default, it will return both as a list.
#'
#' @details This function uses internaly both \link{spDists} and \link{spDistsN1}, with argument
#' \code{longlat = TRUE}.
#'
#' If \code{data} is a matrix, \code{colLon} and \code{colLat} must be ONLY numeric values.
#'
#' It is recommended to use multicore mode only when the database exceeds 2000 rows, less than
#' this, single core mode have proved to be faster.
#'
#' \code{out} argument allows to users to decide having between: the values (the values of minimum distance
#' to coast), the positions (where this minimum distance where achieved in coast) or both.
#'
#' @return It returns a list with 2 levels: \code{value}, The minimum distance to coast line and
#' \code{position}, the point where this minimum distance is reached.
#'
#' @export
#'
#' @examples
#'
#' n <- 100
#'
#' allData <- data.frame(lon = runif(n, -80, -70), lat = runif(n, -18, -2),
#'                       stringsAsFactors = FALSE)
#'
#' allData <- allData[!is.na(isopArea.assigner(allData)),]
#'
#' minValues <- minDistanceToCoast(allData)
#'
#' xlim <- c(-85, -70)
#' ylim <- c(-20, -2)
#'
#' dev.new()
#' par(mar = c(2, 3, 1, 1), xaxs = "i", yaxs = "i")
#' plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)
#'
#' points(allData$lon, allData$lat, pch = 16, cex = 0.5, xlim = c(-85, -70), ylim = c(-20, -2))
#' lines(coastline$lon, coastline$lat)
#'
#'
#' for(i in 1:nrow(minValues$position)){
#'   lines(c(allData$lon[i], minValues$position$lon[i]), c(allData$lat[i], minValues$position$lat[i]),
#'         lty = "dotted", col = "red")
#' }
#'
#' axis(side = 1, at = seq(xlim[1], xlim[2], length.out = 4),
#'      labels = getCoordsAxes(seq(xlim[1], xlim[2], length.out = 4), "lon"))
#' axis(side = 2, at = seq(ylim[1], ylim[2], length.out = 10),
#'      labels = getCoordsAxes(seq(ylim[1], ylim[2], length.out = 10), "lat"), las = 2)
#' box()
minDistanceToCoast <- function(data, colLon = "lon", colLat = "lat", countryFilter = "peru", unit = "nm",
                               multicore = FALSE, ncores = 1, out = c("value", "position")){

  coastline <- ruisu::coastline

  # Check data
  if(all(!is.element(c("data.frame", "matrix"), class(data))) ||
     nrow(data) < 1 || (is.element("data.frame", class(data)) & any(!is.element(c(colLon, colLat), colnames(data)))) ||
     length(colLon) != 1 || length(colLat) != 1 ||
     sum(complete.cases(data[,c(colLon, colLat)])) < 1){
    stop("'data' must be a valid 'data.frame' or 'matrix' with numeric columns for lon/lat.")
  }

  data <- data.frame(lon = an(data[,colLon]),
                     lat = an(data[,colLat]))

  # Get index for filter reference points
  if(!is.null(countryFilter)){
    countryFilter <- chartr(old = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1",
                            new = "aeiouun", x = tolower(countryFilter))

    index <- is.element(tolower(coastline$country), tolower(countryFilter))
  }else{
    index <- range(data$lat) + c(-20, 20)
    index <- coastline$lat > index[1] & coastline$lat < index[2]
  }

  # Filter reference points
  refLines <- as.matrix(coastline[index, c("lon", "lat")])

  # Get coords from data
  validRows <- complete.cases(data) & data[,1] >= -180 & data[,1] <= 180 & data[,2] >= -90 & data[,2] <= 90

  # Create empty objects for outputs
  minDistancesValue <- rep(NA, nrow(data))
  minDistancesPosition <- as.data.frame(matrix(data = NA, nrow = nrow(data), ncol = 2,
                                               dimnames = list(seq(nrow(data)), c("lon", "lat"))))

  # Remove non valid rows
  data <- data[validRows,]

  # Get min distances using single or multithread processes
  if(isTRUE(multicore)){
    # Add an index for each row
    data$n <- seq(nrow(data))

    # Registering cluster
    cl <- makeCluster(ncores)
    registerDoParallel(cl)

    # Run multithread process
    allDistances <- foreach(i = seq(nrow(data)), .inorder = FALSE, .packages = "sp") %dopar% {
      getDistance(data, refLines, i)
    }

    # Finish cluster
    stopCluster(cl)

    # Sort outputs
    index <- order(sapply(allDistances, "[[", 3))
    allDistances <- allDistances[index]

    # Get distances and positions
    minDistancesValue[validRows] <- sapply(allDistances, "[[", 1)
    minDistancesPosition[validRows,] <- t(sapply(allDistances, "[[", 2))
  }else{
    # Get min distances
    allDistances <- spDists(x = refLines,
                            y = as.matrix(data),
                            longlat = TRUE)

    # Get distances and positions
    minDistancesValue[validRows] <- apply(allDistances, 2, min)

    index <- apply(allDistances, 2, which.min)
    minDistancesPosition[validRows,] <- as.matrix(refLines[index,, drop = FALSE])
  }

  dimnames(minDistancesPosition) <- list(seq(nrow(minDistancesPosition)), c("lon", "lat"))

  # Get a factor using unit argument
  unitFactor <- switch(tolower(unit),
                       nm = 1/1.852,
                       km = 1,
                       m = 1e-3)

  # Return a list with results
  output <- list(value = minDistancesValue*unitFactor,
                 position = minDistancesPosition)[tolower(out)]

  return(if(length(output) == 1) output[[1]] else output)
}


# Auxiliar fx -------------------------------------------------------------

getDistance <- function(data, refLines, i){

  allDistances <- spDistsN1(pts = refLines,
                            pt = as.numeric(data[i, 1:2]),
                            longlat = TRUE)

  index <- which.min(allDistances)

  output <- list(value = allDistances[index],
                 posValue = as.numeric(refLines[index,]),
                 nrow = data$n[i])

  return(output)
}

