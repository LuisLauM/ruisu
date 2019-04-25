#' @title Simulate points for a survey track
#'
#' @description This function simulates the track points of a typical acoustic survey.
#'
#' @param breakPoints Arguments for changing the position of change for track way. See Details.
#' @param dotDensity Density of points for tracks in dots-per-nm.
#' @param dcLength Distance from coast in nm (nautical miles) of tracks.
#' @param legLength Length of tracks in nm.
#'
#' @details \code{breakpoints} must be a list of 3 levels:
#' \itemize{
#' \item \strong{points}, which indicates the latitude limits for track sections.
#' \item \strong{angle}, indicating the angle of tracks.
#' \item \strong{n_tracks}, how many tracks by section do you want?
#' }
#'
#' @return A \code{data.frame} with 4 columns: n, lon, lat and index.
#' @export
#'
#' @examples
#' breakPoints <- list(points = c(-4, -6, -15.4, -18.3),
#'                     angle = c(180, 210, 240),
#'                     n_tracks = c(9, 39, 18))
#' dotDensity  <- 0.1
#' dcLength    <- 5
#' legLength   <- 100
#'
#' output <- surveyTrack(breakPoints = breakPoints, dotDensity = dotDensity,
#'                       dcLength = dcLength, legLength = legLength)
#'
#' plot(output$lon, output$lat, pch = 19, cex = 0.1)
surveyTrack <- function(breakPoints, dotDensity, dcLength, legLength){

  # Define projection
  polygonProj <- CRS("+proj=longlat +datum=WGS84")

  # Subset data
  coastBuffer <- coastline[coastline$country == "Peru", c("lon", "lat")]

  # Build a buffer around each point
  coordinates(coastBuffer) <- ~lon + lat
  proj4string(coastBuffer) <- polygonProj

  coastBuffer <- SpatialPoints(coastBuffer)
  coastBuffer <- gBuffer(coastBuffer, width = dcLength*1/60)

  # Extract coordinate values and remove in-land points
  coastBuffer <- as.data.frame(coastBuffer@polygons[[1]]@Polygons[[1]]@coords)
  index <- isopArea.assigner(dataPoints = coastBuffer, colLon = "x", colLat = "y")
  coastBuffer <- coastBuffer[!is.na(index),]

  # Sort points and calculate the distances from buffer points to breakpoints
  coastBuffer <- coastBuffer[nrow(coastBuffer):1,]
  minDistance <- sapply(breakPoints$points, function(x, y) which.min(abs(x - y)), y = coastBuffer$y)

  # Build tracks by breakpoints' sections
  output <- NULL
  counter <- 1
  for(i in seq_along(breakPoints$angle)){
    # Index break points section
    index <- breakPoints$points[c(i, i + 1)]
    startPos <- seq(from = index[1], to = index[2], length.out = breakPoints$n_tracks[i])
    startPos <- startPos[seq(length(startPos) - ifelse(i != length(breakPoints$angle), 1, 0))]

    # Set initial points for tracks
    startPos <- approx(x = coastBuffer$y, y = coastBuffer$x, xout = startPos)
    startPos <- list(x = startPos$y, y = startPos$x)

    # Set final points for tracks
    finalPos <- list(x = legLength*round(cos(breakPoints$angle[i]/180*pi), 3)/60 + startPos$x,
                     y = legLength*round(sin(breakPoints$angle[i]/180*pi), 3)/60 + startPos$y)

    # Create points for tracks
    for(j in seq_along(startPos$x)){
      newPoints <- cbind(seq(from = startPos$x[j], to = finalPos$x[j], length.out = legLength/dotDensity),
                         seq(from = startPos$y[j], to = finalPos$y[j], length.out = legLength/dotDensity),
                         rep(x = counter, times = legLength/dotDensity))

      counter <- counter + 1

      output <- rbind(output, newPoints)
    }
  }

  # Convert to data frame
  output <- as.data.frame(output, stringsAsFactors = FALSE)
  colnames(output) <- c("lon", "lat",  "index")

  # Remove out of limits points
  output <- output[output$lat >= min(breakPoints$points) & output$lat <= max(breakPoints$points),]

  # Build intertrack points
  trackList <- unique(output$index)
  finalOutput <- NULL
  counter <- 1
  for(i in seq(max(trackList) - 1)){

    # Subset points of tracks
    tempData1 <- output[output$index == trackList[i],]
    tempData2 <- output[output$index == trackList[i + 1],]

    # Interchange origins of intertracks
    index1 <- ifelse(trackList[i] %% 2 == 0, 1, nrow(tempData1))
    index2 <- ifelse(trackList[i] %% 2 == 0, 1, nrow(tempData2))

    # Define limits of intertracks
    xValues <- c(tempData1$lon[index1], tempData2$lon[index2])
    yValues <- c(tempData1$lat[index1], tempData2$lat[index2])

    # Calculate distance of intertrack in mn
    legLength <- sqrt(diff(xValues)^2 + diff(yValues)^2)*60

    # Create intertrack points
    lonValues <- seq(xValues[1], xValues[2], length.out = legLength/dotDensity)
    latValues <- seq(yValues[1], yValues[2], length.out = legLength/dotDensity)

    # Compile on a data frame
    interPoints <- data.frame(lon = lonValues, lat = latValues, stringsAsFactors = FALSE)

    # Compile final data
    finalOutput <- rbind(finalOutput,
                         cbind(as.matrix(tempData1[,c("lon", "lat")]), rep(counter, nrow(tempData1))),
                         cbind(as.matrix(interPoints), rep(counter + 1, nrow(interPoints))))

    counter <- counter + 2
  }

  # Complete final data
  finalOutput <- rbind(finalOutput, cbind(as.matrix(tempData2[,c("lon", "lat")]), rep(counter, nrow(tempData2))))

  # Final changes
  finalOutput <- as.data.frame(cbind(seq(nrow(finalOutput)), finalOutput), stringsAsFactors = FALSE)
  colnames(finalOutput) <- c("n", "lon", "lat", "track_index")

  return(finalOutput)
}
