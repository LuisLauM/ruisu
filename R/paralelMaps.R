#' Function for pltting Parallel maps
#'
#' @param file Indicates the table from the main length distribution will be drawn. See details.
#' @param colCategories Name or number of columns which will be used for make categorization.
#' @param allCategories Vector for categories (optional). It must contain all values on 'colCategories'.
#' @param categoriesNames \code{character} vector indicating the names (subtitle) showed for each sub map.
#' @param colLon Name or position of column for longitude. As default, it will be \code{lon}.
#' @param colLat Name or position of column for latitude As default, it will be \code{lat}.
#' @param xlim Limits of X axis.
#' @param ylim Limits of Y axis.
#' @param yInterval Number of intervals for y axis labels.
#' @param xDelay Argument which controls separation between maps.
#' @param newPlot If \code{TRUE}, the plot will be opened in a new window (using \code{x11} command).
#' @param seaCol Color for sea background.
#' @param landCol Color for land.
#' @param showBoxText If \code{TRUE} (default) it allows to show the names of categories for each sub map.
#' @param textBoxLimits A \code{list} (two vector: x and y) with values for limiting text box.
#' @param textBox String with text included inside the text box.
#' @param textBox_line \code{line} (see \code{\link{mtext}}) parameter for text box.
#' @param textBox_cex \code{cex} (see \code{\link{mtext}}) parameter for text box.
#' @param textBox_font \code{font} (see \code{\link{mtext}}) parameter for text box.
#' @param textBoxXCF Correction Factor on X for text box.
#' @param catNames_col \code{col} (see \code{\link{mtext}}) parameter for Categories' text.
#' @param catNames_cex \code{cex} (see \code{\link{mtext}}) parameter for Categories' text.
#' @param catNames_font \code{font} (see \code{\link{mtext}}) parameter for Categories' text.
#' @param addHarbors \code{logical}. Add harbor text/points over the coastline?
#'
#' @return A plot with parallel maps.
#' @export
paralelMaps <- function(file, colCategories, allCategories = NULL, categoriesNames = NULL, colLon = "lon", colLat = "lat",
                        xlim = NULL, ylim = NULL, yInterval = NULL, xDelay = NULL,
                        newPlot = FALSE, seaCol = "lightblue1", landCol = "lightgoldenrod1", showBoxText = TRUE,
                        textBoxLimits = NULL, textBox = NULL, textBox_line = -2, textBox_cex = 1.2, textBox_font = 2,
                        textBoxXCF = 0, catNames_col = "black", catNames_cex = 1, catNames_font = 2,
                        addHarbors = TRUE){

  # Preserve par values before make graphics
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  # Read file
  if(is.element("data.frame", class(file))){
    allData <- file
  }else if(length(file) == 1 && is.character(file) && file.exists(file)){
    allData <- read.csv(file = file, check.names = FALSE, stringsAsFactors = FALSE)
  }else{
    stop("Incorrect value for file. It must be a 'data.frame' or it must indicate the path of input data.")
  }

  # Check values for colLon, colLat and colCategories
  if(any(!is.element(c(colLon, colLat, colCategories), colnames(allData)))){
    stop("Incorrect value for 'colLon', 'colLat' or 'colCategories'.")
  }

  # Get lon/lat and category columns
  allData$lon <- an(allData[,colLon])
  allData$lat <- an(allData[,colLat])
  allData$colCategories <- allData[,colCategories]
  allCategories <- if(is.null(allCategories)) sort(unique(allData$colCategories)) else allCategories

  # Remove rows with NA values
  allData <- allData[complete.cases(allData[,c("lon", "lat", "colCategories")]),]
  if(nrow(allData) < 1) stop("No enought valid rows for make a plot.")

  # Check lengths of categoriesNames and allCategories
  if(!is.null(categoriesNames)){
    if(length(categoriesNames) != length(allCategories)){
      stop("'allCategories' and 'categoriesNames' must have the same length.")
    }
  }else{
    categoriesNames <- allCategories
  }

  # Check allCategories and colCategories
  if(!is.null(allCategories) & any(is.element(unique(colCategories), allCategories))){
    stop("At least one element on 'colCategories' is  not within 'allCategories'.")
  }

  # Set xlim/ylim
  xlim <- if(is.null(xlim)) range(allData$lon) else xlim[1:2]
  ylim <- if(is.null(ylim)) range(allData$lat) else ylim[1:2]
  xDelay <- if(is.null(xDelay)) abs(diff(xlim))/4 else xDelay[1]

  # Extend xlim by length of categoryVector
  prevXlim <- xlim
  xlim <- c(xlim[1] - xDelay*length(allCategories), xlim[2])

  # Create (or not) an empty canvas
  if(isTRUE(newPlot)){
    dev.new()
  }

  # Set plot params
  par(mar = c(2, 4, 1, 1), xaxs = "i", yaxs = "i")

  # Create empty canvas including sea...
  plot(1, 1, pch = NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = NA, ylab = NA)
  polygon(x = c(xlim, rev(xlim)), y = rep(ylim, each = 2), col = seaCol, border = FALSE)

  # ...and land
  map(database = "worldHires", add = TRUE, fill = TRUE, col = landCol, lty = "blank")
  map(database = "worldHires", add = TRUE, interior = FALSE)

  # Draw box text
  if(diff(xlim)/2){
    textBoxLimits <- list(x = xlim[2] - c(diff(prevXlim)/2, 0), y = ylim[2] - c(diff(ylim)/10, 0))
  }
  with(textBoxLimits, polygon(x = c(x, rev(x)), y = rep(y, each = 2), col = "white", border = TRUE))

  # Add text inside box
  textBox <- if(is.null(textBox)) "[Add some text here]" else textBox
  mtext(text = textBox, side = 3, adj = 0.99, line = textBox_line, cex = textBox_cex, font = textBox_font)

  # Draw lines by categories
  for(i in seq_along(allCategories)){
    index <- allData$colCategories == allCategories[i]
    tempData <- allData[index,]

    delayFactor <- xDelay*(1 - i)

    # Add coast line
    if(i != 1){
      coastline <- ruisu::coastline

      lines(coastline$lon + delayFactor, coastline$lat)
    }

    # Add points
    if(nrow(tempData) > 0){
      points(tempData$lon + delayFactor, tempData$lat, pch = 16, cex = 0.5)
    }

    # Add text of categories
    if(isTRUE(showBoxText)){
      text(x = prevXlim[1] + delayFactor + textBoxXCF, y = ylim[2] - 1, labels = categoriesNames[i],
           font = catNames_font, cex = catNames_cex, col = rep(catNames_col, length.out = length(allCategories))[i])
    }
  }

  # Add harbors
  if(isTRUE(addHarbors)){
    harborData <- ruisu::harborData

    with(harborData[harborData[,"importance"] == 1,], points(x = lon, y = lat, pch = 17, col = "red"))
    with(harborData[harborData[,"importance"] == 1,], text(x = lon, y = lat, labels = name, pos = 4, offset = .5))
  }

  # Add axis
  yInterval <- if(is.null(yInterval)) diff(ylim)/5 else yInterval[1]
  addCoordsAxes(yParams = c(ylim, yInterval), what = "y")
  box()

  return(invisible())
}
