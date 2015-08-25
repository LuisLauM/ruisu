mapParalels <- function(file, data = NULL, catCol, number.cat = NULL, names.cat = NULL, namespace.cat = 1, 
                        pos.cat = 1, adj.catY = 1, adj.catX = 0.1, abr.cat = FALSE, adj = 1, adjLon = c(0, 0), 
                        lwd.box = 1, oma = c(2.5, 3.5, 0.2, 0.2), ylim = NULL, cex.axisY = 1, atY = 1, 
                        xlim = NULL, interior = FALSE, col.map = "khaki", col.points = "blue3", lwd.map = 1,
                        hires = TRUE, offset = -0.7, allHarbors = FALSE, pch.harbors = 16, cex.harbors = 1, 
                        tcex.harbors = 1, alpha = 0.5, pch.points = 16, cex.points = 1, col.cat = "black", 
                        allHarborPoints = TRUE, cex.cat = 1)
{
  require(mapdata)
  require(calibrate)
  require(sp)
  require(rgdal)
  require(zoo)
  
  if(is.null(data))
    data <- read.csv(file, stringsAsFactors = FALSE)

  colnames(data) <- tolower(colnames(data))
  catCol <- colnames(data)[catCol]
  
  if(is.null(number.cat))
    number.cat <- unique(data[,catCol])  
  if(is.null(names.cat))
    names.cat <- number.cat
  
  coastline2 <- data9
  coastline <- data7
  
  harbors <- data6
  per200mn <- data8
  
  data <- subset(data, !is.na(data$lon) & !is.na(data$lat))
  data <- data.frame(No = seq(nrow(data)), data)
  coords <- data[,c("No", "lon", "lat")]
  coordinates(coords) <- ~ lon + lat
  coords <- SpatialPoints(coords)
  proj4string(coords) <- CRS(proj4string(per200mn))
  coords <- over(coords, per200mn)
  data <- data[coords == 0,]
  data <- subset(data, !is.na(data$lon) & !is.na(data$lat))
  
  lonRange <- range(data$lon)
  latRange <- range(data$lat)
  
  hires <- if(hires) "worldHires" else "world"
  
  par(mar = rep(0, 4), oma = oma)
  
  if(is.null(xlim))
    xlim <- range(data$lon)
  
  rangeLon <- diff(xlim)
  adjFactor <- (rangeLon*adj)*length(number.cat)
  max.xlim <- c(xlim[1] - rangeLon*adj*(length(number.cat) - 1), xlim[2])
  max.xlim <- c(floor(max.xlim[1]) - adjLon[1], ceiling(max.xlim[2]) + adjLon[2])
  
  if(is.null(ylim))
    ylim <- range(data$lat)
  
  ylim <- seq(floor(ylim[1]), ceiling(ylim[2]), atY)
  
  yLabels <- as.character(ylim)
  for(i in seq_along(yLabels))
    if(ylim[i] < 0)
      yLabels[i] <- paste0(-ylim[i], " °S") else
        if(ylim[i] > 0)
          yLabels[i] <- paste0(ylim[i], " °N")
  
  colourList <- rep(col.points, length(number.cat))
  
  tempData <- subset(data, data[,catCol] == number.cat[1])
  if(nrow(tempData) < 1)
  {
    tempData$lon <- tempData$lon
    plot(tempData$lon, tempData$lat, axes = FALSE, xlim = max.xlim, ylim = range(ylim),
         col = adjustcolor("white", alpha = 0),
         pch = pch.points, cex = cex.points)
  }else
  {
    tempData$lon <- tempData$lon
    plot(tempData$lon, tempData$lat, axes = FALSE, xlim = max.xlim, ylim = range(ylim),
         col = adjustcolor(colourList[1], alpha = alpha),
         pch = pch.points, cex = cex.points)
  }
  
  if(!interior)
  {
    map(hires, add = TRUE, col = col.map, fill = TRUE, lwd = lwd.map/10)
    map(hires, add = TRUE, col = col.map, lwd = lwd.map*3, interior = !interior)
  }else
    map(hires, add = TRUE, col = col.map, fill = TRUE, lwd = lwd.map)
  
  # points(coastline$lon, coastline$lat, type = "l", lwd = lwd.map)
  plot(coastline, add = T, lwd = lwd.map)
  
  names.cat <- rev(names.cat)
  if(abr.cat)
    names.cat <- substr(names.cat, 1, 3)
  
  if(length(names.cat) > 1)
  {
    for(i in seq(2, length(number.cat)))
    {
      tempData <- subset(data, data[,catCol] == number.cat[i])
      
      if(nrow(tempData) < 1)
      {
        tempData <- rbind(tempData, c(1, min(xlim), mean(ylim), number.cat[i]))
        points(tempData$lon, tempData$lat, pch = pch.points, cex = cex.points,
               col = adjustcolor("white", alpha = 0))
      }else
      {
        tempData$lon <- tempData$lon - rangeLon*adj*(i - 1)
        points(tempData$lon, tempData$lat, pch = pch.points, cex = cex.points,
               col = adjustcolor(colourList[i], alpha = alpha))
      }
      
      points(coastline2$lon - rangeLon*adj*(i - 1), coastline2$lat, type = "l", lwd = lwd.map)
      
      if(allHarborPoints)
      {
        points(harbors$lon - rangeLon*adj*(i - 1), harbors$lat, pch = pch.harbors, cex = cex.harbors)
        
        if(allHarbors)
          textxy(harbors$lon - rangeLon*adj*(i - 1), harbors$lat, harbors$name, offset = offset,
                 cex = tcex.harbors)
      }
    }
  }
  
  box(lwd = lwd.box)
  
  axis(2, at = ylim, las = 1, labels = yLabels, cex.axis = cex.axisY)
  if(length(names.cat) == 1)
  {
    xlim <- seq(floor(max.xlim[1]), ceiling(max.xlim[2]), atY)
    
    xLabels <- as.character(xlim)
    for(i in seq_along(xLabels))
      if(xlim[i] < 0)
        xLabels[i] <- paste0(-xlim[i], " °W") else
          if(xlim[i] > 0)
            xLabels[i] <- paste0(xlim[i], " °N")
    
    axis(1, at = xlim, las = 1, labels = xLabels, cex.axis = cex.axisY)
  }
  
  mtext(paste(names.cat, collapse = paste(rep(" ", namespace.cat), collapse="")),
        side = pos.cat, line = -adj.catY, adj = adj.catX, col = col.cat, cex = cex.cat)
  
  return(invisible())
}