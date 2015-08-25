PlotInGoogleMaps <- function(data, colLon = NA, colLat = NA, zoom = 5, ...)
{
  if(is.na(colLon))
    colLon <- match("lon", tolower(names(data)))
  
  if(is.na(colLat))
    colLat <- match("lat", tolower(names(data)))
  
  data <- data[complete.cases(data[,c(colLon, colLat)]), c(colLon, colLat)]
  colnames(data) <- c("lon", "lat")
  
  tempMap <- suppressMessages(get_map(c(lon = mean(data$lon, na.rm = TRUE),
                                        lat = mean(data$lat, na.rm = TRUE)), zoom = zoom))
  
  finalMap <- ggmap(tempMap) +
    geom_point(data = data, aes(x = lon, y = lat), ...)  
  
  return(finalMap)
}