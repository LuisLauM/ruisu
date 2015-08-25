isopArea.assigner <- function(data, colLon = NA, colLat = NA, old = TRUE)
{
  require(sp)
  require(rgdal)
  require(mapdata)
  
  rm.extension <- function(path)
  {
    path <- basename(path)
    newPath <- unlist(gregexpr("[[:punct:]]", path))
    newPath <- substr(path, 1, tail(newPath, 1) - 1)
    
    return(newPath)
  }
  
  if(is.na(colLon))
    colLon <- match("lon", tolower(colnames(data)))
  
  if(is.na(colLat))
    colLat <- match("lat", tolower(colnames(data)))
  
  data <- data.frame(lon = data[,colLon], lat = data[,colLat], stringsAsFactors = FALSE)
  isoCodes <- numeric(nrow(data))
  
  # Read points and convert to SpatialPointsDataFrame object
  points <- data; rm(data)
  
  isoCodes[which(is.na(points$lon) | is.na(points$lat))] <- NA
  coords <- points[!is.na(isoCodes),]
  coordinates(coords) <- ~ lon + lat
  
  # Read polygon of '?reas isoparalitorales' and convert to SpatialPolygonsDataFrame object  
  if(old)
  {
    peru.isoparalit <- data3
  }else
  {
    peru.isoparalit <- data4
  }
  
  proj4string(coords) <- CRS(proj4string(peru.isoparalit))
  
  isoCodes[!is.na(isoCodes)] <- as.numeric(as.matrix(over(coords, peru.isoparalit)))
  
  return(isoCodes)
}
