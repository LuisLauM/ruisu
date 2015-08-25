plotIsoparalitoral <- function(database = "worldHires", old = TRUE, xlim = NULL, ylim = NULL,
                               add = FALSE, col.map = par("col"), bg = par("bg"), areaList = NULL,
                               areaCols = par("bg"), codeLabs = FALSE, m.codeLab = c(0, 0),
                               cex.codeLab = 0.5, offset.codeLab = 0.8, col.codeLab = "black",
                               ...)
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
  
  if(is.null(xlim))
    xlim <- c(-88, -68)
  
  if(is.null(ylim))
    ylim <- c(-20, 0)
  
  # Read polygon of '?reas isoparalitorales' and convert to SpatialPolygonsDataFrame object  
  if(old)
  {
    peru.isoparalit <- data3
    
    centroids <- data1
  }else
  {
    peru.isoparalit <- data4
    
    centroids <- data2
  }
  
  peru_withoutCoastline <- data5
  
  if(!add)
  {
    map(database, xlim = xlim, ylim = ylim, col = col.map, mar = rep(0.1, 4), bg = bg)
    map(database, regions = ".*peru", col = bg, add = T)
    plot(peru_withoutCoastline, add = TRUE)
    plot(data7, add = TRUE)
  }
  
  areaList2 <- areaList
  
  if(!is.null(areaList))
  {
    areaList <- match(areaList, peru.isoparalit$CODIGO)
    
    if(sum(is.na(areaList)) > 0)
      if(sum(is.na(areaList)) == length(areaList))
        stop("All codes in 'areaList' are incorrect.") else
          warning("There exist some incorrect isoparalitoral codes.")
    
    areaList <- areaList[!is.na(areaList)]
  }else
    areaList <- seq_along(peru.isoparalit$CODIGO)
  
  areaCols <- rep(areaCols, length.out = length(areaList))
  
  plot(peru.isoparalit[areaList, 1], add = T, col = areaCols, ...)
  
  if(codeLabs)
  {
    centroids <- centroids[match(areaList2, centroids$code),]
    
    textxy(centroids$lon, centroids$lat, centroids$code,
           m = m.codeLab, cex = cex.codeLab, offset = offset.codeLab, col = col.codeLab)
  }
  
  return(invisible())
}