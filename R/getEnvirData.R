
# Main fx -----------------------------------------------------------------

#' Get environmental info from lon-lat-time data frame
#'
#' @param x A \code{data.frame} with spatial-time information.
#' @param environmentalDir A \code{character} string for nc files.
#' @param dimVars_x A \code{list} with info about names (or position) for lon-lat-time columns on \code{x}. See Details.
#' @param dimVars_nc A \code{list} with info about names for lon-lat-time on nc files.
#' @param regridSize \code{NULL} by default. But if not, it indicates the size of a new grid, which is going to be applied
#' to data.
#' @param groupBy \code{character} string indicating whether to use data by 'month' or 'day' (default). See Details.
#' @param varName \code{character} strings with the name of the value for nc data. See Details.
#' @param justVarValues If \code{TRUE} (default) the function will return just a vector with the environmental values,
#' otherwise it will be the original \code{data.frame} plus the output as a extra column.
#' @param quiet If TRUE, suppress status messages as a progress bar.
#'
#' @details \code{dimVars_x} and \code{dimVars_nc} requires user specify \code{date_format} and \code{date_origin} for
#' original data frame and nc data, respectively. For details about valid values for this levels, please check
#' \code{\link{strptime}} and \code{\link{as.POSIXct}} help.
#'
#' This funtion has been developed for using nc files downloaded from ERDDAP server. Please check that your nc files
#' before to use.
#'
#' Environmental data must be storaged as nc files which each file will contain daily info for a certain month, so if the
#' user has monthly data, it must be (externally) splited by day (repeating the monthly values) and save in monthly files.
#' If \code{groupBy="month"}, the function will average the matrices
#'
#' If \code{varName} is not specified, the function will take the only available variable (if nc file has only one),
#' otherwise the function will stop and show the available variables for the nc. For this last option, function will
#' use a random file in order to get and show available variables, so it highly recomended not to put more than one
#' kind of nc files into the same directory.
#'
#' @return Depending on \code{justVarValues}, it could be a \code{numeric} vector or a \code{data.frame}.
#' @export
getEnvirData <- function(x, environmentalDir,
                         dimVars_x = list(lon = "lon", lat = "lat", date = "date", date_format = "%F"),
                         dimVars_nc = list(lon = "longitude", lat = "latitude", date = "time",
                                           date_origin = "1970-01-01T00:00:00Z"),
                         regridSize = NULL, groupBy = "day", varName = NULL,
                         justVarValues = TRUE, quiet = FALSE){
  # Preserve original x
  oldX <- x

  # Extract Dates from x data
  if(!is.Date(x[,dimVars_x$date]) && !is.POSIXt(x[,dimVars_x$date])){
    xDates <- as.Date(x[,dimVars_x$date], format = dimVars_x$date_format)
  }else{
    xDates <- as.Date(x[,dimVars_x$date])
  }

  dateBreaks <- c(seq(from = min(xDates, na.rm = TRUE), to = max(xDates, na.rm = TRUE), by = groupBy),
                  as.Date(1e7, origin = "1970-1-1"))

  xDates <- as.Date(ac(cut(x = xDates, breaks = dateBreaks,  labels = dateBreaks[-length(dateBreaks)])))



  if(groupBy == "month"){
    xDates <- as.Date(paste(year(xDates), month(xDates), 15, sep = "-"))
  }

  x <- data.frame(lon = as.numeric(x[,dimVars_x$lon]),
                  lat = as.numeric(x[,dimVars_x$lat]),
                  date = xDates,
                  stringsAsFactors = FALSE)

  orderIndex <- order(x$date)
  x <- x[orderIndex,]

  allNC <- list.files(path = environmentalDir, pattern = ".nc", full.names = TRUE)

  ncRanges <- lapply(allNC, getRangesNC)
  ncRanges <- do.call(rbind, ncRanges)

  output <- rep(NA, nrow(x))
  for(i in seq(nrow(ncRanges))){
    tempRange <- as.Date(ncRanges[i,])

    indexData <- which(x$date >= tempRange[1] & x$date <= tempRange[2])
    if(length(indexData) < 1) next

    ncFile <- nc_open(filename = allNC[i])

    if(is.null(varName)){
      if(length(ncFile$var) == 1){
        varName <- names(ncFile$var)
      }else{
        message("\nAvailable variables: ", paste(names(ncFile$var), collapse = ", "),
                "\nPlease, select one of them, copy-paste into 'varName' space and re run the function.")
      }
    }

    ncData <- ncvar_get(nc = ncFile, varid = varName)
    ncDates <- as.Date(as.POSIXct(ncFile$dim[[dimVars_nc$date]]$vals, origin = dimVars_nc$date_origin))

    envirList <- list(x = ncFile$dim[[dimVars_nc$lon]]$vals,
                      y = ncFile$dim[[dimVars_nc$lat]]$vals,
                      z = ncData)

    nc_close(nc = ncFile)

    if(!is.null(regridSize)){
      rangeCoord <- range(envirList$x)
      newX <- seq(floor(rangeCoord[1]), ceiling(rangeCoord[2]), regridSize)
      newX <- newX[newX >= rangeCoord[1] & newX <= rangeCoord[2]]

      rangeCoord <- range(envirList$y)
      newY <- seq(floor(rangeCoord[1]), ceiling(rangeCoord[2]), regridSize)
      newY <- newY[newY >= rangeCoord[1] & newY <= rangeCoord[2]]

      newGrid <- expand.grid(x = newX, y = newY, stringsAsFactors = FALSE)
    }

    envirList$x <- seq(min(envirList$x), max(envirList$x), length.out = length(envirList$x))
    envirList$y <- seq(min(envirList$y), max(envirList$y), length.out = length(envirList$y))

    if(grepl(pattern = "HYCOM_SSS", x = environmentalDir)){
      envirList$x <- envirList$x - 360

      envirList$z <- envirList$z[,,1,]
    }

    if(groupBy == "month"){
      monthlyDate <- as.Date(paste(median(year(ncDates)), median(month(ncDates)), 15, sep = "-"))

      index <- as.yearmon(ncDates) == as.yearmon(monthlyDate)
      envirList$z <- apply(envirList$z[,,index], c(1, 2), mean, na.rm = TRUE)
      ncDates <- monthlyDate
    }

    for(j in seq_along(indexData)){
      k <- indexData[j]
      if(j == 1 || x$date[k] != x$date[indexData[j - 1]]){
        index <- ncDates == x$date[k]
        if(sum(index) < 1) next

        tempRaster <- envirList

        if(length(dim(tempRaster$z)) == 3){
          tempRaster$z <- tempRaster$z[,,index]
        }

        tempRaster <- raster(tempRaster)

        if(!is.null(regridSize)){
          tempGrid <- newGrid

          tempGrid$z <- extract(x = tempRaster, y = tempGrid)

          tempRaster <- aggregate(x = tempRaster, fact = regridSize/res(tempRaster),
                                  fun = mean, na.rm = TRUE)
        }
      }

      output[k] <- extract(x = tempRaster, y = x[k, 1:2])
    }

    if(!isTRUE(quiet)){
      progressBar(i = i, n = nrow(ncRanges))
    }
  }

  outputData <- output[order(orderIndex)]
  if(!justVarValues){
    outputData <- data.frame(oldX, outputData, stringsAsFactors = FALSE)
    colnames(outputData) <- c(colnames(oldX), basename(environmentalDir))
  }

  return(outputData)
}


# Auxiliar fx -------------------------------------------------------------

getRangesNC <- function(filename){
  ncFile <- nc_open(filename = filename)

  output <- range(as.Date(as.POSIXct(ncFile$dim$time$vals, origin = "1970-1-1 00:00:00")))

  nc_close(nc = ncFile)

  return(as.character(output))
}
