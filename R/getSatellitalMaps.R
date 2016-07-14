#' @title Obtener información satelital de variables oceanográficas
#' @description Esta función toma un rango de fechas y coordenadas y extrae los mapas de distribución de
#' Temperatura Superficial del Mar (SST), Salinidad Superficial del Mar (SSS), Clorofila-a (chl) y
#' Topografía (topo)
#'
#' @param initialDate Fecha inicial para la extracción de mapas.
#' @param finalDate Fecha final para la extracción de mapas.
#' @param timeRes Resolución temporal para la obtención de mapas: Mensual ('month') o diario ('day').
#' @param what ¿Qué variable desea descargar? Ver detalles.
#' @param dateList Lista de fechas para la extracción de mapas.
#' @param lonRange Rango de coordenadas de longitud. Si es \code{NULL}, tomará valores entre 85°W y 90°W.
#' @param latRange Rango de coordenadas de latitud. Si es \code{NULL}, tomará valores entre 20°S y 2°S.
#' @param outputFormat Formato de descarga de archivos: NetCDF (nc), Valores separados por comas (csv),
#' image (png).
#' @param outputDir Directorio para la descarga de archivos.
#'
#' @export
#'
#' @examples
#' getSatellitalMaps(initialDate = "2014-3-26", finalDate = "2014-4-17", what = "sst", timeRes = "day",
#' lonRange = c(-90, -85), latRange = c(-5, 0), outputFormat = "png",
#' outputDir = "D:/output/")

getSatellitalMaps <- function(initialDate = NULL, finalDate = NULL, timeRes = "month", what = "SST", dateList = NULL,
                              lonRange = c(-85, -70), latRange = c(-20, -2), outputFormat = "png", outputDir = "."){

  # what: sst, sss, chl, topo

  # Verifying correct values for lon, lat and dates
  if(sum(lonRange < -180 | lonRange > 180) > 1)
    stop("Incorrect values for 'lonRange'.")

  if(sum(latRange < -90 | latRange > 90) > 1)
    stop("Incorrect values for 'latRange'.")

  if(all(is.null(initialDate), is.null(finalDate), is.null(dateList)))
    stop("You have to indicate some value for initialDate/finalDate or dateList")

  timePrefix <- switch(tolower(timeRes),
                       day = c("1", "Daily", "day"),
                       month = c("m", "Monthly", "month"),
                       "Incorrect value for 'timeRes'. Please, select between 'day' and 'month'.")

  # Make date vector
  if(!is.null(dateList)){
    dateList <- sort(as.Date(dateList))

    seqTime <- as.Date(paste(year(dateList), month(dateList), day(dateList), sep = "-"))
  }else{

    initialDate <- as.Date(initialDate)
    finalDate <- as.Date(finalDate)

    seqTime <- seq(from = as.Date(paste(year(initialDate), month(initialDate), day(initialDate), sep = "-")),
                   to = as.Date(paste(year(finalDate), month(finalDate), day(initialDate), sep = "-")),
                   by = timePrefix[3])

    if(initialDate > finalDate)
      stop("initialDate is after finalDate.")
  }

  # Get values for months and years
  yearList <- year(seqTime)
  monthList <- month(seqTime)
  dayList <- day(seqTime)

  monthList <- sapply(monthList, function(x) if(x < 10) paste0("0", x) else x)
  dayList <- sapply(dayList, function(x) if(x < 10) paste0("0", x) else x)

  # Define extension for output file
  formatList <- data.frame(format = c("nc", "csv", "largePng", "esriAscii"),
                           extension = c("nc", "csv", "png", "esriAscii"),
                           stringsAsFactors = FALSE)

  if(!is.element(tolower(outputFormat), formatList$extension))
    stop("Incorrect value for 'outputFormat'. Please, select between 'nc', 'csv' or 'png'.")

  formatIndex <- match(tolower(outputFormat), formatList$extension)

  # Organize lon and lat values
  lonRange <- lonRange2 <- sort(lonRange)
  latRange <- sort(latRange)

  lonRange <- sapply(lonRange, function(x) if(x < 0 & x >= -180) return(360 + x) else return(x))

  lonRange <- gsub(x = format(lonRange, nsmall = 1), pattern = " ", replacement = "", perl = TRUE)
  latRange <- gsub(x = format(latRange, nsmall = 1), pattern = " ", replacement = "", perl = TRUE)

  if(tolower(what) == "topo" && length(seqTime) > 1){
    cat(paste0("\n It makes little sense to download several maps of topography, as this is unchanged in the short term.\n",
               "\n Would you like to download just one map or continue (repeatedly) with the whole list of dates? ",
               "(1: Just once; 2: Whole list): "))
    topoAnswer <- scan(what = character(), nmax = 1, quiet = TRUE)
    cat("\n")

    if(topoAnswer == "1")
      seqTime <- seqTime[1]
  }


  # Initialize downloading loop
  errorList <- errorNames <- NULL
  for(i in seq_along(seqTime)){

    tempLons <- lonRange
    tempLats <- latRange

    datePrefix <- NULL

    # Define text parts for download URL
    if(tolower(what) == "sst"){
      # Prefix for SST
      if(seqTime[i] <= as.Date("2004-1-15")){
        prefix1 <- "erdPHssta"
        satellite <- "Pathfinder Ver 5.0"
      }else if(seqTime[i] <= as.Date("2011-1-15")){
        prefix1 <- "erdAGssta"
        satellite <- "POES AVHRR"
      }else{
        prefix1 <- "erdMBsstd"
        satellite <- "Aqua MODIS"
      }

      prefix1 <- paste0(prefix1, timePrefix[1], "day.")

      prefix2 <- "sst"
      prefix3 <- "[(0.0)]"

    }else if(tolower(what) == "topo"){
      # Prefix for topography
      prefix1 <- "usgsCeSrtm30v6."
      prefix2 <- "topo"
      prefix3 <- ""

      datePrefix <- ""

      tempLats <- rev(latRange)
      tempLons <- lonRange2

      satellite <- "SRTM30"

    }else if(tolower(what) == "chl"){
      # Prefix for chlorophyll
      if(seqTime[i] <= as.Date("2003-1-15")){
        prefix1 <- "erdSWchla"
        prefix3 <- "[(0.0)]"

        satellite <- "SeaWiFS"
      }else {
        prefix1 <- "erdMH1chla"
        prefix3 <- ""
        satellite <- "Aqua MODIS"

        tempLats <- rev(latRange)
        tempLons <- lonRange2
      }

      prefix1 <- paste0(prefix1, timePrefix[1], "day.")
      prefix2 <- "chlorophyll"

    }else if(tolower(what) == "sss"){

      # Prefix for salinity
      prefix1 <- paste0("jplAquariusSSS", timePrefix[2], "V4.")
      prefix2 <- "sss"
      prefix3 <- ""

      tempLats <- rev(latRange)
      tempLons <- lonRange2

      satellite <- "Aquarius SSS v4"
    }

    if(is.null(datePrefix))
      datePrefix <- paste0("[(", yearList[i], "-", monthList[i], "-", dayList[i],
                           "T00:00:00Z)]")

    # Concatenate parts and make URL for downloading
    tempURL <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/", prefix1,
                      formatList$format[formatIndex], "?", prefix2, datePrefix, prefix3, "[(", tempLats[1], "):(",
                      tempLats[2], ")][(", tempLons[1], "):(", tempLons[2], ")]&.draw=surface&.vars=longitude|latitude|",
                      prefix2, "&.colorBar=|||||")

    # print(tempURL)

    # If URL is incorrect or if file is not available, show warning message and save failed URL
    if(!url.exists(url = tempURL)){
      errorList <- c(errorList, tempURL)
      errorNames <- c(errorNames, as.character(seqTime[i]))

      cat(paste0("\n ", as.character(seqTime[i]), " ...... Failed! \n"))

      next
    }

    # Make output file name
    outputFile <- paste0(toupper(what), "-",
                         gsub(pattern = "[^\\d]+", replacement = "", x = seqTime[i], perl = TRUE),
                         "_Satellite-", satellite, "-", timePrefix[2], ".",
                         ifelse(outputFormat == "largePng", "png", formatList$extension[formatIndex]))

    suppressWarnings(download.file(url = tempURL, destfile = file.path(outputDir, outputFile), quiet = TRUE,
                                   mode = "wb"))

    cat(paste0("\n ", as.character(seqTime[i]), " ...... OK \n"))
  }

  if(!is.null(errorNames))
    names(errorList) <- errorNames

  cat(paste0("\n Download finished! There were ", length(errorList), " problems. \n"))

  # Final output will be a list with failed URLs
  return(if(is.null(errorList)) invisible() else errorList)
}
