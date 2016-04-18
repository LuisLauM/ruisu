getSatellitalMaps <- function(initialDate = NULL, finalDate = NULL, what = "SST", dateList = NULL,
                              lonRange = c(-85, -70), latRange = c(-20, -2), outputDir = ".", outputFormat = "png"){

  # what: sst, sss, chl, topo

  # Verifying correct values for lon, lat and dates
  if(sum(lonRange < -180 | lonRange > 180) > 1)
    stop("Incorrect values for 'lonRange'.")

  if(sum(latRange < -90 | latRange > 90) > 1)
    stop("Incorrect values for 'latRange'.")

  if(all(is.null(initialDate), is.null(finalDate), is.null(dateList)))
    stop("You have to indicate some value for initialDate/finalDate or dateList")

  # Make date vector
  if(!is.null(dateList)){
    dateList <- sort(as.Date(dateList))

    seqTime <- as.Date(paste(year(dateList), month(dateList), 1, sep = "-"))
  }else{
    initialDate <- as.Date(initialDate)
    finalDate <- as.Date(finalDate)

    seqTime <- seq(from = as.Date(paste(year(initialDate), month(initialDate), 1, sep = "-")),
                   to = as.Date(paste(year(finalDate), month(finalDate), 1, sep = "-")),
                   by = "mon")

    if(initialDate > finalDate)
      stop("initialDate is after finalDate.")
  }

  # Get values for months and years
  yearList <- year(seqTime)
  monthList <- month(seqTime)

  monthList <- sapply(monthList, function(x) if(x < 10) paste0("0", x) else x)

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
        prefix1 <- "erdPHsstamday."
        satellite <- "Pathfinder Ver 5.0"
      }else if(seqTime[i] <= as.Date("2011-1-15")){
        prefix1 <- "erdAGsstamday."
        satellite <- "POES AVHRR"
      }else{
        prefix1 <- "erdMBsstdmday."
        satellite <- "Aqua MODIS"
      }

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
        prefix1 <- "erdSWchlamday."
        prefix3 <- "[(0.0)]"

        satellite <- "SeaWiFS"
      }else {
        prefix1 <- "erdMH1chlamday."
        prefix3 <- ""
        satellite <- "Aqua MODIS"

        tempLats <- rev(latRange)
        tempLons <- lonRange2
      }

      prefix2 <- "chlorophyll"

    }else if(tolower(what) == "sss"){
      # Prefix for salinity
      prefix1 <- "jplAquariusSSSMonthlyV4."
      prefix2 <- "sss"
      prefix3 <- "[(0.0)]"

      satellite <- "Aquarius SSS v4"
    }

    if(is.null(datePrefix))
      datePrefix <- paste0("[(", yearList[i], "-", monthList[i], "-16T12:00:00Z)]")

    # Concatenate parts and make URL for downloading
    tempURL <- paste0("http://coastwatch.pfeg.noaa.gov/erddap/griddap/", prefix1,
                      formatList$format[formatIndex], "?", prefix2, datePrefix, prefix3, "[(", tempLats[1], "):(",
                      tempLats[2], ")][(", tempLons[1], "):(", tempLons[2], ")]&.draw=surface&.vars=longitude|latitude|",
                      prefix2, "&.colorBar=|||||")

    # print(tempURL)

    # If URL is incorrect or if file is not available, show warning message and save failed URL
    if(!url.exists(url = tempURL)){
      errorList <- c(errorList, tempURL)
      errorNames <- c(errorNames, as.character(seqTime[i]))

      cat(paste0("\n ", as.character(as.yearmon(seqTime[i])), " ...... Failed! \n"))

      next
    }

    # Make output file name
    outputFile <- paste0(toupper(what), "-",
                         gsub(pattern = "[^\\d]+", replacement = "", x = seqTime[i], perl = TRUE),
                         "_Satellite-", satellite, ".",
                         ifelse(outputFormat == "largePng", "png", formatList$extension[formatIndex]))

    suppressWarnings(download.file(url = tempURL, destfile = file.path(outputDir, outputFile), quiet = TRUE,
                                   mode = "wb"))

    cat(paste0("\n ", as.character(as.yearmon(seqTime[i])), " ...... OK \n"))
  }

  if(!is.null(errorNames))
    names(errorList) <- errorNames

  cat(paste0("\n Download finished! There were ", length(errorList), " problems. \n"))

  # Final output will be a list with failed URLs
  return(if(is.null(errorList)) invisible() else errorList)
}

# # DEMO
# getSatellitalMaps(initialDate = "1992-11-15", finalDate = "1993-4-2",
#            lonRange = c(-85, -80), latRange = c(-5, 0),
#            outputFormat = "csv")
#
# getSatellitalMaps(dateList = c("2000-1-1", "2007-4-8", "2012-12-3"), what = "chl")
# getSatellitalMaps(dateList = c("2000-1-1", "2007-4-8", "2012-12-3"), what = "chl", outputFormat = "nc")
