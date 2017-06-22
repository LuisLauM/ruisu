#' #@title Get satellital information for oceanographic variables
#' #@description This function uses date and space range and get maps for Sea Surface Temperature (SST),
#' Sea Surface Salinity (SSS), Chlorophyll-a (Chl), Topography (topo), Sea level (sealevel) and SODA
#' estimations for Temperature (sst_soda) and Salinity (sss_soda) by depth (\code{atDepth} parameter).
#'
#' #@param initialDate Initial date.
#' #@param finalDate Final date.
#' #@param timeRes Time resolution for maps: Monthly ('month') o daily ('day').
#' #@param what What variable do you want to download? See details.
#' #@param dateList List with dates for downloading..
#' #@param lonRange Range of longitude. If \code{NULL}, it will take values from 85 W to 90 W (Peru).
#' #@param latRange Range of latitude. If \code{NULL}, it will take values from 2 S to 20 S (Peru).
#' #@param outputFormat What extension would you like to use for download? (nc, csv, png).
#' #@param outputDir Folder for downloading.
#' #@param atDepth Depth (usually meters) of layer to downloading.
#' #@param showURL Do you want to show the URL each time?
#'
#' #@details Function will use data bases of ERDDAP website
#' \link{https://coastwatch.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000}.
#'
#' This function will use the dates values to choose from what source the values will be taken.
#'
#' #@export
#'
#' #@examples
#' getSatellitalMaps(initialDate = "2014-3-26", finalDate = "2014-4-17", what = "sst", timeRes = "day",
#' lonRange = c(-90, -85), latRange = c(-5, 0), outputFormat = "png",
#' outputDir = "../")
# getSatellitalMaps <- function(initialDate = NULL, finalDate = NULL, timeRes = "month", what = "SST", atDepth = NULL,
#                               dateList = NULL, lonRange = c(-85, -70), latRange = c(-20, -2), outputFormat = "png",
#                               outputDir = ".", showURL = FALSE){
#
#   getSatellitalMaps_internal(initialDate, finalDate, timeRes, what, atDepth, dateList, lonRange, latRange, outputFormat,
#                              outputDir, showURL)
#
#   cat(paste0("\n Download finished! There were ", length(errorList), " problems. \n"))
#
#
#   # Final output will be a list with failed URLs
#   return(if(is.null(errorList)) invisible() else errorList)
# }

# getSatellitalMaps_internal <- function(initialDate, finalDate, timeRes, what, atDepth, dateList, lonRange,
#                                        latRange, outputFormat, outputDir, showURL){
#
#   # what: sst, sss, chl, topo
#
#   # Verifying correct values for lon, lat and dates
#   if(sum(lonRange < -180 | lonRange > 180) > 1)
#     stop("Incorrect values for 'lonRange'.")
#
#   if(sum(latRange < -90 | latRange > 90) > 1)
#     stop("Incorrect values for 'latRange'.")
#
#   if(all(is.null(initialDate), is.null(finalDate), is.null(dateList)))
#     stop("You have to indicate some value for initialDate/finalDate or dateList")
#
#   timePrefix <- switch(tolower(timeRes),
#                        day = c("1", "Daily", "day"),
#                        month = c("m", "Monthly", "month"),
#                        "Incorrect value for 'timeRes'. Please, select between 'day' and 'month'.")
#
#   # Make date vector
#   if(!is.null(dateList)){
#     dateList <- sort(as.Date(dateList))
#
#     seqTime <- as.Date(paste(year(dateList), month(dateList), day(dateList), sep = "-"))
#   }else{
#
#     initialDate <- as.Date(initialDate)
#     finalDate <- as.Date(finalDate)
#
#     seqTime <- seq(from = as.Date(paste(year(initialDate), month(initialDate), day(initialDate), sep = "-")),
#                    to = as.Date(paste(year(finalDate), month(finalDate), day(finalDate), sep = "-")),
#                    by = timePrefix[3])
#
#     if(initialDate > finalDate)
#       stop("initialDate is after finalDate.")
#   }
#
#   # Get values for months and years
#   yearList <- year(seqTime)
#   monthList <- month(seqTime)
#   dayList <- day(seqTime)
#
#   monthList <- sapply(monthList, function(x) if(x < 10) paste0("0", x) else x)
#   dayList <- sapply(dayList, function(x) if(x < 10) paste0("0", x) else x)
#
#   # Define extension for output file
#   formatList <- data.frame(format = c("nc", "csv", "largePng", "esriAscii"),
#                            extension = c("nc", "csv", "png", "esriAscii"),
#                            stringsAsFactors = FALSE)
#
#   if(!is.element(tolower(outputFormat), formatList$extension))
#     stop("Incorrect value for 'outputFormat'. Please, select between 'nc', 'csv' or 'png'.")
#
#   formatIndex <- match(tolower(outputFormat), formatList$extension)
#
#   # Organize lon and lat values
#   lonRange <- lonRange2 <- sort(lonRange)
#   latRange <- sort(latRange)
#
#   lonRange <- sapply(lonRange, function(x) if(x < 0 & x >= -180) return(360 + x) else return(x))
#
#   lonRange <- gsub(x = format(lonRange, nsmall = 1), pattern = " ", replacement = "", perl = TRUE)
#   latRange <- gsub(x = format(latRange, nsmall = 1), pattern = " ", replacement = "", perl = TRUE)
#
#   if(tolower(what) == "topo" && length(seqTime) > 1){
#     cat(paste0("\n It makes little sense to download several maps of topography, as this is unchanged in the short term.\n",
#                "\n Would you like to download just one map or continue (repeatedly) with the whole list of dates? ",
#                "(1: Just once; 2: Whole list): "))
#     topoAnswer <- scan(what = character(), nmax = 1, quiet = TRUE)
#     cat("\n")
#
#     if(topoAnswer == "1")
#       seqTime <- seqTime[1]
#   }
#
#
#   # Initialize downloading loop
#   errorList <- errorNames <- NULL
#   for(i in seq_along(seqTime)){
#
#     tempLons <- lonRange
#     tempLats <- latRange
#
#     datePrefix <- NULL
#
#     # Define text parts for download URL
#     if(tolower(what) == "sst"){
#       # Prefix for SST
#       if(seqTime[i] <= as.Date("2004-1-15")){
#         if(tolower(timeRes) == "day"){
#           prefix1 <- "erdPHssta"
#           satellite <- "Pathfinder Ver 5.0"
#         }else if(tolower(timeRes) == "month"){
#           prefix1 <- "erdMTsstd"
#           satellite <- "Terra MODIS"
#         }
#       }else if(seqTime[i] <= as.Date("2011-1-15")){
#         prefix1 <- "erdAGssta"
#         satellite <- "POES AVHRR"
#       }else{
#         prefix1 <- "erdMBsstd"
#         satellite <- "Aqua MODIS"
#       }
#
#       prefix1 <- paste0(prefix1, timePrefix[1], "day_LonPM180.")
#
#       prefix2 <- "sst"
#       prefix3 <- "[(0.0)]"
#
#       tempLons <- lonRange2
#
#     }else if(tolower(what) == "sst_soda"){
#
#       # Prefix for salinity
#       prefix1 <- "hawaii_d90f_20ee_c4cb_LonPM180."
#       prefix2 <- "temp"
#       prefix3 <- paste0("[(", ifelse(is.null(atDepth), 5.01, atDepth), ")]")
#
#       # tempLats <- rev(latRange)
#       tempLons <- lonRange2
#
#       satellite <- "SODA-POP-2.2.4"
#     }else if(tolower(what) == "topo"){
#       # Prefix for topography
#       prefix1 <- "usgsCeSrtm30v6."
#       prefix2 <- "topo"
#       prefix3 <- ""
#
#       datePrefix <- ""
#
#       tempLats <- rev(latRange)
#       tempLons <- lonRange2
#
#       satellite <- "SRTM30"
#
#     }else if(tolower(what) == "chl"){
#       # Prefix for chlorophyll
#       if(seqTime[i] <= as.Date("2003-1-15")){
#         prefix1 <- "erdSWchla"
#         prefix3 <- "[(0.0)]"
#
#         satellite <- "SeaWiFS"
#       }else {
#         prefix1 <- "erdMH1chla"
#         prefix3 <- ""
#         satellite <- "Aqua MODIS"
#
#         tempLats <- rev(latRange)
#         tempLons <- lonRange2
#       }
#
#       prefix1 <- paste0(prefix1, timePrefix[1], "day.")
#       prefix2 <- "chlorophyll"
#
#     }else if(tolower(what) == "sss"){
#
#       # Prefix for salinity
#       prefix1 <- paste0("jplAquariusSSS", timePrefix[2], "V4.")
#       prefix2 <- "sss"
#       prefix3 <- ""
#
#       tempLats <- rev(latRange)
#       tempLons <- lonRange2
#
#       satellite <- "Aquarius SSS v4"
#     }else if(tolower(what) == "sss_soda"){
#
#       # Prefix for salinity
#       prefix1 <- "hawaii_d90f_20ee_c4cb_LonPM180."
#       prefix2 <- "salt"
#       prefix3 <- paste0("[(", ifelse(is.null(atDepth), 5.01, atDepth), ")]")
#
#       # tempLats <- rev(latRange)
#       tempLons <- lonRange2
#
#       satellite <- "SODA-POP-2.2.4"
#     }else if(tolower(what) == "sealevel"){
#
#       # Prefix for sea level
#       prefix1 <- paste0("erdTAssh", timePrefix[1], "day_LonPM180.")
#       prefix2 <- "ssh"
#       prefix3 <- "[(0.0)]"
#
#       tempLons <- lonRange2
#
#       satellite <- "AVISOcean"
#     }
#
#     if(is.null(datePrefix))
#       datePrefix <- paste0("[(", yearList[i], "-", monthList[i], "-", dayList[i],
#                            "T00:00:00Z)]")
#
#     # Concatenate parts and make URL for downloading
#     tempURL <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/", prefix1,
#                       formatList$format[formatIndex], "?", prefix2, datePrefix, prefix3, "[(", tempLats[1], "):(",
#                       tempLats[2], ")][(", tempLons[1], "):(", tempLons[2], ")]&.draw=surface&.vars=longitude|latitude|",
#                       prefix2, "&.colorBar=|||||")
#
#     if(isTRUE(showURL)){
#       cat(paste0("\n", tempURL))
#     }
#
#     # If URL is incorrect or if file is not available, show warning message and save failed URL
#     if(!url.exists(url = tempURL)){
#       errorList <- c(errorList, tempURL)
#       errorNames <- c(errorNames, ac(seqTime[i]))
#
#       cat(paste0("\n ", ac(seqTime[i]), " ...... Failed! \n"))
#
#       next
#     }
#
#     # Make output file name
#     outputFile <- paste0(toupper(what), "-",
#                          gsub(pattern = "[^\\d]+", replacement = "", x = seqTime[i], perl = TRUE),
#                          "_Satellite-", satellite, "-", timePrefix[2], ".",
#                          ifelse(outputFormat == "largePng", "png", formatList$extension[formatIndex]))
#
#     suppressWarnings(download.file(url = tempURL, destfile = file.path(outputDir, outputFile), quiet = TRUE,
#                                    mode = "wb"))
#
#     cat(paste0("\n ", ac(seqTime[i]), " ...... OK \n"))
#   }
#
#   if(!is.null(errorNames))
#     names(errorList) <- errorNames
#
#   # Final output will be a list with failed URLs
#   return(if(is.null(errorList)) invisible() else errorList)
# }
