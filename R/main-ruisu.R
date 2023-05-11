#' @importFrom sp dd2dms proj4string over coordinates "coordinates<-" "proj4string<-" spDistsN1 spDists plot CRS SpatialPoints
#' @importFrom rgeos gBuffer
#' @importFrom fields tim.colors image.plot
#' @importFrom zoo rollmean
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' @importFrom foreach foreach "%dopar%"
#' @importFrom stats filter aggregate approx complete.cases median rbeta rnorm runif spline
#' @importFrom maps map
#' @importFrom rstudioapi initializeProject openProject
#' @importFrom grDevices adjustcolor dev.list dev.new dev.off rainbow
#' @importFrom graphics abline arrows axis barplot box grid image lines mtext par plot.new plot.window points polygon text
#' @importFrom utils read.csv write.csv
#' @importFrom lubridate is.Date is.POSIXt year month
#' @importFrom magrittr "%>%"
NULL

#' @title Tools for habitual tasks (and another fancy things) at IMARPE
#' @author Wencheng Lau-Medrano, \email{luis.laum@@gmail.com}
#' @name ruisu-package
#' @description Tools for common tasks as well as some fancy functions
#' @aliases ruisu-package ruisu
#' @docType package
#' @keywords miscellany, IMARPE
NULL

#' @title \code{data.frame} with new (corrected) AIP information.
#' @name AIPData_new
#' @description \code{data.frame} with 7 columns:
#' @aliases AIPData_new
#' @docType data
#' @usage AIPData_new
#' @format A \code{data.frame} with
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{data.frame} with old (original) AIP information.
#' @name AIPData_old
#' @description \code{data.frame} with 7 columns:
#' @aliases AIPData_old
#' @docType data
#' @usage AIPData_old
#' @format A \code{data.frame} with
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{shapefile} with old (original) AIP information.
#' @name AIPShapefile_old
#' @description \code{SpatialPolygonsDataFrame} object.
#' @aliases AIPShapefile_old
#' @docType data
#' @usage AIPShapefile_old
#' @format A \code{SpatialPolygonsDataFrame}
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{shapefile} with new (corrected) AIP information.
#' @name AIPShapefile_new
#' @description \code{SpatialPolygonsDataFrame} object.
#' @aliases AIPShapefile_new
#' @docType data
#' @usage AIPShapefile_new
#' @format A \code{SpatialPolygonsDataFrame}
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{data.frame} with information of the main Peruvian harbors
#' @name harborData
#' @description Some detailed information of the main Peruvian harbors.
#' @aliases harborData
#' @docType data
#' @usage harborData
#' @format A \code{data.frame} with information of Peruvian harbors: name, pattern, lon, lat, area, importance. See Details
#' @details \code{pattern} information is used by \code{getHarbor} funtion for making a match between given strings and
#' the harbor names of \code{harbordata}.
#' \code{importance} column separates the principal harbors (1 value) from the secondary ones (0 values).
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{data.frame} with coordinates of the eastern side of the American coast.
#' @name coastline
#' @description coordinates of the eastern side of the American coast.
#' @aliases coastline
#' @docType data
#' @usage coastline
#' @format A \code{data.frame} with coordinates of the eastern side of the American coast: country, lon, lat
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{charcater} vector with names of months in Spanish.
#' @name month.name_spanish
#' @description Similar to \code{month.name} but in Spanish.
#' @aliases month.name_spanish
#' @docType data
#' @usage month.name_spanish
#' @format A \code{charcater} vector with names of months in Spanish.
NULL

#' @title \code{charcater} vector with the three-letter abbreviations for names of months in Spanish.
#' @name month.abb_spanish
#' @description Similar to \code{month.abb} but in Spanish.
#' @aliases month.abb_spanish
#' @docType data
#' @usage month.abb_spanish
#' @format A \code{charcater} vector with the three-letter abbreviations for names of months in Spanish.
NULL

#' @title \code{data.frame} with main information about some Peruvian fishes.
#' @name speciesInfo
#' @description A table with information of scientific name, type of measurement of length, typycal unit
#' for expresing this measurements, Minimum length, Maximum length, typical interval length, juvenile length.
#' @aliases speciesInfo
#' @docType data
#' @usage speciesInfo
#' @format A \code{data.frame} with main information about some Peruvian fishes.
NULL

#' @title \code{data.frame} with environmental scenarios given by IGP
#' @name envirScenarios
#' @description A table with information of El Nino/La Nina events classification from 1951 to 2016.
#' @aliases envirScenarios
#' @docType data
#' @usage envirScenarios
#' @format A \code{data.frame} with environmental scenarios given by IGP (year_start, month_start,
#' year_end, month_end, magnitude, what).
NULL

#' @title \code{list} with a test inspired on Harry Potter books.
#' @name testExample
#' @description A \code{list} including Q&A inspired on Harry Potter books in order to clarify how
#' to organize inputs for \code{sillyTest} function.
#' @aliases testExample
#' @docType data
#' @usage testExample
#' @format A \code{list} with two levels: 1) The first, the questions as a \code{character} vector and
#' 2) the second, the posible answers for each question.
NULL

#' @title \code{SpatialPolygonsDataFrame} object with a buffer in meters of South America coastline.
#' @name coastlineBuffer_m
#' @description \code{SpatialPolygonsDataFrame} object with a buffer in meters of South America coastline.
#' @aliases coastlineBuffer_m
#' @docType data
#' @usage coastlineBuffer_m
#' @format \code{SpatialPolygonsDataFrame} object with a buffer in meters of South America coastline.
NULL

#' @title \code{SpatialPolygonsDataFrame} object with a buffer in nautical miles of South America coastline.
#' @name coastlineBuffer_nm
#' @description \code{SpatialPolygonsDataFrame} object with a buffer in nautical miles of South America coastline.
#' @aliases coastlineBuffer_nm
#' @docType data
#' @usage coastlineBuffer_nm
#' @format \code{SpatialPolygonsDataFrame} object with a buffer in nautical miles of South America coastline.
NULL
