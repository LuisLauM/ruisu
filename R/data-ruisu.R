#' @title \code{data.frame} with new (corrected) AIP information.
#' @name AIPData_new
#' @description \code{data.frame} with 7 columns:
#' @aliases AIPData_new
#' @usage AIPData_new
#' @format A \code{data.frame} with
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{data.frame} with old (original) AIP information.
#' @name AIPData_old
#' @description \code{data.frame} with 7 columns:
#' @aliases AIPData_old
#' @usage AIPData_old
#' @format A \code{data.frame} with
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{shapefile} with old (original) AIP information.
#' @name AIPShapefile_old
#' @description \code{SpatialPolygonsDataFrame} object.
#' @aliases AIPShapefile_old
#' @usage AIPShapefile_old
#' @format A \code{SpatialPolygonsDataFrame}
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{shapefile} with new (corrected) AIP information.
#' @name AIPShapefile_new
#' @description \code{SpatialPolygonsDataFrame} object.
#' @aliases AIPShapefile_new
#' @usage AIPShapefile_new
#' @format A \code{SpatialPolygonsDataFrame}
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{data.frame} with information of the main Peruvian harbors
#' @name harborData
#' @description Some detailed information of the main Peruvian harbors.
#' @aliases harborData
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
#' @usage coastline
#' @format A \code{data.frame} with coordinates of the eastern side of the American coast: country, lon, lat
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{charcater} vector with names of months in Spanish.
#' @name month.name_spanish
#' @description Similar to \code{month.name} but in Spanish.
#' @aliases month.name_spanish
#' @usage month.name_spanish
#' @format A \code{charcater} vector with names of months in Spanish.
NULL

#' @title \code{charcater} vector with the three-letter abbreviations for names of months in Spanish.
#' @name month.abb_spanish
#' @description Similar to \code{month.abb} but in Spanish.
#' @aliases month.abb_spanish
#' @usage month.abb_spanish
#' @format A \code{charcater} vector with the three-letter abbreviations for names of months in Spanish.
NULL

#' @title \code{data.frame} with main information about some Peruvian fishes.
#' @name speciesInfo
#' @description A table with information of scientific name, type of measurement of length, typycal unit
#' for expresing this measurements, Minimum length, Maximum length, typical interval length, juvenile length.
#' @aliases speciesInfo
#' @usage speciesInfo
#' @format A \code{data.frame} with main information about some Peruvian fishes.
NULL

#' @title \code{data.frame} with environmental scenarios given by IGP
#' @name envirScenarios
#' @description A table with information of El Nino/La Nina events classification from 1951 to 2016.
#' @aliases envirScenarios
#' @usage envirScenarios
#' @format A \code{data.frame} with environmental scenarios given by IGP (year_start, month_start,
#' year_end, month_end, magnitude, what).
NULL

#' @title \code{list} with a test inspired on Harry Potter books.
#' @name sillyTest
#' @description A \code{list} including Q&A inspired on Harry Potter books in order to clarify how
#' to organize inputs for \code{sillyTest} function.
#' @aliases sillyTest
#' @usage sillyTest
#' @format A \code{list} with two levels: 1) The first, the questions as a \code{character} vector and
#' 2) the second, the posible answers for each question.
NULL

#' @title \code{SpatialPolygonsDataFrame} object with a buffer in meters of South America coastline.
#' @name coastlineBuffer_m
#' @description \code{SpatialPolygonsDataFrame} object with a buffer in meters of South America coastline.
#' @aliases coastlineBuffer_m
#' @usage coastlineBuffer_m
#' @format \code{SpatialPolygonsDataFrame} object with a buffer in meters of South America coastline.
NULL

#' @title \code{SpatialPolygonsDataFrame} object with a buffer in nautical miles of South America coastline.
#' @name coastlineBuffer_nm
#' @description \code{SpatialPolygonsDataFrame} object with a buffer in nautical miles of South America coastline.
#' @aliases coastlineBuffer_nm
#' @usage coastlineBuffer_nm
#' @format \code{SpatialPolygonsDataFrame} object with a buffer in nautical miles of South America coastline.
NULL

#' @title \code{data.frame} with some random coordinates for running \link[ruisu]{parallelMaps}.
#' @name coord_example
#' @description \code{data.frame} with some random coordinates for running \link[ruisu]{parallelMaps}.
#' @aliases coord_example
#' @usage coord_example
#' @format \code{data.frame} object with three columns: lon, lat and group.
NULL
