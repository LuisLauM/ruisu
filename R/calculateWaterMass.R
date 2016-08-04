#' Title Function to identify water mass from different sources.
#'
#' @param data \code{data.frame} object including SST, SSS, month, depth, longitude and latitude information (View Details).
#' @param method Select method for water mass estimation: Oliveros (2020), Swartzman (2008) and Zuta (1978).
#' @param sst Name of column which contains Sea Surface Temperature info.
#' @param sss Name of column which contains Sea Surface Salinity info.
#' @param lon Name of column which contains Longitude info.
#' @param lat Name of column which contains Latitude info.
#' @param month Name of column which contains Month info.
#' @param depth Name of column which contains Depth info.
#' @param dc Name of column which contains Distance to Coast info.
#' @param asFactors \code{logical}. If \code{TRUE}, output will returned as a \code{factor} object, otherwise as a \code{character} vector.
#'
#' @export
calculateWaterMass <- function(data, method = "oliveros", sst = "sst", sss = "sss", lon = "lon",
                               lat = "lat", month = "month", depth = "depth", dc = "dc",
                               asFactors = TRUE){

  output <- switch(tolower(method),
                   oliveros = .watermass_oliveros(data = data, sst = sst, sss = sss, lon = lon,
                                                  lat = lat, month = month, dc = dc,
                                                  asFactors = asFactors),
                   swartzman = .watermass_swartzman(data = data, sst = sst, sss = sss, lat = lat,
                                                    month = month, dc = dc, asFactors = asFactors),
                   zuta = .watermass_zuta(data = data, sst = sst, sss = sss, lat = lat,
                                          depth = depth, asFactors = asFactors))

  return(output)
}
