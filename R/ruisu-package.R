#' @title Tools for habitual tasks (and another fancy things) in data analysis
#' @author Wencheng Lau-Medrano, \email{luis.laum@@outlook.com}
#' @description Tools for common tasks as well as some fancy functions
#' @keywords miscellany
"_PACKAGE"

## usethis namespace: start
#' @importFrom sp dd2dms proj4string over coordinates "coordinates<-" "proj4string<-" plot CRS SpatialPoints
#' @importFrom fields tim.colors image.plot
#' @importFrom zoo rollmean
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#' @importFrom foreach foreach "%dopar%"
#' @importFrom stats aggregate approx complete.cases median rbeta rnorm runif spline
#' @importFrom maps map
#' @importFrom rstudioapi initializeProject openProject
#' @importFrom grDevices adjustcolor dev.list dev.new dev.off rainbow png
#' @importFrom graphics abline arrows axis barplot box grid image lines mtext par plot.new plot.window points polygon text rasterImage
#' @importFrom utils read.csv write.csv modifyList
#' @importFrom lubridate is.Date is.POSIXt year month
#' @importFrom png readPNG
#' @importFrom abind abind
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter transmute select mutate distinct
#' @importFrom metR LonLabel LatLabel
#' @importFrom terra rast extract distance
#' @import mapdata
## usethis namespace: end
NULL
