#' @title Tools for habitual tasks (and another fancy things) in data analysis
#' @author Wencheng Lau-Medrano, \email{luis.laum@@outlook.com}
#' @description Tools for common tasks as well as some fancy functions
#' @keywords miscellany
"_PACKAGE"

## usethis namespace: start
#' @import mapdata
#' @importFrom abind abind
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert_danger
#' @importFrom cli cli_warn
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr all_of
#' @importFrom dplyr anti_join
#' @importFrom dplyr filter transmute select mutate distinct group_by summarise
#' @importFrom dplyr join_by
#' @importFrom dplyr left_join
#' @importFrom dplyr pull
#' @importFrom fields tim.colors image.plot
#' @importFrom foreach foreach "%dopar%"
#' @importFrom graphics abline arrows axis barplot box grid image lines mtext par plot.new plot.window points polygon text rasterImage locator
#' @importFrom grDevices adjustcolor dev.list dev.new dev.off rainbow png
#' @importFrom lubridate is.Date is.POSIXt year month
#' @importFrom magrittr "%>%"
#' @importFrom maps map
#' @importFrom metR LonLabel LatLabel
#' @importFrom parallel makeCluster stopCluster
#' @importFrom png readPNG
#' @importFrom qgisprocess qgis_extract_output
#' @importFrom qgisprocess qgis_run_algorithm
#' @importFrom rstudioapi initializeProject openProject
#' @importFrom sf st_as_sf st_combine st_cast st_intersects
#' @importFrom sf st_read
#' @importFrom sp dd2dms proj4string over coordinates "coordinates<-" "proj4string<-" plot CRS SpatialPoints
#' @importFrom stats aggregate approx complete.cases median rbeta rnorm runif spline
#' @importFrom stringi stri_trans_general
#' @importFrom stringr fixed
#' @importFrom stringr str_replace_all
#' @importFrom terra distance
#' @importFrom terra rast extract distance
#' @importFrom tibble tibble
#' @importFrom utils read.csv write.csv modifyList
#' @importFrom zoo rollmean
## usethis namespace: end
NULL
