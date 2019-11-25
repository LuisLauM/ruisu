
# Main fx -----------------------------------------------------------------

#' @title Easy way to get the labels of coordinates from a vector.
#'
#' @param coord Numeric vector of coordinates.
#' @param what Indicate \code{coord} belongs to longitude (\code{what = "lon"}) or
#' latitude (\code{what = "lat"}) values.
#' @param dms A \code{character} strings indicating what to show (d: degrees,
#' m: minutes, s: seconds).
#'
#' @return A \code{character} vector, ready to put as coords labels.
#' @export
#'
#' @examples
#' myLongitudes <- seq(-20, -15, 0.5)
#' getCoordsAxes(coord = myLongitudes, what = "lat")
getCoordsAxes <- function(coord, what, dms = "d"){

  output <- sapply(coord, getCoordsAxesInternal, what = what, dms = dms)

  return(output)
}


# Auxiliar fx -------------------------------------------------------------

getCoordsAxesInternal <- function(coord, what, dms){

  if(tolower(what) == "lon"){
    if(coord < 0){
      sufix <- "W"
    }else if(coord > 0){
      sufix <- "E"
    }else{
      sufix <- ""
    }
  }else if(tolower(what) == "lat"){
    if(coord < 0){
      sufix <- "S"
    }else if(coord > 0){
      sufix <- "N"
    }else{
      sufix <- ""
    }
  }else{
    stop("Incorrect value for 'what' parameter.")
  }

  coord <- dd2dms(coord)

  degrees <- coord@deg
  minutes <- coord@min
  seconds <- round(coord@sec, 1)

  degrees <- paste0(degrees, "\u00b0")
  minutes <- paste0(ifelse(minutes < 10, "0", ""), minutes, "'")
  seconds <- paste0(ifelse(seconds < 10, "0", ""), seconds, "\"")

  output <- paste0(ifelse(grepl(x = dms, pattern = "d"), degrees, ""),
                   ifelse(grepl(x = dms, pattern = "m"), minutes, ""),
                   ifelse(grepl(x = dms, pattern = "s"), seconds, ""),
                   sufix)

  return(output)
}
