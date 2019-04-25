
# Main fx -----------------------------------------------------------------

#' @title Easy way to get the labels of coordinates from a vector.
#'
#' @param coord Numeric vector of coordinates.
#' @param what Indicate \code{coord} belongs to longitude (\code{what = "lon"}) or
#' latitude (\code{what = "lat"}) values.
#'
#' @return A \code{character} vector, ready to put as coords labels.
#' @export
#'
#' @examples
#' myLongitudes <- seq(-20, -15, 0.5)
#' getCoordsAxes(coord = myLongitudes, what = "lat")
getCoordsAxes <- function(coord, what){

  output <- sapply(coord, getCoordsAxesInternal, what = what)

  return(output)
}


# Auxiliar fx -------------------------------------------------------------

getCoordsAxesInternal <- function(coord, what){

  if(tolower(what) == "lon"){
    if(coord < 0){
      sufix <- "\u00b0 W"
    }else if(coord > 0){
      sufix <- "\u00b0 E"
    }else{
      sufix <- "\u00b0"
    }
  }else if(tolower(what) == "lat"){
    if(coord < 0){
      sufix <- "\u00b0 S"
    }else if(coord > 0){
      sufix <- "\u00b0 N"
    }else{
      sufix <- "\u00b0"
    }
  }else{
    stop("Incorrect value for 'what' parameter.")
  }

  output <- paste0(round(abs(coord), 3), sufix)

  return(output)
}
