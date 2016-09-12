#' getCoordsAxes
#'
#' @param coord
#' @param what
#'
#' @return
#' @export
#'
#' @examples
getCoordsAxes <- function(coord, what){

  output <- sapply(coord, .getCoordsAxes, what = what)

  return(output)
}

.getCoordsAxes <- function(coord, what){

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
