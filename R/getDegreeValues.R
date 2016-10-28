#' Title
#'
#' @param x
#' @param what
#'
#' @return
#' @export
#'
#' @examples
getDegreeValues <- function(x, what){
  degreeValue <- suppressWarnings(abs(an(substr(x, 1, 2))))
  minuteValue <- suppressWarnings(abs(an(substr(x, 3, 4))/60))
  secondValue <- suppressWarnings(abs(an(substr(x, 5, 6))/3600))

  output <- (ifelse(!is.na(degreeValue), degreeValue, 0) +
               ifelse(!is.na(minuteValue), minuteValue, 0) +
               ifelse(!is.na(secondValue), secondValue, 0))

  what <- tolower(what)
  type <- tolower(substr(x, nchar(x), nchar(x)))
  if(what == "lon"){
    type <- (type == "e")*2 - 1
  }else if(what == "lat"){
    type <- (type == "n")*2 - 1
  }

  return(output*type)
}
