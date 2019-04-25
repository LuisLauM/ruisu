#' @title Get harbor information from name
#'
#' @param myHarbor \code{character}. Text with the name of harbor.
#'
#' @details The function will use \code{harborData} as a reference data.
#'
#' @export
#'
#' @examples
#' getHarbor(myHarbor = "T. de mora")
getHarbor <- function(myHarbor){

  myHarbor <- chartr(old = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1",
                     new = "aeiouun", x = tolower(myHarbor))

  output <- NULL
  for(i in seq_along(myHarbor)){

    tempHarbor <- myHarbor[i]

    harborPos <- which(sapply(ruisu::harborData$pattern, grepl, x = tempHarbor))

    if(length(harborPos) > 1){
      warning(paste(tempHarbor, "matched with more than one pattern at pos =", i,
                    "\nFunction will take the first matched value:",
                    ruisu::harborData$name[harborPos[1]]))
    }

    output <- c(output, harborPos[1])
  }

  return(as.list(ruisu::harborData[output,]))
}
