#' Get harbor information  from name
#'
#' @param myHarbor \code{character}. Text with the name of harbor.
#'
#' @export
#'
#' @examples
#' getHarbor(myHarbor = "T. de mora")
getHarbor <- function(myHarbor){
  myHarbor <- chartr(old = "αινσϊόρ", new = "aeiouun", x = tolower(myHarbor))

  allPatterns <- strsplit(x = harborData$pattern, split = "-")

  output <- NULL
  for(i in seq_along(allPatterns)){
    tempPattern <- allPatterns[[i]]

    for(j in seq_along(tempPattern)){
      tempPattern2 <- tempPattern[j]

      if(grepl(pattern = "_", x = tempPattern2)){

        tempPattern2 <- unlist(strsplit(tempPattern2, "_"))

        if(is.element(substr(myHarbor, 1, 1), tempPattern2[1]) && grepl(pattern = tempPattern2[2], x = myHarbor)){
          output <- c(output, i)

          break
        }
      }else if(grepl(pattern = tempPattern[1], x = myHarbor)){
        output <- c(output, i)

        break
      }
    }
  }

  return(as.list(harborData[output,]))
}
