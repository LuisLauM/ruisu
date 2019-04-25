#' @title Extract info from AIP codes
#'
#' @param aipVector Vector of AIP.See Details
#'
#' @details AIP codes must be written in format DDLLPP, where DD is value of Distance to coast (mn/10),
#' LL are values of Latitude (as integer) and PP is position (up or down).
#'
#' @return A \code{data.frame} with variables dc, lat and upDown.
#' @export
#'
#' @examples
#' getAIPInfo(c(30073, 1020, 2010))
getAIPInfo <- function(aipVector){
  ncharAip <- nchar(aipVector)

  dc <- an(substr(aipVector, 1, ifelse(ncharAip == 4, 1, 2)))*10
  lat <- an(substr(aipVector, ifelse(ncharAip == 4, 2, 3), ncharAip - 1))
  upDown <- an(substr(aipVector, ncharAip, ncharAip))

  if(any(!is.element(upDown, c(0, 3)))){
    warning(paste("Values #", paste(which(!is.element(upDown, c(0, 3))), collapse = ", "),
                  "have wrong values for up-down info."))
  }

  return(data.frame(dc, lat, upDown, stringsAsFactors = FALSE))
}
