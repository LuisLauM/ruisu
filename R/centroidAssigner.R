
# Main fx -----------------------------------------------------------------

#' @name centroidAssigner
#' @aliases centroidAssigner
#' @title Returns centroid values from grid codes.
#'
#' @description This function takes a vector of grid codes and returns centroids (center of mass) in lon-lat values.
#'
#' @param code Vector with grid codes.
#' @param what \code{character} indicating whether AIP or Marsden squares are going to be used as reference.
#' @param ... Extra arguments passed to selected method.
#'
#' @details For \code{what = "isoparalitoral"} (default), it allows to use argument \code{old}, for specifying whether
#' to use old AIP shape (\code{AIPShapefile_old}) o the new (\code{AIPShapefile_new}).
#'
#' @export
#'
#' @examples
#' isopCodes <- c(1050, 4043, 17073, 27103)
#' centroidAssigner(code = isopCodes, what = "isoparalitoral")
#'
#' marsdenCodes <- c("A6", "B8", "c12")
#' centroidAssigner(code = marsdenCodes, what = "marsden")
centroidAssigner <- function(code, what = "isoparalitoral", ...){

  output <- switch(what,
                   isoparalitoral = centroidAssigner_isop(code = code, ...),
                   marsden = centroidAssigner_marsden(code = tolower(code), ...),
                   "Incorrect value for 'what'. See help for checking available methods.")

  colnames(output) <- c("code", "lon", "lat")
  rownames(output) <- seq(nrow(output))

  return(output)
}


# Auxiliar fx -------------------------------------------------------------

centroidAssigner_isop <- function(code, old = TRUE){
  isoAreas <- ifelse(isTRUE(old), "AIPData_old", "AIPData_new")

  isoAreas <- get(isoAreas)

  index <- match(code, isoAreas$code)

  output <- data.frame(code, isoAreas[index, c("x", "y")])

  return(output)
}

centroidAssigner_marsden <- function(code){

  code <- gsub(x = code, pattern = "[[:space:]]|[[:punct:]]", replacement = "", perl = TRUE)
  letterCode <- gsub(x = code, pattern = "[0-9]", replacement = "", perl = TRUE)
  numberCode <- an(gsub(x = code, pattern = "[a-z]", replacement = "", perl = TRUE))

  letterCode <- -seq(3.5, 19.5)[sapply(letterCode, function(x) match(letters[1:17], x = x))]
  numberCode <- -(numberCode + 69.5)

  numberCode[is.na(letterCode)] <- NA

  output <- data.frame(toupper(code), numberCode, letterCode, stringsAsFactors = FALSE)

  return(output)
}
