
# Main fx -----------------------------------------------------------------

#' @name digitsum
#' @aliases digitsum
#' @title Returns sum of digit from number
#'
#' @description Recreational function that split digit from number and gets sum of them. If \code{recursive = TRUE}
#' this operation will be done until output consist of one digit.
#'
#' @usage digitsum(x, recursive = FALSE)
#'
#' @param x Numeric vector.
#' @param recursive Would you want to sum until output consist of one digit?
#'
#' @export
#'
#' @examples
#' digitsum(1516)
#'
#' digitsum(1516, recursive = TRUE)
digitsum <- function(x, recursive = FALSE)
{
  if(length(x) == 1){
    x <- digitSum(x = x, recursive = recursive)
  }else{
    x <- sapply(x, digitSum, recursive = recursive)
  }

  return(x)
}


# Auxiliar fx -------------------------------------------------------------

digitSum <- function(x, recursive = FALSE){
  if(recursive){
    while(nchar(x) > 1){
      x <- sum(an(unlist(strsplit(ac(x), ""))), na.rm = TRUE)
    }
  }else{
    x <- sum(an(unlist(strsplit(ac(x), ""))), na.rm = TRUE)
  }
}
