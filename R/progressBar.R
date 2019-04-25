#' @title Make a progress bar
#'
#' @param i Step of the iteration.
#' @param n Total number of iteration
#' @param iterator If 'i' belongs to a vector of iterators, define it here.
#'
#' @return The function returns only messages (from \code{cat}).
#' @export
#'
#' @examples
#' n = 1234
#' for(i in seq(n)){
#'   Sys.sleep(0.01)
#'   progressBar(i = i, n = n)
#' }
progressBar <- function(i, n = NULL, iterator = NULL){

  if(is.null(n)){
    if(is.null(iterator)){
      stop("You must indicate a valid value for 'i' or 'iterator'.")
    }

    n <- length(iterator)
    i <- match(i, iterator)
  }

  if(i == 1){
    cat("\n", paste(rep("....|", 10), collapse = ""), "\n")
  }

  a <- i/n*100
  b <- (i - 1)/n*100

  allMarks <- seq(0, 100, 2)

  nBars <- sum(allMarks >= b & allMarks < a)

  cat(paste(rep("=", nBars), collapse = ""))

  if(i == n){
    cat(" Complete!\n")
  }

  return(invisible())
}
