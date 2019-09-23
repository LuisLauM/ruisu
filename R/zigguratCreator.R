#' Creates a zigurat matrix
#'
#' @param n Number of floors of zigurat (size of matrix).
#'
#' @return A square matrix \eqn{n * n}.
#' @export
#'
#' @examples
#' out <- zigguratCreator(n = 10)
#' image(out)
zigguratCreator <- function(n){
  out <- mat.or.vec(nr = n*2 + 1, nc = n*2 + 1) + 1
  for(i in seq(n - 1)){
    out[seq(i + 1, nrow(out) - i), seq(i + 1, ncol(out) - i)] <- i + 1
  }

  # Correct center point
  xIndex <- ceiling(nrow(out)/2)
  yIndex <- ceiling(ncol(out)/2)
  out[xIndex, yIndex] <- out[xIndex, yIndex] + 1

  return(out)
}
