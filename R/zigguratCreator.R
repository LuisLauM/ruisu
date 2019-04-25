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
  out <- mat.or.vec(nr = (n - 1)*2 + 1, nc = (n - 1)*2 + 1) + 1
  for(i in seq(n - 1)){
    out[seq(i + 1, nrow(out) - i), seq(i + 1, ncol(out) - i)] <- i + 1
  }

  return(out)
}
