#' @title Curious way to show a message.
#'
#' @param message What message do you want to show?
#' @param delay Specify the time delay between letters.
#' @param nroBombs Number of lights (points) that will be shown.
#' @param dispersion Dispersion of points.
#' @param cex.text Size of text.
#'
#' @export
#'
#' @examples
#' newYear("Happy new year_2017!")
newYear <- function(message, delay = 4, nroBombs = 100, dispersion = 10, cex.text = 3){

  xlim <- c(0, 20)
  ylim <- c(0, 200)

  dev.new()
  plot(1, 1, pch = NA, xlim = xlim, ylim = ylim, xlab = NA, ylab = NA)

  message <- unlist(strsplit(message, split = "_"))
  n <- sum(nchar(message)) + delay

  xAle <- runif(n = n, min = xlim[1] + 5, max = xlim[2] - 5)
  yAle <- runif(n = n, min = ylim[1] + 50, max = ylim[2])

  par(mar = rep(0.1, 4))
  for(i in seq(n)){
    tempX <- seq(from = 0, to = yAle[i], length.out = 5)

    col1 <- rainbow(100)[as.integer(runif(1, 1, 100))]

    for(j in tempX){
      if(i == n){
        break
      }

      if(identical(j, yAle[i])){
        coords <- data.frame(x = rnorm(n = nroBombs, mean = xAle[i], sd = xlim[2]/dispersion),
                             y = rnorm(n = nroBombs, mean = yAle[i], sd = ylim[2]/dispersion))

        points(coords, pch = 8, cex = 0.4, col = rainbow(100))

      }else
        points(xAle[i], j, pch = 19, , col = col1)

      if(i > delay){
        mtext(substr(paste(message, collapse = "\n"), 1, i-delay), side = 1, line = -15, font = 2, cex = cex.text)
      }

      Sys.sleep(0.05)

      plot.new()
      plot.window(xlim = xlim, ylim = ylim)
    }
  }

  coords <- data.frame(x = rnorm(n = nroBombs^1.8, mean = xlim/2, sd = diff(xlim)),
                       y = rnorm(n = nroBombs^1.8, mean = ylim/2, sd = diff(ylim)))

  points(coords, pch = 8, cex = 0.4, col = rainbow(100))

  mtext(paste(message, collapse = "\n"), side = 1, line = -15, font = 2, cex = cex.text)

  return(invisible())
}
