randomRectangles <- function(nsquares = 500)
{
  nsquares <- as.integer(nsquares)
  
  par(bg = "black")
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), col = "white", pch = ".", xlim = c(0, 1), ylim = c(0, 1))
  
  for(i in seq(nsquares))
  {
    center <- runif(2)
    size <- rbeta(2, 1, 50)
    
    color <- sample(c(seq(9), "A", "B", "C", "D", "E", "F"), 12, replace = T)
    fill <- paste("#", paste(color[seq(6)], collapse = ""), sep = "")
    brdr <- paste("#", paste(color[seq(7, 12)], collapse = ""), sep = "")
    
    rect(center[1] - size[1], center[2] - size[2], center[1] + size[1], center[2] + size[2],
         col = fill, border = brdr, density = NA, lwd = 1.5)
  }
  
  return(invisible())
}