unfold <- function(marks, freqs)
{
  finalVector <- NULL
  freqs[is.na(freqs)] <- 0
  for(i in seq_along(marks))
    finalVector <- c(finalVector, rep(marks[i], freqs[i]))

  return(finalVector)
}

juv <- function(data, len, juvLim = 12){
  return(sum(data[len < juvLim], na.rm = TRUE))
}


VectorInVector = function(pattern, tag)
{
  lenTag <- length(pattern) - 1

  result <- NULL
  for(i in seq(length(tag) - lenTag))
  {
    if(isTRUE(identical(tag[seq(i, i + lenTag)], pattern)))
      result <- c(result, i)
  }

  return(result)
}

roundUp <- function(x, to = 10)
  to*(x%/%to + as.logical(x%%to))
