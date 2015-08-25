# Funci?n 'digitSum'
# Devuelve la suma de d?gitos de un n?mero, si el resultado posee m?s de un d?gito,
# vuelve a realizar la suma sobre ?ste.

digitsum <- function(x, recursive = FALSE)
{
  .digitSum <- function(x, recursive = FALSE)
    if(recursive)
      while(nchar(x) > 1)
        x <- sum(as.numeric(unlist(strsplit(as.character(x), ""))), na.rm = TRUE) else
          x <- sum(as.numeric(unlist(strsplit(as.character(x), ""))), na.rm = TRUE)
  
  if(length(x) == 1)
    x <- .digitSum(x = x, recursive = recursive) else
      x <- sapply(x, .digitSum, recursive = recursive)
  
  return(x)
}