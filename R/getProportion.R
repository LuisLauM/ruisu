#' @title Get proportions from table
#'
#' @description Takes a table and gets propotions by row, column or total.
#'
#' @param table \code{data.frame} or \code{matrix} object. All values must be numeric.
#' @param group Specify how the proportions will be extracted. 1 indicates rows, 2 indicates
#' columns and 3 indicates rows and columns (total).
#'
#' @export
#'
#' @examples
#' myTable <- matrix(data = runif(n = 15, min = 0, max = 30), ncol = 3)
#' getProportion(table = myTable)
getProportion <- function(table, group = 3){
  table <- as.matrix(table)

  if(group == 1){
    output <- t(apply(table, group, function(x) x/sum(x, na.rm = TRUE)))
  }else if(group == 2){
    output <- apply(table, group, function(x) x/sum(x, na.rm = TRUE))
  }else if(group == 3){
    output <- table/sum(table, na.rm = TRUE)
  }

  .Deprecated("tableAdd")

  return(output)
}
