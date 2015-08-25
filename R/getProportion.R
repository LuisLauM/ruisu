# Funci?n 'getProportion'
# Toma una tabla y devuelve los valores proporcionales ya sea por fila (group = 1),
# columna(group = 2) o toda la tabla (group > 2)

getProportion <- function(table, group = 3)
{
  table <- as.matrix(table)
  
  if(group == 1)
    return(t(apply(table, group, function(x, ...) x/sum(x, ...), na.rm = TRUE))) else
      if(group == 2)
        return(apply(table, group, function(x, ...) x/sum(x, ...), na.rm = TRUE)) else
          return(table/sum(table, na.rm = TRUE))
}