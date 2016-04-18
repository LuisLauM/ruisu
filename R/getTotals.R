# Funci?n 'getTotals'
# Agrega los totales por fila y columna

getTotals <- function(table, group = 3){
  table <- as.matrix(table)

  if(group == 2)
    table <- cbind(table, Total = apply(table, 1, sum, na.rm = TRUE)) else
      if(group == 1)
        table <- rbind(table, Total = apply(table, 2, sum, na.rm = TRUE)) else
        {
          table <- cbind(table, Total = apply(table, 1, sum, na.rm = TRUE))
          table <- rbind(table, Total = apply(table, 2, sum, na.rm = TRUE))
        }

  return(table)
}
