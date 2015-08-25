splitData <- function(file, iniDates = NA, finDates = NA, listMonths = NULL, save = NA, filename = NULL)
{
  # Funci?n getDates
  # Toma valores de fechas (iniciales y finales) y genera una lista con ellos
  getDates <- function(dates)
  {
    dates <- as.character(dates)
    iniDates <- min(as.Date(dates), na.rm = TRUE)
    finDates <- max(as.Date(dates), na.rm = TRUE)
    
    newIni <- newFin <- NULL
    for(i in seq(as.numeric(readline("?Cu?ntas secciones desea tomar?: "))))
    {
      while(is.null(newIni) | is.null(newFin) |
              if(!is.null(newIni) & !is.null(newFin))
              {
                as.Date(newIni, format = "%d/%m/%Y") < iniDates[1] |
                  as.Date(newFin, format = "%d/%m/%Y") > finDates[1] |
                  as.Date(newIni, format = "%d/%m/%Y") > as.Date(newFin, format = "%d/%m/%Y")
              } else FALSE)
      {
        newIni <- readline(paste("Fecha inicio, secci?n", i, "(DD/MM/AAAA): "))
        newFin <- readline(paste("Fecha final,  secci?n", i, "(DD/MM/AAAA): "))
      }
      
      iniDates <- c(as.character(iniDates), as.character(as.Date(newIni, format = "%d/%m/%Y")))
      finDates <- c(as.character(finDates), as.character(as.Date(newFin, format = "%d/%m/%Y")))
      newIni <- newFin <- NULL
    }
    
    return(list(iniDates = iniDates[-1], finDates = finDates[-1]))
  }
  
  # Cargar data
  data <- read.csv(file, stringsAsFactors = FALSE)
  data <- data[!is.na(data$year) & !is.na(data$month) & !is.na(data$day),]
  
  # Crear columna de fechas
  data$date <- as.Date(with(data, paste(year, month, day, sep = "-")))
  
  # Extraer filas con meses indicados en "listMonths"
  if(!is.null(listMonths))
  {
    newData <- NULL
    for(i in listMonths)
      newData <- rbind(newData, data[data$month == i,])
    
    data <- newData[order(newData$date),]
  }
  
  # Generar objeto de fechas iniciales y finales
  if(is.na(iniDates) | is.na(finDates))
  {
    print(paste("Rango de fechas: Desde ", min(data$date), " hasta ", max(data$date), sep = ""))
    dates <- getDates(data$date) 
  }
  else
    dates <- list(iniDates = as.Date(iniDates, format = "%d/%m/%Y"),
                  finDates = as.Date(finDates, format = "%d/%m/%Y"))
  
  data <- data.frame(index = seq(nrow(data)), data)
  
  # Realizar el split
  newData <- NULL
  for(i in seq_along(dates$iniDates))
    newData <- rbind(newData,
                     data[data$date >= as.Date(dates$iniDates[i]) &
                            data$date <= as.Date(dates$finDates[i]),])
  
  newData <- newData[!duplicated(newData$index),]
  newData <- newData[order(newData$date), -match("index", names(newData))]
  
  # Guardar o mostrar
  if(save)
  {
    if(is.null(filename))
      filename <- readline("Nombre de archivo: ")
    
    write.csv(newData, paste0(filename, ".csv"), row.names = FALSE)
    
    return(invisible())
  }else
    return(newData)
}