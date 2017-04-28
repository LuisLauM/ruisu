#' @import lubridate
#' @importFrom raster intersect
#' @import maps
#' @import rgdal
#' @import RCurl
#' @import stats
#' @import sp
#' @import graphics
#' @import utils
#' @import grDevices
#' @import fields
#'
#' @title Miscellany functions for the IMARPE work
#'
#' @author Wencheng Lau-Medrano, \email{llau@@imarpe.gob.pe}
#' @name ruisu-package
#' @description Package with useful tools and functions for everyday IMARPE work.
#' @aliases ruisu-package ruisu
#' @docType package
#' @keywords miscellany, IMARPE
NULL

#' @title \code{data.frame} with new (corrected) AIP information.
#' @name AIPData_new
#' @description \code{data.frame} with 7 columns:
#' @aliases AIPData_new
#' @docType data
#' @usage AIPData_new
#' @format A \code{data.frame} with
#' @references Instituto del Mar del Peru.
NULL

#' @title \code{data.frame} with old (original) AIP information.
#' @name AIPData_old
#' @description \code{data.frame} with 7 columns:
#' @aliases AIPData_old
#' @docType data
#' @usage AIPData_old
#' @format A \code{data.frame} with
#' @references Instituto del Mar del Peru.
NULL


#' @title Abbreviation for \code{as.numeric}
#'
#' @param x Object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.numeric}.
#'
#' @export
#'
#' @examples
#' as.numeric("02.33")
#' an("02.33")
an <- function(x, ...){
  return(as.numeric(x, ...))
}

#' @title Abbreviation for \code{as.character}
#'
#' @param x object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.character}.
#'
#' @export
#'
#' @examples
#' as.character(TRUE)
#' ac(TRUE)
ac <- function(x, ...){
  return(as.character(x, ...))
}

#' @title Abbreviation for \code{as.numeric(as.character(x))}
#'
#' @param x object to be coerced or tested.
#' @param ... Further arguments passed to \code{as.character}.
#'
#' @export
#'
#' @examples
#' exampleVector <- runif(n = 20, min = 0, max = 100)
#' anc(cut(x = exampleVector, breaks = seq(0, 100, 20), labels = 1:5))
anc <- function(x, ...){
  return(as.numeric(as.character(x, ...)))
}

#' @title Title Function to identify water mass from different sources.
#'
#' @param data \code{data.frame} including SST, SSS, month, depth, longitude and latitude information (see Details).
#' @param method Select method for water mass estimation: Oliveros (2020), Swartzman (2008, default) and Zuta (1978).
#' @param sst Name of column which contains Sea Surface Temperature info.
#' @param sss Name of column which contains Sea Surface Salinity info.
#' @param lon Name of column which contains Longitude info.
#' @param lat Name of column which contains Latitude info.
#' @param month Name of column which contains Month info.
#' @param depth Name of column which contains Depth info.
#' @param dc Name of column which contains Distance to Coast info.
#' @param asFactors \code{logical}. If \code{TRUE}, output will returned as a \code{factor} object, otherwise as
#' a \code{character} vector.
#'
#' @details Variables in \code{data} must be as next: SST ( C), SSS (PSU), depth (m), longitude ( W) and
#' latitude ( E).
#'
#' @export
calculateWaterMass <- function(data, method = "swartzman", sst = "sst", sss = "sss", lon = "lon",
                               lat = "lat", month = "month", depth = "depth", dc = "dc",
                               asFactors = TRUE){

  output <- switch(tolower(method),
                   oliveros = .watermass_oliveros(data = data, sst = sst, sss = sss, lon = lon,
                                                  lat = lat, month = month, dc = dc,
                                                  asFactors = asFactors),
                   swartzman = .watermass_swartzman(data = data, sst = sst, sss = sss, lat = lat,
                                                    month = month, dc = dc, asFactors = asFactors),
                   zuta = .watermass_zuta(data = data, sst = sst, sss = sss, lat = lat,
                                          depth = depth, asFactors = asFactors))

  return(output)
}

#' @name centroidAssigner
#' @aliases centroidAssigner
#' @title Returns centroid values from Isoparalitoral-area codes.
#'
#' @description This function takes a vector of AIP codes and returns centroids (center of mass) in lon-lat values.
#'
#' @usage centroidAssigner(isoCode, old = TRUE)
#'
#' @param isoCode Vector with AIP codes.
#' @param old \code{logical}. Specifying whether to use old AIP shape (\code{AIPShapefile_old}) o the new (\code{AIPShapefile_new}).
#'
#' @export
#'
#' @examples
#' areaCodes <- c(1050, 4043, 17073, 27103)
#' centroidAssigner(isoCode = areaCodes)
centroidAssigner <- function(isoCode, old = TRUE)
{
  isoAreas <- ifelse(isTRUE(old), "AIPData_old", "AIPData_new")

  isoAreas <- get(isoAreas)

  index <- match(isoCode, isoAreas$code)
  output <- data.frame(isoCode, isoAreas[index, c("x", "y")])
  colnames(output) <- c("area", "lon", "lat")
  rownames(output) <- seq(nrow(output))

  return(output)
}

#' @title Laboratory Counter simulator
#'
#' @param spList (Optional) Path of file to re start counting. If \code{NULL}, new counting will start. This file
#' requires columns "Especie", "Caracter", "Conteo".
#' @param filepath File name where the output file will be saved.
#' @param reset Would you want to continue with the previous count (\code{FALSE}) or reset (put in zeros)?
#'
#' @details This function uses next commands:
#' \itemize{
#' \item{\strong{new}{Defines a new species that will be included in the count.}}
#' \item{\strong{correct}{Allows to make some basic operation in count values. E.g. +1, -2.}}
#' \item{\strong{end}{Ends the count and save the file.}}
#' \item{{\strong{help}}{Shows commands' help in console.}}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' counterZOO(spList = "myFolder/myTable.csv")
#' }
counterZOO <- function(spList = NULL, filepath = "conteo.csv", reset = TRUE){

  cat("\n--------------------------\n")
  cat("-------- COMANDOS --------\n")
  cat("--------------------------\n")

  cat("\n new     : Para definir una especie nueva en la tabla de conteo.\n")
  cat("\n correct : Para corregir o realizar una operacion sobre alguno\n")
  cat("\n           de los valores de conteo.\n")
  cat("\n end     : Finalizar conteo (mostrar y guardar tabla de conteo).\n")
  cat("\n help    : Mostrar lista de comandos.\n")

  # Definir funcion para mostrar la tabla de conteo
  showPlots <- function(spList){

    # Eliminar plots anteriores (si hubiera)
    if(!is.null(dev.list()))
      dev.off()

    # Definir parametros de ploteo
    par(mfrow = c(1, 1), mar = rep(0, 4), xaxs = "i", yaxs = "i", oma = rep(0, 4))

    # Definir el maximo numero de filas
    maxXlim <- ifelse(test = nrow(spList) <= 9,
                      yes = 10, no = 1 + nrow(spList))

    # Generar las lineas de la tabla de conteo
    plot.new()
    plot.window(xlim = c(0, 10), ylim = c(maxXlim, 0))
    abline(v = c(6, 8), col = "gray50")
    grid(nx = 0, ny = maxXlim, col = "gray50")
    box()

    # Mostrar los header de la tabla
    text(x = 3, y = 0.5, labels = "Especie")
    text(x = 7, y = 0.5, labels = "Caracter")
    text(x = 9, y = 0.5, labels = "Conteo")

    # Mostrar cada fila (Nombre, caracter asociado y conteo)
    for(i in 1:nrow(spList)){
      text(x = 3, y = 0.5 + i, labels = spList[i, 1])
      text(x = 7, y = 0.5 + i, labels = spList[i, 2])
      text(x = 9, y = 0.5 + i, labels = spList[i, 3])
    }
  }

  command <- "start"

  # Si se ha definido una tabla, cargarla
  if(!is.null(spList)){
    # Leer tabla
    spList <- as.matrix(read.csv(spList, stringsAsFactors = FALSE))

    # Mantener los valores en la tabla o poner el conteo en cero (reset)
    if(reset)
      spList[1:nrow(spList), 3] <- "0"

    # Mostrar tabla
    showPlots(spList)

    # Definir un valor diferente a cero para conteo
    allCounter <- 1
  }else{
    allCounter <- 0
  }

  # Bucle de conteo y peticion de datos
  while(is.na(command) || command != "end"){
    # Mensaje si el conteo esta en cero (i.e. no hay NINGUNA especie)
    if(allCounter > 0)
      cat("\nIngrese caracter o comando: ")

    # Si hay al menos una especie definida, realizar la peticion de caracter para contar
    # o el comando a ejecutar
    command <- ifelse(test = allCounter == 0 & is.null(spList),
                      yes = "new",
                      no = tolower(scan(what = "character", nmax = 1, quiet = TRUE)))

    if(command == paste0(letters[c(24, 9, 13)], collapse = ""))
      cat("\nHey ", paste(paste0(LETTERS[c(19, 8, 1, 18, 11, 25)], collapse = ""),
                          paste0(LETTERS[c(24, 9, 13)], collapse = "")),
          "! :", LETTERS[4], "\n", sep = "")

    # Si no se ha colocado ningun caracter, volver a realizar la peticion
    if(length(command) == 0 || is.na(command)){
      cat("\nCaracter o comando incorrecto.")
      next
    }

    # Si se utilizo el comando 'new', realizar las peticiones correspondientes
    if(command == "new"){
      # Peticion del nombre de la especie nueva
      cat("\nIngrese el nombre de la especie NUEVA: ")
      newSp <- scan(what = "character", nmax = 1, quiet = TRUE)

      # Peticion del caracter asociado a la especie nueva
      cat("\nIngrese el caracter ASOCIADO a la especie NUEVA: ")

      # Si el caracter indicado ya ha sido asignado a otra especie, volver a realizar
      # la peticion
      charExist <- TRUE
      while(charExist){
        newChar <- tolower(scan(what = "character", nmax = 1, quiet = TRUE))

        if(newChar %in% spList[,2]){
          cat("\nEl caracter asociado YA EXISTE, elija otro.")
          charExist <- TRUE
        }else
          charExist <- FALSE
      }

      # Registrar lo digitado en la tabla de conteo
      spList <- rbind(spList, c(newSp, newChar, "0"))

      # Incremento del contador interno
      allCounter <- allCounter + 1
      next
    }

    if(command == "help"){
      cat("\n --------------------------\n")
      cat("-------- COMANDOS --------\n")
      cat("--------------------------\n")

      cat("\n new     : Para definir una especie nueva en la tabla de conteo.\n")
      cat("\n correct : Para corregir o realizar una operacion sobre alguno\n")
      cat("\n           de los valores de conteo.\n")
      cat("\n end     : Finalizar conteo (mostrar y guardar tabla de conteo).\n")

      next
    }

    # Ubicar la posicion de la fila segun el caracter asociado digitado
    pos <- match(command, as.character(spList[,2]))

    # Procedimientos para el comando 'corr' (correccion)
    if(grepl(x = command, pattern = "corr")){
      # Peticion del caracter a corregir
      cat("\nIngrese el caracter a CORREGIR: ")
      corrChar <- tolower(scan(what = "character", nmax = 1, quiet = TRUE))

      # Indicar la correccion (adicion, sustracciom, multiplicacion o division)
      cat("\nIngrese la CORRECCION (e.g. +1, -2, *3, /4): ")
      correction <- tolower(scan(what = "character", nmax = 1, quiet = TRUE))

      # Ubicar la posicion de la fila segun el caracter asociado digitado
      pos <- match(corrChar, as.character(spList[,2]))

      # Extraer valores a corregir
      a <- as.numeric(spList[pos, 3])
      b <- as.numeric(substr(correction, 2, nchar(correction)))

      # Realiza la correccion
      spList[pos, 3] <- switch(substr(correction, 1, 1),
                               "+" = as.character(a + b),
                               "-" = as.character(a - b),
                               "*" = as.character(a * b),
                               "/" = as.character(a / b))

      # Mostrar tabla
      showPlots(spList)

      next
    }

    # Si el caracter ingresado no coincide con ninguno en la tabla, volver a
    # realizar la peticion
    if(is.na(pos)){
      # Si el caracter ingresado es 'end', finalizar el conteo
      if(command == "end")
        break else{
          cat("\nCaracter INCORRECTO!")
          next
        }
    }

    # Si el caracter asociado es valido, incrementar el conteo de esa especie
    spList[pos, 3] <- as.character(as.numeric(spList[pos, 3]) + 1)

    # Mostrar tabla
    showPlots(spList)

    # Incremento del contador interno
    allCounter <- allCounter + 1
  }

  # Armar tabla de conteo como data.frame
  spList <- data.frame(Especie = spList[,1],
                       Caracter = spList[,2],
                       Conteo = spList[,3])

  # Mensaje final
  cat("\n Conteo finalizado! \n")

  # Guardar un csv con la tabla de conteo
  if(!is.null(filepath) && dir.exists(filepath)){
    write.csv(spList, file = filepath, quote = FALSE, row.names = FALSE)

    cat("\n El archivo de conteo se ha guardado en: ", file.path(getwd(), filepath), "\n\n")
  }

  return(spList)
}

#' @name digitsum
#' @aliases digitsum
#' @title Returns sum of digit from number
#'
#' @description Recreational function that split digit from number and gets sum of them. If \code{recursive = TRUE}
#' this operation will be done until output consist of one digit.
#'
#' @usage digitsum(x, recursive = FALSE)
#'
#' @param x Numeric vector.
#' @param recursive Would you want to sum until output consist of one digit?
#'
#' @export
#'
#' @examples
#' digitsum(1516)
#'
#' digitsum(1516, recursive = TRUE)
digitsum <- function(x, recursive = FALSE)
{
  if(length(x) == 1){
    x <- .digitSum(x = x, recursive = recursive)
  }else{
    x <- sapply(x, .digitSum, recursive = recursive)
  }

  return(x)
}

#' @title Easy way to get the labels of coordinates from a vector.
#'
#' @param coord Numeric vector of coordinates.
#' @param what Indicate \code{coord} belongs to longitude (\code{what = "lon"}) or
#' latitude (\code{what = "lat"}) values.
#'
#' @return A \code{character} vector, ready to put as coords labels.
#' @export
#'
#' @examples
#' myLongitudes <- seq(-20, -15, 0.5)
#' getCoordsAxes(coord = myLongitudes, what = "lat")
getCoordsAxes <- function(coord, what){

  output <- sapply(coord, .getCoordsAxes, what = what)

  return(output)
}

#' @title Get harbor information from name
#'
#' @param myHarbor \code{character}. Text with the name of harbor.
#'
#' @details The function will use \code{harborData} as a reference data.
#'
#' @export
#'
#' @examples
#' getHarbor(myHarbor = "T. de mora")
getHarbor <- function(myHarbor){

  myHarbor <- chartr(old = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1",
                     new = "aeiouun", x = tolower(myHarbor))

  output <- NULL
  for(i in seq_along(myHarbor)){

    tempHarbor <- myHarbor[i]

    harborPos <- which(sapply(harborData$pattern, grepl, x = tempHarbor))

    if(length(harborPos) > 1){
      warning(paste(tempHarbor, "matched with more than one pattern at pos =", i,
                    "\nFunction will take the first matched value:", harborData$name[harborPos[1]]))
    }

    output <- c(output, harborPos[1])
  }

  return(as.list(harborData[output,]))
}

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
getProportion <- function(table, group = 3)
{
  table <- as.matrix(table)

  if(group == 1){
    output <- t(apply(table, group, function(x) x/sum(x, na.rm = TRUE)))
  }else if(group == 2){
    output <- apply(table, group, function(x) x/sum(x, na.rm = TRUE))
  }else if(group == 3){
    output <- table/sum(table, na.rm = TRUE)
  }

  return(output)
}

#' @title Get satellital information for oceanographic variables
#' @description This function uses date and space range and get maps for Sea Surface Temperature (SST),
#' Sea Surface Salinity (SSS), Chlorophyll-a (Chl), Topography (topo), Sea level (sealevel) and SODA
#' estimations for Temperature (sst_soda) and Salinity (sss_soda) by depth (\code{atDepth} parameter).
#'
#' @param initialDate Initial date.
#' @param finalDate Final date.
#' @param timeRes Time resolution for maps: Monthly ('month') o daily ('day').
#' @param what What variable do you want to download? See details.
#' @param dateList List with dates for downloading..
#' @param lonRange Range of longitude. If \code{NULL}, it will take values from 85 W to 90 W (Peru).
#' @param latRange Range of latitude. If \code{NULL}, it will take values from 2 S to 20 S (Peru).
#' @param outputFormat What extension would you like to use for download? (nc, csv, png).
#' @param outputDir Folder for downloading.
#' @param atDepth Depth (usually meters) of layer to downloading.
#' @param showURL Do you want to show the URL each time?
#'
#' @details Function will use data bases of ERDDAP website
#' \link{https://coastwatch.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000}.
#'
#' This function will use the dates values to choose from what source the values will be taken.
#'
#' @export
#'
#' @examples
#' getSatellitalMaps(initialDate = "2014-3-26", finalDate = "2014-4-17", what = "sst", timeRes = "day",
#' lonRange = c(-90, -85), latRange = c(-5, 0), outputFormat = "png",
#' outputDir = "../")
getSatellitalMaps <- function(initialDate = NULL, finalDate = NULL, timeRes = "month", what = "SST", atDepth = NULL,
                              dateList = NULL, lonRange = c(-85, -70), latRange = c(-20, -2), outputFormat = "png",
                              outputDir = ".", showURL = FALSE){

  getSatellitalMaps_internal(initialDate, finalDate, timeRes, what, atDepth, dateList, lonRange, latRange, outputFormat,
                             outputDir, showURL)

  cat(paste0("\n Download finished! There were ", length(errorList), " problems. \n"))


  # Final output will be a list with failed URLs
  return(if(is.null(errorList)) invisible() else errorList)
}

#' @title Title Function to get AIP code from lon-lat information.
#'
#' @param dataPoints \code{data.frame} which has Longitude and Latitude information.
#' @param colLon Name of column which contains Longitude info.
#' @param colLat Name of column which contains Latitude info.
#' @param old \code{logical}. Do you prefer to use old or new AIP data?
#'
#' @return A numeric vector indicating the AIP values for each coord.
#'
#' @export
#'
#' @examples
#' exampleCoords <- data.frame(lon = runif(n = 10, min = -80, max = -78),
#'                             lat = runif(n = 10, min = -14, max = -12))
#'
#' isopArea.assigner(dataPoints = exampleCoords)
isopArea.assigner <- function(dataPoints, colLon = "lon", colLat = "lat", old = TRUE){

  referenceShapefile <- get(ifelse(test = isTRUE(old), yes = "AIPShapefile_old", no = "AIPShapefile_new"))

  dataPoints <- switch(class(dataPoints),
                       "data.frame" = as.data.frame(dataPoints[,c(colLon, colLat)]),
                       "numeric" = data.frame(lon = dataPoints[1], lat = dataPoints[2], stringsAsFactors = FALSE))

  output <- rep(NA, nrow(dataPoints))

  index <- complete.cases(dataPoints)
  dataPoints <- dataPoints[index,]

  coordinates(dataPoints) <- dataPoints

  proj4string(dataPoints) <- proj4string(referenceShapefile)

  dataPoints <- over(x = dataPoints, y = referenceShapefile)

  output[index] <- dataPoints$code

  return(output)
}

#' @title Determine the proximity to coast of points
#'
#' @param dataPoints \code{data.frame} which has Longitude and Latitude information.
#' @param colLon Name of column which contains Longitude info.
#' @param colLat Name of column which contains Latitude info.
#' @param units Which units do you want to use for measuring the proximity to coast.
#' @param distance Maximum distance (in degrees) to make the searching.
#'
#' @return A \code{logical} vector indicating whether the coordinates belong to the area between
#' the coast line and a buffer of the selected distance.
#' @export
#'
#' @examples
#' isNearCoast(c(-81.191, -5.211), distance = 20)
#' isNearCoast(c(-81.191, -5.211), distance = 10)
isNearCoast <- function(dataPoints, colLon = "lon", colLat = "lat", units = "m", distance = 20){

  # Check values for aguments
  if(tolower(units) == "m"){
    posibleValues <- c(10, 20, 30, 50, 100, 150, 200, 300)
  }else if(tolower(units) == "nm"){
    posibleValues <- seq(0.2, 2, 0.2)
  }else{
    stop("'units' must be m (meters) or nm (nautical miles).")
  }

  if(length(distance) != 1 || !is.numeric(an(distance)) || !is.element(an(distance), posibleValues)){
    stop("'distance' must be numeric, length 1 and values ", paste(posibleValues, collapse = ", "), ".")
  }

  # Select the reference shapefile
  referenceShapefile <- get(ifelse(test = tolower(units) == "m", yes = "coastlineBuffer_m", no = "coastlineBuffer_nm"))

  # If the input data is a vector, rearrange in a data.frame
  dataPoints <- switch(class(dataPoints),
                       "data.frame" = as.data.frame(dataPoints[,c(colLon, colLat)]),
                       "numeric" = data.frame(lon = dataPoints[1], lat = dataPoints[2], stringsAsFactors = FALSE))

  # Select just points with valid values for both lon and lat
  index <- complete.cases(dataPoints)
  dataPoints <- dataPoints[index,]

  # Define projection
  coordinates(dataPoints) <- dataPoints
  proj4string(dataPoints) <- proj4string(referenceShapefile)

  # Make the intersection
  dataPoints <- over(x = dataPoints, y = referenceShapefile)

  # Build the output vector (logical)
  output <- rep(FALSE, nrow(dataPoints))
  output[index & dataPoints$distance <= distance] <- TRUE

  return(output)
}

#' @title  Given a frequency table size, it generates a vertical array of graphics for each specified
#' category (months, latitudes, years, etc.).
#'
#' @param file1 Indicates the table from the main length distribution will be drawn. See details.
#' @param file2 Indicates the table from the secondary length distribution will be drawn. See details.
#' @param dataFactor Factor that multiplies matrices 1 and 2.
#' @param newPlot If \code{TRUE}, the plot will be opened in a new window (using \code{x11} command).
#' @param profile \code{character}. Indicates the profile (as an species' name) used to predine values of
#' xlim, juvLine, xInterval and xlab.
#' @param xlim Limits of x axis.
#' @param xInterval Number of intervals for x axis labels.
#' @param ylim Limits of y axis.
#' @param yInterval Number of intervals for y axis labels.
#' @param ylimList List of xlim value for each plot.
#' @param yIntervalList List of yInterval value for each plot.
#' @param ltys1 lty (see \code{\link{par}}) parameter for main length distribution lines.
#' @param lwds1 lwd (see \code{\link{par}}) parameter for main length distribution lines.
#' @param col1 col (see \code{\link{par}}) parameter for main length distribution lines.
#' @param ltys2 lty (see \code{\link{par}}) parameter for secondary length distribution lines.
#' @param lwds2 lwd (see \code{\link{par}}) parameter for secondary length distribution lines.
#' @param col2 col (see \code{\link{par}}) parameter for secondary length distribution lines.
#' @param juvLimit Limit length for juveniles.
#' @param juvLty lty (see \code{\link{par}}) parameter for juvenile line.
#' @param juvLwd lwd (see \code{\link{par}}) parameter for juvenile line.
#' @param juvCol col (see \code{\link{par}}) parameter for juvenile line.
#' @param showJuv1 Do you want to show the percentage of juveniles 1 as text?
#' @param juvLine1 line (see \code{\link{mtext}}) parameter for juvenile 1 line.
#' @param juvCex1 Size for juveniles 1 text.
#' @param juvLabel1 Text before juvenile 1 value.
#' @param showJuv2 Do you want to show the percentage of juveniles 2 as text?
#' @param juvLine2 line (see \code{\link{mtext}}) parameter for juvenile 2 line.
#' @param juvCex2 Size for juveniles 2 text.
#' @param juvLabel2 Text before juvenile 2 value.
#' @param juvAdj adj (see \code{\link{mtext}}) parameter for juvenile text.
#' @param juvRound Number of decimals places used to show juveniles' values.
#' @param juvSide side (see \code{\link{mtext}}) parameter for juvenile text.
#' @param juvTextCol col (see \code{\link{par}}) parameter for juvenile text. If \code{NULL}
#' (default), function will use \code{col1} and \code{col2} values.
#' @param cex.axis cex.axis (see \code{\link{par}}) parameter for x and y axes.
#' @param cex.lab cex.lab (see \code{\link{par}}) parameter for x and y axes.
#' @param ylab_line line (see \code{\link{mtext}}) parameter for ylab text.
#' @param namesAdj adj (see \code{\link{mtext}}) parameter for categories (column names) text.
#' @param title_text Text for title. If not \code{NULL}, default \code{oma = c(5, 5, 3, 3)}
#' @param title_line line (see \code{\link{mtext}}) parameter for title text.
#' @param title_cex Size for title text.
#' @param title_col col (see \code{\link{par}}) parameter for title text.
#' @param title_font font (see \code{\link{par}}) parameter for title text.
#' @param smooth \code{logical} Do you want to smooth the length distribution lines?
#' @param oma oma (see \code{\link{par}}) parameter for plot. Default \code{c(5, 5, 1, 3)}.
#' @param xlab Label for x axis.
#' @param ylab Label for y axis.
#' @param noDataLabel \code{character}. If there is no data in a column, the function will show
#' what this parameter indicates.
#' @param ylabFactor \code{numeric}. Value that multiplies all values in both main and secondary matrices.
#' @param relative \code{logical} Do you want to plot relatives or absolutes values?
#'
#' @details This function takes a length frequency data and returns an stacking plot of that frequencies.
#' smooth: logical. If TRUE, function uses an spline function to plot by categories.
#' This function uses a data with categories at col and length at rows.
#'
#'
#' @export
lengthFrequencyPlot <- function(file1, file2 = NULL, dataFactor = 1, newPlot = FALSE,
                                profile = NULL, xlim = NULL, xInterval = 1, ylim = c(0, 50), yInterval = NULL,
                                ylimList = NULL, yIntervalList = NULL,
                                ltys1 = "solid", lwds1 = "1", col1 = "black", ltys2 = "solid", lwds2 = "1", col2 = "blue",
                                juvLimit = NULL, juvLty = "dotted", juvLwd = 1, juvCol = "red",
                                showJuv1 = TRUE, juvLine1 = -2, juvCex1 = 1, juvLabel1 = "juveniles_1 = ",
                                showJuv2 = TRUE, juvLine2 = -4, juvCex2 = 1, juvLabel2 = "juveniles_2 = ",
                                juvAdj = 0.99, juvRound = 0, juvSide = 3, juvTextCol1 = NULL, juvTextCol2 = NULL,
                                cex.axis = 1, cex.lab = 1, ylab_line = 3, namesAdj = 0.01,
                                title_text = NULL, title_line = 2, title_cex = NULL, title_col = "black", title_font = 1,
                                smooth = FALSE, oma = NULL, xlab = NULL, ylab = "Frecuencia (%)",
                                noDataLabel = "Sin datos", ylabFactor = 1, relative = TRUE){

  if(is.element(class(file1), c("data.frame", "matrix"))){

    message("First column will be taken as the values of length.")
    file1Names <- list(an(file1[,1]), colnames(file1)[-1])
    file1 <- data.frame(file1[,-1], stringsAsFactors = FALSE)
    dimnames(file1) <- file1Names
  }else if(length(file1) == 1 && is.character(file1) && file.exists(file1)){
    file1 <- read.csv(file = file1, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
  }else{
    stop("Incorrect value for file1. It must be a 'matrix', 'data.frame' or it must indicate the file name of a length matrix.")
  }

  file1 <- file1*dataFactor

  if(!is.null(file2)){
    if(is.element(class(file2), c("data.frame", "matrix"))){
      file2Names <- list(an(file2[,1]), colnames(file2)[-1])
      file2 <- data.frame(file2[,-1], stringsAsFactors = FALSE)
      dimnames(file2) <- file1Names
    }else{
      file2 <- read.csv(file = file2, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }

    if(ncol(file1) != ncol(file2)){
      stop("'file1' and 'file2' must have the same number of columns.")
    }

    if(isTRUE(relative)){
      file2 <- sweep(as.matrix(file2), 2, colSums(file2, na.rm = TRUE), "/")*100
    }

    # file2[file2 <= 0 | is.na(file2)] <- 0

    file2 <- file2*dataFactor

    ltys2 <- rep(ltys2, length.out = ncol(file2))
    lwds2 <- rep(lwds2, length.out = ncol(file2))
    col2 <- rep(col2, length.out = ncol(file2))
  }

  if(!is.null(profile)){

    index <- match(tolower(profile), rownames(speciesInfo))

    if(is.null(xlim)){
      xlim <- an(speciesInfo[index, c("Lmin", "Lmax")])
    }

    if(is.null(juvLimit)){
      juvLimit <- an(speciesInfo$juvenile[index])
    }

    if(is.null(xInterval)){
      xInterval <- an(speciesInfo$bin[index])
    }

    if(is.null(xlab)){
      xlab <- paste0("Longitud ", speciesInfo$lengthType[index], " (", speciesInfo$unit[index], ")")
    }

  }

  file1[file1 <= 0 | is.na(file1)] <- 0

  if(isTRUE(relative)){
    file1 <- sweep(as.matrix(file1), 2, colSums(file1, na.rm = TRUE), "/")*100
  }

  if(is.null(xlim)){
    xlim <- range(an(rownames(file1)))
  }

  ltys1 <- rep(ltys1, length.out = ncol(file1))
  lwds1 <- rep(lwds1, length.out = ncol(file1))
  col1 <- rep(col1, length.out = ncol(file1))

  file1[file1 <= 0 | is.na(file1)] <- 0

  if(isTRUE(newPlot)){
    dev.new()
  }

  if(is.null(oma)){
    if(is.null(title_text)){
      oma <- c(5, 5, 1, 3)
    }else{
      oma <- c(5, 5, 3, 3)
    }
  }

  par(mfrow = c(ncol(file1), 1), mar = c(rep(0, 4)), oma = oma, xaxs = "i", yaxs = "i")

  for(i in seq(ncol(file1))){

    if(!is.null(ylimList)){
      ylim <- ylimList[[i]]
    }

    plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)

    if(isTRUE(smooth)){
      allLengths <- spline(x = an(rownames(file1)), y = file1[,i], method = "natural", n = 1e3)
    }else{
      allLengths <- list(x = an(rownames(file1)), y = file1[,i])
    }

    allLengths$y[allLengths$y < 0.001] <- NA




    # Add juveniles 1 text
    if(sum(!is.na(allLengths$y)) < 2){
      mtext(text = noDataLabel, side = juvSide, adj = juvAdj, line = juvLine1, cex = juvCex1)
    }else if(!is.null(juvLimit)){
      if(isTRUE(showJuv1)){
        if(is.null(juvTextCol1)){
          juvTextCol1 <- col1
        }else{
          juvTextCol1 <- rep(juvTextCol1, len = length(col1))
        }

        juvValue <- sum(allLengths$y[allLengths$x < juvLimit], na.rm = TRUE)/sum(allLengths$y, na.rm = TRUE)
        mtext(text = paste0(juvLabel1, round(juvValue*100, juvRound), " %"), cex = juvCex1,
              col = juvTextCol1[i], side = juvSide, line = juvLine1, adj = juvAdj)
      }

      abline(v = juvLimit, lty = juvLty, lwd = juvLwd, col = juvCol)
    }

    lines(allLengths, lty = ltys1[i], lwd = lwds1[i], col = col1[i])

    if(!is.null(file2)){
      if(isTRUE(smooth)){
        allLengths <- spline(x = an(rownames(file2)), y = file2[,i], method = "natural", n = 1e3)
      }else{
        allLengths <- list(x = an(rownames(file2)), y = file2[,i])
      }

      # Add juveniles 2 text
      if(sum(!is.na(allLengths$y)) < 2){
        mtext(text = noDataLabel, side = juvSide, adj = juvAdj, line = juvLine2, cex = juvCex2)
      }else if(!is.null(juvLimit)){
        if(isTRUE(showJuv2)){
          if(is.null(juvTextCol2)){
            juvTextCol2 <- col2
          }else{
            juvTextCol2 <- rep(juvTextCol2, len = length(col2))
          }

          juvValue <- sum(allLengths$y[allLengths$x < juvLimit], na.rm = TRUE)/sum(allLengths$y, na.rm = TRUE)
          mtext(text = paste0(juvLabel2, round(juvValue*100, juvRound), " %"), cex = juvCex2,
                col = juvTextCol2[i], side = juvSide, line = juvLine2, adj = juvAdj)
        }
      }

      allLengths$y[allLengths$y < 0.001] <- NA

      lines(allLengths, lty = ltys2[i], lwd = lwds2[i], col = col2[i])
    }

    if(is.null(yInterval)){
      yInterval <- diff(ylim)/5
    }

    if(!is.null(yIntervalList)){
      yInterval <- yIntervalList[[i]]
    }

    if(i %% 2 > 0){
      axis(side = 2, at = seq(ylim[1], ylim[2], by = yInterval), las = 2, cex.axis = cex.axis,
           labels = seq(ylim[1], ylim[2], by = yInterval)/ylabFactor)
    }else{
      axis(side = 4, at = seq(ylim[1], ylim[2], by = yInterval), las = 2, cex.axis = cex.axis,
           labels = seq(ylim[1], ylim[2], by = yInterval)/ylabFactor)
    }

    if(i == ncol(file1)){
      if(!is.null(profile) && profile == "anchoveta"){
        axis(side = 1, at = seq(xlim[1], xlim[2], by = xInterval), labels = NA, tcl = -0.25)
        axis(side = 1, at = seq(xlim[1], xlim[2], by = 1), cex.axis = cex.axis)
      }else{
        axis(side = 1, at = seq(xlim[1], xlim[2], by = xInterval), cex.axis = cex.axis)
      }
    }

    mtext(text = colnames(file1)[i], side = 3, adj = namesAdj, line = -2)

    box()
  }

  if(!is.null(title_text)){
    mtext(text = title_text, side = 3, line = title_line, outer = TRUE, cex = title_cex,
          col = title_col, font = title_font)
  }

  mtext(text = ifelse(is.null(xlab), "Longitud", xlab), side = 1, line = 3, outer = TRUE,
        cex = cex.lab)
  mtext(text = ylab, side = 2, line = ylab_line, outer = TRUE, cex = cex.lab)

  return(invisible())
}

#' @title Get the minumim distance to coast
#'
#' @param data \code{data.frame} with coords that will be used to calculate min distance to coast line.
#' @param colLon Name or position of column for longitude. As default, it will be \code{lon}.
#' @param colLat Name or position of column for latitude As default, it will be \code{lat}.
#' @param countryFilter Select the country for make comparation
#' @param unit Define the unit for outputs: nm (nautical miles), kilometers (km), m (meters).
#'
#' @return It returns a list with both the minimum distance to coast line and the point where this
#' distance is reached.
#'
#' @export
#'
#' @examples
#'
#' n <- 100
#'
#' allData <- data.frame(lon = runif(n, -80, -70), lat = runif(n, -18, -2),
#'                       stringsAsFactors = FALSE)
#'
#' allData <- allData[!is.na(isopArea.assigner(allData)),]
#'
#' minValues <- minDistanceToCoast(allData)
#'
#' xlim <- c(-85, -70)
#' ylim <- c(-20, -2)
#'
#' dev.new()
#' par(mar = c(2, 3, 1, 1), xaxs = "i", yaxs = "i")
#' plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)
#'
#' points(allData$lon, allData$lat, pch = 16, cex = 0.5, xlim = c(-85, -70), ylim = c(-20, -2))
#' lines(coastline$lon, coastline$lat)
#'
#'
#' for(i in 1:nrow(minValues$position)){
#'   lines(c(allData$lon[i], minValues$position$lon[i]), c(allData$lat[i], minValues$position$lat[i]),
#'         lty = "dotted", col = "red")
#' }
#'
#' axis(side = 1, at = seq(xlim[1], xlim[2], length.out = 4),
#'      labels = getCoordsAxes(seq(xlim[1], xlim[2], length.out = 4), "lon"))
#' axis(side = 2, at = seq(ylim[1], ylim[2], length.out = 10),
#'      labels = getCoordsAxes(seq(ylim[1], ylim[2], length.out = 10), "lat"), las = 2)
#' box()
minDistanceToCoast <- function(data, colLon = "lon", colLat = "lat", countryFilter = "peru", unit = "nm"){

  pointsRange <- range(data[,colLat], na.rm = TRUE)

  if(!is.null(countryFilter)){
    countryFilter <- chartr(old = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1",
                            new = "aeiouun", x = tolower(countryFilter))

    index <- tolower(coastline$country) == tolower(countryFilter)
  }else{
    index <- range(data[,colLat]) + c(-20, 20)
    index <- coastline$lat > index[1] & coastline$lat < index[2]
  }

  refLines <- coastline[index,]
  allDistances <- spDists(x = as.matrix(coastline[index, c("lon", "lat")]),
                          y = as.matrix(data[,c(colLon, colLat)]), longlat = TRUE)
  minDistancesValue <- apply(allDistances, 2, min)
  minDistancesPosition <- refLines[,c("lon", "lat")][apply(allDistances, 2, which.min),]

  unitFactor <- switch(tolower(unit),
                       nm = 1/1.852,
                       km = 1,
                       m = 1e-3)

  return(list(value = minDistancesValue*unitFactor,
              position = minDistancesPosition))
}

#' @title Calculate the moving average for a numeric vector
#'
#' @param x Numeric vector.
#' @param n Size for grouping.
#' @param circular If \code{TRUE}, wrap the filter around the ends of the series,
#' otherwise assume external values are missing (\code{NA}).
#' @param ... Extra arguments passed to \code{\link{filter}} function.
#'
#' @return A numeric vector with the same length of the original vector.
#' @export
#'
#' @examples
#' exampleVector <- runif(n = 20, min = 0, max = 100)
#' movingAverage(x = exampleVector)
movingAverage <- function(x, n = 3, circular = TRUE, ...)
{
  output <- filter(x, rep(1/n, n), circular = circular, ...)

  return(an(output))
}

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

#' @title Title plotIsoparalitoral
#'
#' @param codeList AIP codes to plot.
#' @param add logical flag that specifies whether to add to the current plot. If FALSE, a new plot is begun, using
#' coordinates for Peruvian shore.
#' @param old \code{logical}. Specifying whether to use old AIP shape (\code{AIPShapefile_old}) o the new (\code{AIPShapefile_new}).
#' @param plotParams Extra parameters passed to \code{\link{plot}} function. This argument is ignored if \code{add = TRUE}.
#' @param mapParams Extra parameters passed to \code{\link{map}} function. This argument is ignored if \code{add = TRUE}.
#' @param ... Extra parameters passed to \code{plot.SpatialPolygons} function.
#'
#' @details \code{plot.SpatialPolygons} is an internal function of \code{\link{sp}}. Some important parameters are: \code{border} which
#' allows to specify color (or colors if a vector) of each AIP. \code{col} is useful to modify fill color of AIP.
#'
#' @export
plotIsoparalitoral <- function(codeList = NULL, add = FALSE, old = TRUE,
                               plotParams = list(xlim = c(-90, -70), ylim = c(-18, -2), axes = TRUE, xlab = NA, ylab = NA),
                               mapParams = list(database = "world"), ...)
{

  # Select shape
  referenceShapefile <- get(ifelse(test = isTRUE(old), yes = "AIPShapefile_old", no = "AIPShapefile_new"))

  if(!isTRUE(add)){
    # Make plot
    do.call(what = "plot", args = c(list(x = 1, y = 1), plotParams))

    # Make coast line
    do.call(what = "map", args = c(list(add = TRUE), mapParams))
  }


  if(is.null(codeList)){
    codeList <- referenceShapefile$code
  }

  index <- match(codeList, referenceShapefile$code)

  if(sum(is.na(index)) == length(index)){
    stop("None of values in 'codeList' is a valid AIP code.")
  }

  codeList <- index

  plot(referenceShapefile[codeList, 1], add = TRUE, ...)

  return(invisible())
}

#' @title Easy way to prepare a folder for a typical R Project
#'
#' @param folder \code{character}. Path of the folder that will be created.
#' @param type Indicates the type of sub directories that will be created (see Details).
#'
#' @details You can indicate the set of directories that will be created using \code{type} as follows:
#' \itemize{
#'  \item{"1"}{"code", "data", "figures", "outputs"}
#'  \item{"2"}{"code", "data", "figures", "raw", "outputs", "bib"}
#'  \item{"3"}{"bib", "drafts", "presentations", "results/raw", "results/data", "results/figures",
#'             "results/code/figures", "results/code/analysis"}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prepareProjFolder(folder = "../exampleFolder", type = 3)
#' }
prepareProjFolder <- function(folder, type = 1){

  folderList <- switch(type,
                       "1" = c("code", "data", "figures", "outputs"),
                       "2" = c("code", "data", "figures", "raw", "outputs", "bib"),
                       "3" = c("bib", "drafts", "presentations", "results/raw", "results/data", "results/figures",
                               "results/code/figures", "results/code/analysis"),
                       "Incorrect value for 'type'.")

  sapply(file.path(folder, folderList), dir.create, showWarnings = FALSE, recursive = TRUE)

  return(invisible())
}

#' @title Title Draw colorful squares
#'
#' @param nsquares How much squares do you desire? (500, as default).
#'
#' @export
#'
#' @examples
#' randomRectangles(nsquares = 120)
randomRectangles <- function(nsquares = 500){
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

#' @title Make a progress bar
#'
#' @param i Step of the iteration.
#' @param n Total number of iteration
#' @param stepText Text before 'n'.
#'
#' @return The function returns only messages (from \code{cat}).
#' @export
#'
#' @examples
#' n = 1000
#' for(i in seq(n)){
#'   Sys.sleep(0.01)
#'   progressBar(i = i, n = n)
#' }
progressBar <- function(i, n, stepText = "n"){

  if(i > n){
    stop("Incorrect value for 'i' or 'n'.")
  }

  index <- floor(i/n*100)

  if(index == floor((i - 1)/n*100)){
    return(invisible())
  }

  if(index %% 5 == 0){
    if(index %% 25 == 0){
      if(i != n){
        cat(paste0("  ", index, "% (", stepText, " = ", i, ")\n"))
      }else{
        cat(paste0(" 100% (", stepText, " = ", i, ")\n"))
      }
    }else{
      cat("|")
    }
  }else{
    cat(".")
  }

  return(invisible())
}

#' @title Get an overlay polygon from two set of points
#'
#' @param points1 Set of points 1 (\code{data.frame} or \code{list}).
#' @param points2 Set of points 2 (\code{data.frame} or \code{list}).
#' @param fillBase Indicates the way for closing the polygon. If \code{NULL} (default), the polygons will be closed
#' joining the first and the last point. For extra ways, see details.
#'
#' @details \code{points1} and \code{points2} must be a \code{list} or \code{data.frame} with 'x' and 'y' levels as
#' coordinates.
#' \code{fillBase} argument can be used for closing the polygons. If \code{NULL}, first and last points will
#' be joined. another value may be a function (\code{min}, \code{max}, \code{mean} or \code{median}) or a single
#' number (e.g. zero).
#'
#' @return An \code{SpatialPolygons} object.
#' @export
#'
#' @examples
#' n <- 30
#' points1 <- list(x = seq(n), y = runif(n = n, min = 20, max = 40))
#' points2 <- list(x = seq(n), y = runif(n = n, min = 5, max = 30))
#'
#' plot(points1, type = "l", ylim = c(0, 40))
#' lines(points2)
#'
#' overShape <- getOverlay(points1 = points1, points2 = points2, fillBase = 0)
getOverlay <- function(points1, points2, fillBase = NULL){

  points1 <- as.list(points1)
  points2 <- as.list(points2)

  if(!is.null(fillBase)){

    validFunctions <- c("min", "max", "mean", "median")
    if(is.function(fillBase) & is.element(fillBase, validFunctions)){
      fillBase <- match.fun(fillBase)

      fillBase <- fillBase(y, na.rm = TRUE)
    }else if(!is.numeric(fillBase)){
      stop("Incorrect value for 'fillBase'.")
    }

    points1 <- with(points1, list(x = c(x, rev(x)), y = c(y, rep(fillBase, length(y)))))
    points2 <- with(points2, list(x = c(x, rev(x)), y = c(y, rep(fillBase, length(y)))))
  }else{
    with(points1, list(x = c(x, x[1]), y = c(y, y[1])))
    points1 <- with(points1, list(x = c(x, x[1]), y = c(y, y[1])))
    points2 <- with(points2, list(x = c(x, x[1]), y = c(y, y[1])))
  }


  points1 <- SpatialPolygons(Srl = list(Polygons(srl = list(Polygon(points1)), ID = 1)))


  points2 <- SpatialPolygons(Srl = list(Polygons(srl = list(Polygon(points2)), ID = 1)))

  overShape <- intersect(x = points1, y = points2)
  unionShape <- union(x = points1, y = points2)

  return(list(overShape, unionShape))
}


#' @title Plot typical wind arrows from wind velocity data
#'
#' @param uComponent Matrix with values of velocity in x component See Details.
#' @param vComponent Matrix with values of velocity in y component See Details.
#' @param maxLength For changing length of arrows (\code{numeric}).
#' @param densityfactor For changing the quantity of arrows on plot. Value from 0 to 1.
#' @param arrowCol Color of arrows.
#' @param add \code{logical}; if \code{TRUE}, add to current plot and \code{...} will not be considered.
#' @param xInterval If \code{add = TRUE}, it works for specifing the interval of marks in X axis.
#' @param yInterval If \code{add = TRUE}, it works for specifing the interval of marks in Y axis.
#' @param col Color table to use for image.
#' @param ... Extra arguments passed to \code{image} function. It will only be used if \code{add = FALSE}.
#'
#' @details Both \code{uComponent} and \code{vComponent} must be list objects with levels x (vector of longitudes),
#' y (vector of latitudes) and z (matrix of values).
#'
#' @return A plot of arrows indicating the intensity and direction of winds.
#' @export
makeWindPlot <- function(uComponent, vComponent, maxLength = 1, densityfactor = 0.98, arrowCol = "black",
                         add = TRUE, includeRaster = TRUE, col = tim.colors(1e3),
                         xInterval = NULL, yInterval = NULL, ...){

  intensityMatrix <- list(x = uComponent$x,
                          y = uComponent$y,
                          z = sqrt(uComponent$z^2 + vComponent$z^2))

  angleMatrix <- list(x = uComponent$x,
                      y = uComponent$y,
                      z = atan(vComponent$z/uComponent$z))

  if(is.null(list(...)$xlim)){
    if(is.list(intensityMatrix)){
      xlim <- range(intensityMatrix$x)
    }else if(is.list(angleMatrix)){
      xlim <- range(angleMatrix$x)
    }else{
      xlim <- c(0, 1)
    }
  }

  if(is.null(list(...)$ylim)){
    if(is.list(intensityMatrix)){
      ylim <- range(intensityMatrix$y)
    }else if(is.list(angleMatrix)){
      ylim <- range(angleMatrix$y)
    }else{
      ylim <- c(0, 1)
    }
  }

  if(is.null(list(...)$zlim)){
    zlim <- c(0, max(an(intensityMatrix$z), na.rm = TRUE))
  }

  if(is.matrix(intensityMatrix)){
    intensityMatrix <- list(x = seq(xlim[1], xlim[2], length.out = nrow(intensityMatrix)),
                            y = seq(ylim[1], ylim[2], length.out = ncol(intensityMatrix)),
                            z = intensityMatrix)
  }

  if(is.matrix(angleMatrix)){
    angleMatrix <- list(x = seq(xlim[1], xlim[2], length.out = nrow(angleMatrix)),
                        y = seq(ylim[1], ylim[2], length.out = ncol(angleMatrix)),
                        z = angleMatrix)
  }

  # x11()
  if(!isTRUE(add)){
    plot(1, 1, pch = NA, axes = FALSE, xlim = xlim, ylim = ylim, xlab = NA, ylab = NA)
  }

  if(isTRUE(includeRaster)){
    image(intensityMatrix, add = TRUE, col = col, ...)
  }

  densityfactor <- floor((nrow(intensityMatrix$z) - 1)*(1 - densityfactor) + 1)

  for(i in seq(from = 1, to = nrow(intensityMatrix$z), by = densityfactor)){
    for(j in seq(from = 1, to = ncol(intensityMatrix$z), by = densityfactor)){

      intensityValue <- intensityMatrix$z[i, j]
      angleValue <- angleMatrix$z[i, j]

      if(!is.na(intensityValue) || !is.na(angleValue)){
        arrows(x0 = intensityMatrix$x[i], y0 = intensityMatrix$y[j],
               x1 = intensityMatrix$x[i] + sin(angleValue)*intensityMatrix$z[i, j]*maxLength,
               y1 = intensityMatrix$y[j] + cos(angleValue)*intensityMatrix$z[i, j]*maxLength,
               length = 0.05, angle = 30, col = arrowCol)
      }
    }
  }

  xInterval <- if(is.null(xInterval) || is.na(xInterval)) diff(xlim)/5 else xInterval
  yInterval <- if(is.null(yInterval) || is.na(yInterval)) diff(ylim)/5 else yInterval

  if(!isTRUE(add)){
    xAxis <- seq(xlim[1], xlim[2], xInterval)
    yAxis <- seq(ylim[1], ylim[2], yInterval)

    axis(side = 1, at = xAxis, labels = getCoordsAxes(xAxis, "lon"))
    axis(side = 2, at = yAxis, labels = getCoordsAxes(yAxis, "lat"), las = 2)
    box()
  }

  if(isTRUE(includeRaster)){
    image.plot(intensityMatrix, add = TRUE, col = col, legend.only = TRUE, ...)
  }


  return(invisible())
}

#' @title Extract info from AIP codes
#'
#' @param aipVector Vector of AIP.See Details
#'
#' @details AIP codes must be written in format DDLLPP, where DD is value of Distance to coast (mn/10),
#' LL are values of Latitude (as integer) and PP is position (up or down).
#'
#' @return A \code{data.frame} with variables dc, lat and upDown.
#' @export
#'
#' @examples
#' getAIPInfo(c(30073, 1020, 2010))
getAIPInfo <- function(aipVector){
  ncharAip <- nchar(aipVector)

  dc <- an(substr(aipVector, 1, ifelse(ncharAip == 4, 1, 2)))*10
  lat <- an(substr(aipVector, ifelse(ncharAip == 4, 2, 3), ncharAip - 1))
  upDown <- an(substr(aipVector, ncharAip, ncharAip))

  if(any(!is.element(upDown, c(0, 3)))){
    warning(paste("Values #", paste(which(!is.element(upDown, c(0, 3))), collapse = ", "), "have wrong values for up-down info."))
  }

  return(data.frame(dc, lat, upDown, stringsAsFactors = FALSE))
}

#' @title Add a box with a text inside
#'
#' @param xLimits x-Axis limits for box.
#' @param yLimits y-Axis limits for box.
#' @param text A character or \link{expression} vector specifying the text to be written.
#' @param border the color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders. See Details.
#' @param col The color for filling the box polygon.
#' @param lty The line type to be used, as in par.
#' @param ... Extra arguments passed from \link{text} function.
#'
#' @details For extra details about \code{border}, \code{col} and \code{lty}, check the description of \link{polygon}.
#'
#' @export
#'
#' @examples
#' plot(1, 1, pch = NA)
#' addTextBox(xLimits = c(0.9, 1.1), yLimits = c(0.9, 1.1), text = "Hello World!", col = "indianred1", font = 2)
addTextBox <- function(xLimits, yLimits, text, border = NULL, col = "white", lty = par("lty"), ...){

  polygon(x = c(xLimits, rev(xLimits)), y = rep(yLimits, each = 2), border = border, col = col)
  text(x = mean(xLimits), y = mean(yLimits), labels = text, ...)

  return(invisible())
}
