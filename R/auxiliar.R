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

  return(invisible())
}

getDefinitions <- function(ranges, definitionTable){

  # Checking number of definition given
  rangesLength <- unlist(lapply(ranges, length)) - 1
  if(sum(rangesLength) != (ncol(definitionTable) - 1)){
    stop("Incorrect number of definitions. They must be\n", paste(names(rangesLength), collapse = "\t"),
         "\n", paste(rangesLength, collapse = "\t"))
  }

  # Set definition table
  waterMassesNames <- definitionTable[,1]
  definitionTable <- definitionTable[,-1]

  allCombinations <- NULL
  for(i in seq(nrow(definitionTable))){

    outIndex <- list()
    for(j in seq_along(ranges)){

      envirIndex <- seq(from = ifelse(j == 1, 1, cumsum(rangesLength)[j - 1] + 1), by = 1, length.out = rangesLength[j])
      outIndex[[j]] <- which(!is.na(as.logical(definitionTable[i, envirIndex])))
    }
    names(outIndex) <- names(ranges)

    allCombinations <- rbind(allCombinations, cbind(watermass = waterMassesNames[i], do.call(expand.grid, outIndex)))
  }

  allCombinations <- data.frame(as.matrix(allCombinations), stringsAsFactors = FALSE)
  colnames(allCombinations) <- tolower(colnames(allCombinations))

  return(allCombinations)
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



digitSum <- function(x, recursive = FALSE){
  if(recursive){
    while(nchar(x) > 1){
      x <- sum(an(unlist(strsplit(ac(x), ""))), na.rm = TRUE)
    }
  }else{
    x <- sum(an(unlist(strsplit(ac(x), ""))), na.rm = TRUE)
  }
}

getCoordsAxesInternal <- function(coord, what){

  if(tolower(what) == "lon"){
    if(coord < 0){
      sufix <- "\u00b0 W"
    }else if(coord > 0){
      sufix <- "\u00b0 E"
    }else{
      sufix <- "\u00b0"
    }
  }else if(tolower(what) == "lat"){
    if(coord < 0){
      sufix <- "\u00b0 S"
    }else if(coord > 0){
      sufix <- "\u00b0 N"
    }else{
      sufix <- "\u00b0"
    }
  }else{
    stop("Incorrect value for 'what' parameter.")
  }

  output <- paste0(round(abs(coord), 3), sufix)

  return(output)
}

setQuestion <- function(qst, ans){
  cat("\n", qst, "\n\n")

  ansIndex <- data.frame(real = seq_along(ans),
                         random = sample(x = seq_along(ans), size = length(ans), replace = FALSE))

  for(i in seq_along(ansIndex$random)){
    cat(paste0(i, ". "), ans[ansIndex$random[i]], "\n", sep = "")
  }

  cat("\n Tu respuesta: ")

  tempAns <- scan(what = character(), nmax = 1, quiet = TRUE)

  while(length(tempAns) < 1 || !(tempAns %in% seq_along(ans))){
    if(length(tempAns) < 1){
      cat("\u00bfDeseas salir del test? (s/n): ")

      quitAns <- scan(what = character(), nmax = 1, quiet = TRUE)

      if(tolower(quitAns) == "s"){
        cat("Ve a releer tus libros, te falta mucho")

        return(invisible())
      }
    }

    cat("Elecci\u00f3n incorrecta, selecciona un n\u00famero entre 1 y ", length(ans), "\n\n", sep = "")

    cat("Tu respuesta: ")

    tempAns <- scan(what = character(), nmax = 1, quiet = TRUE)
  }

  tempAns <- an(tempAns)

  return(ansIndex$random[tempAns])
}
