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
  #########################################################
  ##################### FUNCIÓN DE CONTEO #################
  ########################## v. 1.0 #######################
  ############## Autor: Wencheng Lau-Medrano ##############
  #########################################################

  cat("\n--------------------------\n")
  cat("-------- COMANDOS --------\n")
  cat("--------------------------\n")

  cat("\n new     : Para definir una especie nueva en la tabla de conteo.\n")
  cat("\n correct : Para corregir o realizar una operación sobre alguno\n")
  cat("\n           de los valores de conteo.\n")
  cat("\n end     : Finalizar conteo (mostrar y guardar tabla de conteo).\n")
  cat("\n help    : Mostrar lista de comandos.\n")

  # Definir función para mostrar la tabla de conteo
  showPlots <- function(spList){

    # Eliminar plots anteriores (si hubiera)
    if(!is.null(dev.list()))
      dev.off()

    # Definir parámetros de ploteo
    par(mfrow = c(1, 1), mar = rep(0, 4), xaxs = "i", yaxs = "i", oma = rep(0, 4))

    # Definir el máximo número de filas
    maxXlim <- ifelse(test = nrow(spList) <= 9,
                      yes = 10, no = 1 + nrow(spList))

    # Generar las líneas de la tabla de conteo
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

  # Bucle de conteo y petición de datos
  while(is.na(command) || command != "end"){
    # Mensaje si el conteo está en cero (i.e. no hay NINGUNA especie)
    if(allCounter > 0)
      cat("\nIngrese caracter o comando: ")

    # Si hay al menos una especie definida, realizar la petición de caracter para contar
    # o el comando a ejecutar
    command <- ifelse(test = allCounter == 0 & is.null(spList),
                      yes = "new",
                      no = tolower(scan(what = "character", nmax = 1, quiet = TRUE)))

    if(command == paste0(letters[c(24, 9, 13)], collapse = ""))
      cat("\nHey ", paste(paste0(LETTERS[c(19, 8, 1, 18, 11, 25)], collapse = ""),
                          paste0(LETTERS[c(24, 9, 13)], collapse = "")),
          "! :", LETTERS[4], "\n", sep = "")

    # Si no se ha colocado ningún caracter, volver a realizar la petición
    if(length(command) == 0 || is.na(command)){
      cat("\nCaracter o comando incorrecto.")
      next
    }

    # Si se utilizó el comando 'new', realizar las peticiones correspondientes
    if(command == "new"){
      # Petición del nombre de la especie nueva
      cat("\nIngrese el nombre de la especie NUEVA: ")
      newSp <- scan(what = "character", nmax = 1, quiet = TRUE)

      # Petición del caracter asociado a la especie nueva
      cat("\nIngrese el caracter ASOCIADO a la especie NUEVA: ")

      # Si el caracter indicado ya ha sido asignado a otra especie, volver a realizar
      # la petición
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
      cat("\n correct : Para corregir o realizar una operación sobre alguno\n")
      cat("\n           de los valores de conteo.\n")
      cat("\n end     : Finalizar conteo (mostrar y guardar tabla de conteo).\n")

      next
    }

    # Ubicar la posición de la fila segun el caracter asociado digitado
    pos <- match(command, as.character(spList[,2]))

    # Procedimientos para el comando 'corr' (corrección)
    if(grepl(x = command, pattern = "corr")){
      # Petición del caracter a corregir
      cat("\nIngrese el caracter a CORREGIR: ")
      corrChar <- tolower(scan(what = "character", nmax = 1, quiet = TRUE))

      # Indicar la corrección (adición, sustraccióm, multiplicación o división)
      cat("\nIngrese la CORRECCION (e.g. +1, -2, *3, /4): ")
      correction <- tolower(scan(what = "character", nmax = 1, quiet = TRUE))

      # Ubicar la posición de la fila segun el caracter asociado digitado
      pos <- match(corrChar, as.character(spList[,2]))

      # Extraer valores a corregir
      a <- as.numeric(spList[pos, 3])
      b <- as.numeric(substr(correction, 2, nchar(correction)))

      # Realiza la corrección
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
    # realizar la petición
    if(is.na(pos)){
      # Si el caracter ingresado es 'end', finalizar el conteo
      if(command == "end")
        break else{
          cat("\nCaracter INCORRECTO!")
          next
        }
    }

    # Si el caracter asociado es válido, incrementar el conteo de esa especie
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
  cat("\n ¡Conteo finalizado! \n")

  # Guardar un csv con la tabla de conteo
  if(!is.null(filepath) && dir.exists(filepath)){
    write.csv(spList, file = filepath, quote = FALSE, row.names = FALSE)

    cat("\n El archivo de conteo se ha guardado en: ", file.path(getwd(), filepath), "\n\n")
  }

  return(spList)
}

