
# Main fx -----------------------------------------------------------------

#' Functions for making an interactive test
#'
#' @param testName The name for the test
#' @param questions \code{character} vector indicating the questions.
#' @param answers \code{list} with the options for each question.
#' @param correctAnswers \code{numeric} vector indicating the index for correct answers.
#'
#' @details Of course, arguments for questions, answers and correct answers must have the same length.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' sillyTest(testName = "Harry Potter", questions = testExample$questions,
#'           answers = testExample$answers, correctAnswers = testExample$correctAnswers)
#' }
sillyTest <- function(testName, questions, answers, correctAnswers){

  if(length(unique(c(length(questions), length(answers), length(correctAnswers)))) > 1){
    stop("Arguments for questions, answers and correct answers must have the same length.")
  }

  cat(paste0("Hola, bienvenido(a) a este Test de ", testName[1],
             ". \n\n \u00bfCu\u00e1l es tu nombre?"))

  gamerName <- scan(what = character(), n = 1, quiet = TRUE)

  cat("\n ", sprintf("\u00a1Genial %s!", gamerName),
      " \nEl test completo se compone de ", length(questions),
      " preguntas, \u00bfCu\u00e1ntas deseas resolver? ", sep = "")

  qstNumber <- suppressWarnings(as.integer(an(scan(what = character(), nmax = 1, quiet = TRUE))))

  while(is.na(qstNumber) || (qstNumber > length(questions) | qstNumber < 1)){
    cat("\n ", sprintf("Elecci\u00f3n incorrecta %s, selecciona un n\u00famero entre 1 y ",
                       length(questions), " \n\n", gamerName),
        "\n", sep = "")


    cat("Tu respuesta ")

    qstNumber <- suppressWarnings(as.integer(an(scan(what = character(), nmax = 1, quiet = TRUE))))
  }

  qstIndex <- sample(x = seq_along(questions), size = qstNumber, replace = FALSE)

  allAns <- NULL
  for(i in qstIndex){
    allAns <- rbind(allAns, c(i, setQuestion(qst = questions[i], ans = answers[[i]]), correctAnswers[i]))
  }

  colnames(allAns) <- c("question", "answer", "correct")

  correction <- apply(allAns[,-1], 1, function(x) abs(diff(x)))

  score <- round(sum(correction < 1, na.rm = TRUE)/qstNumber*100, 0)

  scorePlace <- ac(cut(score, breaks = c(-Inf, 20, 50, 75, 90, Inf),
                       labels = c("Muy mal", "Mal", "Bien", "Muy bien", "Genial")))

  cat("\n ", sprintf("\u00a1%s, %s!", scorePlace, gamerName),
      "\n", sprintf("Tu puntaje fue de %s", score), "%!", sep = "")

  return(invisible())
}


# Auxiliar fx -------------------------------------------------------------

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

