#' @title  Given a frequency table size, it generates a vertical array of graphics for each specified
#' category (months, latitudes, years, etc.).
#'
#' @details This function takes a length frequency data and returns an stacking plot of that frequencies.
#' smooth: logical. If TRUE, function uses an spline function to plot by categories.
#' This function uses a data with categories at cols and length at rows.
#'
#' @param file \code{character}. File path of length frequencies table.
#' @param file2 \code{character}. File path of second length frequencies table.
#' @param profile \code{character}. Profile by species.
#' @param xlab A title for the X axis.
#' @param ylab A title for the Y axis.
#' @param cex.axis Size of axis text.
#' @param xlim Limits for the X axis.
#' @param Xinterval Intervals for the X axis.
#' @param Yinterval Intervals for the Y axis.
#' @param lcol Vector of colors for length lines (one or more).
#' @param lcol2 Color (just one) of length line for second table.
#' @param lwd Width of length lines.
#' @param lty Vector of line type for length lines (one or more).
#' @param styleLab Numer (0,1,2,3), the style of axis labels.
#' @param jValue Juvenile length.
#' @param jtext
#' @param jcex
#' @param jtextcol
#' @param cat.cex
#' @param jround
#' @param jcol
#' @param jwd
#' @param jty
#' @param sinpesca
#' @param sinjuveniles
#' @param zeros
#' @param nzeros
#' @param showRange
#' @param adj
#' @param showJuv
#' @param smooth
#' @param spar
#' @param totalFreq
#' @param totalLabel
#'
#' @export
lengthFrequencyPlot <- function(file, file2 = NA, profile = NULL, categoryNames = NULL,
                                xlab = "Longitud (cm)", ylab = "Frecuencia (%)", ylim = NULL,
                                cex.axis = 1.2, xlim = NULL, Xinterval = 1, Yinterval = 10,
                                lcol = "blue", lcol2 = NULL, lwd = 1, lty = "solid", lineCategory = -2,
                                styleLab = 2, jValue = NA, jtext = NA, jcex = 1, jtextcol = "black",
                                cat.cex = 1, jround = 1, jcol = "red", jwd = 1, jline = -1.5,
                                jty = "dotted", sinpesca = "Sin pesca", sinjuveniles = "Sin juveniles",
                                zeros = FALSE, nzeros = 0, showRange = TRUE, adj = 0.02, showJuv = TRUE,
                                smooth = TRUE, spar = 0.01, totalFreq = FALSE,
                                totalLabel = "Total"){

  data <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)

  if(totalFreq){
    data <- data.frame(data[,1], apply(data, 1, sum, na.rm = TRUE))
  }else{
    data[,apply(data, 2, function(x) sum(x > 0, na.rm = TRUE)) < 2] <- 0
  }

  if(zeros | smooth){
    data[is.na(data)] = 0
  }else{
    data[data == 0] <- NA
  }

  if(totalFreq){
    data <- data.frame(data[,1], data[,2]/sum(data[,2], na.rm = TRUE)*100, check.names = FALSE)
    colnames(data) <- c("length", totalLabel)
  }else{
    data <- data.frame(data[,1],
                       apply(data[,-1], 2, function(x) 100*x/sum(x, na.rm = TRUE)),
                       check.names = FALSE)
  }

  if(nzeros > 0){
    iniIndex <- apply(apply(data[,-1], 2, is.na), 2, VectorInVector, pattern = c(TRUE, FALSE))
    finIndex <- apply(apply(data[,-1], 2, is.na), 2, VectorInVector, pattern = c(FALSE, TRUE))
    for(i in seq(2, ncol(data))){
      if(sum(is.na(data[,i])) == nrow(data)){
        next
      }

      index <- c(min(iniIndex[[i - 1]]), max(finIndex[[i - 1]]))
      index <- c(seq(index[1] - nzeros + 1, index[1]), seq(index[2], index[2] + nzeros - 1))
      data[index,i] <- 0
    }
  }

  if(is.null(ylim)){
    ylim <- c(0, roundUp(max(data, na.rm = TRUE)))
  }

  if(is.null(xlim)){
    xlim <- c(roundUp(min(data[,1])), roundUp(max(data[,1])))
  }

  if(!is.null(profile)){
    if(tolower(profile) == "anchoveta"){
      Xinterval <- 0.5
      jValue <- 12
      jtext <- "juv = "

    }else if(tolower(profile) == "jurel"){
      Xinterval <- 1
      jValue <- 31
      jtext <- "juv = "

    }else if(tolower(profile) == "caballa"){
      Xinterval <- 1
      jValue <- 29
      jtext <- "juv = "
    }
  }

  lcol <- rep(lcol, length.out = ncol(data) - 1)

  if(is.null(lcol2)){
    lcol2 <- "gray31"
  }

  lty <- rep(lty, length.out = ncol(data) - 1)

  if(!is.na(file2)){
    data2 <- read.csv(file2)[,seq(ncol(data))]
    data2 <- data.frame(data2[,1],
                        apply(data2[,-1], 2, function(x) 100*x/sum(x, na.rm = TRUE)),
                        check.names = FALSE)
    ylim <- c(0, max(c(max(data2, na.rm = TRUE), ylim[2])))
  }

  if(is.null(categoryNames)){
    categoryNames <- colnames(data)
  }else{
    categoryNames <- c("", categoryNames)
  }


  x11()
  par(mfrow = c(ncol(data) - 1, 1), mar = c(0, 2, 0, 2), oma = c(6, 4, 1, 1))

  for(i in seq(2, ncol(data))){
    if(sum(is.na(data[,i])) == nrow(data)){
      plot(data[,1], rep(0, nrow(data)), col = "white", xlab = NA, ylab = NA, axes = FALSE,
           xlim = xlim, ylim = ylim)
    }else{
      if(smooth){
        model <- smooth.spline(data[,1], data[,i], spar = spar)
        model <- predict(model, seq(xlim[1], xlim[2], diff(xlim)/500))

        if(!isTRUE(zeros)){
          model$y[which(model$y < 0.1)] <- NA
        }else{
          model$y[which(model$y < 0.1)] <- 0
        }

        plot(model, xlab = NA, ylab = NA, axes = FALSE, ylim = ylim, type = "l",
             lty = lty[i - 1], xlim = xlim, col = lcol[i - 1], lwd = lwd)
      }else{
        plot(data[,1], data[,i], xlab = NA, ylab = NA, axes = FALSE, ylim = ylim, type = "l",
             lty = lty[i - 1], xlim = xlim, col = lcol[i - 1], lwd = lwd)
      }
    }

    if(!is.na(file2)){
      if(sum(is.na(data2[,i])) == nrow(data2)){
        points(data2[,1], rep(0, nrow(data2)), col = "white",
               xlim = xlim, ylim = ylim)
      }else if(smooth){
        model <- smooth.spline(data2[,1], data2[,i], spar = spar)
        model <- predict(model, seq(xlim[1], xlim[2], diff(xlim)/500))

        if(!isTRUE(zeros)){
          model$y[which(model$y < 0.1)] <- NA
        }else{
          model$y[which(model$y < 0.1)] <- 0
        }

        points(model, ylim = ylim, type = "l",
               lty = lty[i - 1], xlim = xlim, col = lcol2, lwd = lwd)
      }else{
        points(data2[,1], data2[,i], ylim = ylim, type = "l",
               lty = lty[i - 1], xlim = xlim, col = lcol2, lwd = lwd)
      }
    }

    box()

    atY <- seq(ylim[1], ylim[2], by = Yinterval)
    if(i%%2 != 0 | ncol(data) == 2){
      axis(2, las = styleLab, cex.axis = cex.axis, at = atY, labels = atY)
    }else{
      axis(4, las = styleLab, cex.axis = cex.axis, at = atY, labels = atY)
    }

    if(!is.na(jValue)){
      abline(v = jValue, lty = jty, col = jcol, lwd = jwd)
    }

    if(isTRUE(showJuv)){
      if(juv(data[,i], data[,1], jValue) != 0){

        juvtext = paste(jtext,
                        round(juv(data[,i], data[,1], jValue), jround),
                        "%", sep = "")
      }else{
        if(sum(is.na(data[,i])) == nrow(data)){
          juvtext = sinpesca
        }else{
          juvtext = sinjuveniles
        }
      }

      mtext(juvtext, 3, line = jline, adj = 1 - adj, cex = jcex, col = jtextcol) # juveniles
    }

    if(showRange){
      if(sum(is.na(data[,i])) != nrow(data)){
        lentext = paste0("Rango : ", min(data[!is.na(data[,i]), 1], na.rm = TRUE),
                         " - ", max(data[!is.na(data[,i]), 1], na.rm = TRUE))
      }else{
        lentext = NA
      }
    }else{
      lentext = NA

      mtext(lentext, 1, line = -1, adj = 1 - adj, cex = jcex - 0.2) # Rango tallas
    }

    mtext(categoryNames[i], 3, line = lineCategory, adj = adj, cex = cat.cex) # Categor?as
  }

  atX = seq(xlim[1], xlim[2], by = Xinterval)
  axis(side = 1, at = atX, labels = atX, cex.axis = cex.axis)

  mtext(xlab, 1, line = 4, outer = TRUE, cex = cex.axis)
  mtext(ylab, 2, line = 2, outer = TRUE, cex = cex.axis)

  return(invisible())
}
