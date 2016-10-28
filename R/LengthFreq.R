#' @title  Given a frequency table size, it generates a vertical array of graphics for each specified
#' category (months, latitudes, years, etc.).
#'
#' @details This function takes a length frequency data and returns an stacking plot of that frequencies.
#' smooth: logical. If TRUE, function uses an spline function to plot by categories.
#' This function uses a data with categories at cols and length at rows.
#'
#' @param file1 Indica la tabla desde donde se dibujarán las frecuencias por talla. Ver detalles.
#' @param file2 Indica la tabla secundaria desde donde se dibujarán las frecuencias por talla. Ver detalles.
#' @param xlim
#' @param xInterval
#' @param ylim
#' @param yInterval
#' @param ltys1
#' @param lwds1
#' @param cols1
#' @param ltys2
#' @param lwds2
#' @param cols2
#' @param juvLine
#' @param juvLty
#' @param juvLwd
#' @param juvCol
#' @param cex.axis
#' @param cex.lab
#' @param smooth
#' @param oma
#' @param yLab
#' @param noDataLabel
#' @param juvLabel
#' @param yLabFactor
#' @param relative
#' @param profile
#' @param xlab
#'
#' @export
lengthFrequencyPlot <- function(file1, file2 = NULL,
                                profile = NULL, xlim = NULL, xInterval = 1, ylim = c(0, 50), yInterval = NULL,
                                ltys1 = "solid", lwds1 = "1", cols1 = "black", ltys2 = "solid", lwds2 = "1", cols2 = "blue",
                                juvLine = NULL, juvLty = "dotted", juvLwd = 1, juvCol = "red", juvCex = 1,
                                cex.axis = 1, cex.lab = 1,
                                smooth = FALSE, oma = c(5, 5, 1, 3), xLab = NULL, yLab = "Frecuencia (%)",
                                noDataLabel = "Sin datos", juvLabel = "juveniles = ", yLabFactor = 1, relative = TRUE){

  if(is.element(class(file1), c("data.frame", "matrix"))){
    file1 <- file1[,-1]
  }else{
    file1 <- read.csv(file = file1, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
  }

  if(!is.null(file2)){
    if(is.element(class(file1), c("data.frame", "matrix"))){
      file2 <- file2[,-1]
    }else{
      file2 <- read.csv(file = file2, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }

    if(ncol(file1) != ncol(file2)){
      stop("'file1' and 'file2' must have the same number of columns.")
    }

    if(isTRUE(relative)){
      file2 <- sweep(as.matrix(file2), 2, colSums(file2, na.rm = TRUE), "/")*100
    }

    file2[file2 <= 0 | is.na(file2)] <- 0

    ltys2 <- rep(ltys2, length.out = ncol(file2))
    lwds2 <- rep(lwds2, length.out = ncol(file2))
    cols2 <- rep(cols2, length.out = ncol(file2))
  }



  if(!is.null(profile)){

    index <- match(tolower(profile), rownames(speciesInfo))

    if(is.null(xlim)){
      xlim <- an(speciesInfo[index, c("Lmin", "Lmax")])
    }

    if(is.null(juvLine)){
      juvLine <- an(speciesInfo$juvenile[index])
    }

    if(is.null(xInterval)){
      xInterval <- an(speciesInfo$bin[index])
    }

    if(is.null(xLab)){
      xLab <- paste0("Longitud ", speciesInfo$lengthType[index], " (", speciesInfo$unit[index], ")")
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
  cols1 <- rep(cols1, length.out = ncol(file1))

  file1[file1 <= 0 | is.na(file1)] <- 0

  x11()
  par(mfrow = c(ncol(file1), 1), mar = c(rep(0, 4)), oma = oma, xaxs = "i", yaxs = "i")

  for(i in seq(ncol(file1))){
    plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)

    if(isTRUE(smooth)){
      allLengths <- spline(x = an(rownames(file1)), y = file1[,i], method = "natural", n = 1e3)
    }else{
      allLengths <- list(x = an(rownames(file1)), y = file1[,i])
    }

    allLengths$y[allLengths$y < 0.001] <- NA

    if(sum(!is.na(allLengths$y)) < 2){
      mtext(text = noDataLabel, side = 3, adj = 0.99, line = -2)
    }else if(!is.null(juvLine)){
      juvValue <- sum(allLengths$y[allLengths$x < juvLine], na.rm = TRUE)/sum(allLengths$y, na.rm = TRUE)
      mtext(text = paste0(juvLabel, round(juvValue*100, 0), " %"), cex = juvCex,
            side = 3, line = -2, adj = 0.99)

      abline(v = juvLine, lty = juvLty, lwd = juvLwd, col = juvCol)
    }

    lines(allLengths, lty = ltys1[i], lwd = lwds1[i], col = cols1[i])

    if(!is.null(file2)){
      if(isTRUE(smooth)){
        allLengths <- spline(x = an(rownames(file2)), y = file2[,i], method = "natural", n = 1e3)
      }else{
        allLengths <- list(x = an(rownames(file2)), y = file2[,i])
      }

      allLengths$y[allLengths$y < 0.001] <- NA

      lines(allLengths, lty = ltys2[i], lwd = lwds2[i], col = cols2[i])
    }

    if(is.null(yInterval)){
      yInterval <- diff(ylim)/5
    }

    if(i %% 2 > 0){
      axis(side = 2, at = seq(ylim[1], ylim[2], by = yInterval), las = 2, cex.axis = cex.axis,
           labels = seq(ylim[1], ylim[2], by = yInterval)/yLabFactor)
    }else{
      axis(side = 4, at = seq(ylim[1], ylim[2], by = yInterval), las = 2, cex.axis = cex.axis,
           labels = seq(ylim[1], ylim[2], by = yInterval)/yLabFactor)
    }

    if(i == ncol(file1)){
      if(!is.null(profile) && profile == "anchoveta"){
        axis(side = 1, at = seq(xlim[1], xlim[2], by = xInterval), labels = NA, tcl = -0.25)
        axis(side = 1, at = seq(xlim[1], xlim[2], by = 1), cex.axis = cex.axis)
      }else{
        axis(side = 1, at = seq(xlim[1], xlim[2], by = xInterval), cex.axis = cex.axis)
      }
    }

    mtext(text = colnames(file1)[i], side = 3, adj = 0.01, line = -2)

    box()
  }

  mtext(text = ifelse(is.null(xLab), "Longitud", xLab), side = 1, line = 3, outer = TRUE,
        cex = cex.lab)
  mtext(text = yLab, side = 2, line = 3, outer = TRUE, cex = cex.lab)

  return(invisible())
}
