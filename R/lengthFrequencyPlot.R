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
#' @param namesLine1 line (see \code{\link{mtext}}) parameter for text of category for file 1.
#' @param namesCex1 cex (see \code{\link{mtext}}) parameter for text of category for file 1.
#' @param namesCol1 col (see \code{\link{mtext}}) parameter for text of category for file 1.
#' @param showNames2 \code{logical} do you want to show the names of categories of file 2?
#' @param namesLine2 line (see \code{\link{mtext}}) parameter for text of category for file 2.
#' @param namesCex2 cex (see \code{\link{mtext}}) parameter for text of category for file 2.
#' @param namesCol2 col (see \code{\link{mtext}}) parameter for text of category for file 2.
#' @param juvTextCol1 col (see \code{\link{mtext}}) parameter for text of juveniles for file 1.
#' @param juvTextCol2 col (see \code{\link{mtext}}) parameter for text of juveniles for file 2.
#'
#' @details This function takes a length frequency data and returns an stacking plot of that frequencies.
#' smooth: logical. If TRUE, function uses an spline function to plot by categories.
#' This function uses a data with categories at col and length at rows.
#'
#'
#' @export
lengthFrequencyPlot <- function(file1, file2 = NULL, dataFactor = 1, newPlot = FALSE, profile = NULL,
                                xlim = NULL, xInterval = NULL,
                                ylim = c(0, 50), yInterval = NULL,
                                ylimList = NULL, yIntervalList = NULL,
                                ltys1 = "solid", lwds1 = "1", col1 = "black", ltys2 = "solid", lwds2 = "1", col2 = "blue",
                                juvLimit = NULL, juvLty = "dotted", juvLwd = 1, juvCol = "red",
                                showJuv1 = TRUE, juvLine1 = -2, juvCex1 = 1, juvLabel1 = "juveniles_1 = ",
                                showJuv2 = TRUE, juvLine2 = -4, juvCex2 = 1, juvLabel2 = "juveniles_2 = ",
                                namesLine1 = -2, namesCex1 = 1, namesCol1 = NULL,
                                showNames2 = FALSE, namesLine2 = -2, namesCex2 = 1, namesCol2 = NULL,
                                juvAdj = 0.99, juvRound = 0, juvSide = 3, juvTextCol1 = NULL, juvTextCol2 = NULL,
                                cex.axis = 1, cex.lab = 1, ylab_line = 3, namesAdj = 0.01,
                                title_text = NULL, title_line = 2, title_cex = NULL, title_col = "black", title_font = 1,
                                smooth = FALSE, oma = NULL, xlab = NULL, ylab = "Frecuencia (%)",
                                noDataLabel = "Sin datos", ylabFactor = 1, relative = TRUE){

  # Preserve par values before make graphics
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  if(any(is.element(class(file1), c("data.frame", "matrix")))){

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
    if(any(is.element(class(file2), c("data.frame", "matrix")))){
      file2Names <- list(an(file2[,1]), colnames(file2)[-1])
      file2 <- data.frame(file2[,-1], stringsAsFactors = FALSE)
      dimnames(file2) <- file2Names
    }else{
      file2 <- read.csv(file = file2, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }

    if(ncol(file1) != ncol(file2)){
      stop("'file1' and 'file2' must have the same number of columns.")
    }

    if(isTRUE(relative)){
      file2Names <- dimnames(file2)
      file2 <- data.frame(sweep(as.matrix(file2), 2, colSums(file2, na.rm = TRUE), "/")*100)
      dimnames(file2) <- file2Names
    }

    # file2[file2 <= 0 | is.na(file2)] <- 0

    file2 <- file2*dataFactor

    ltys2 <- rep(ltys2, length.out = ncol(file2))
    lwds2 <- rep(lwds2, length.out = ncol(file2))
    col2 <- rep(col2, length.out = ncol(file2))
    namesCol2 <- if(is.null(namesCol2)) col2 else rep(namesCol2, length.out = ncol(file2))
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

  if(is.null(xInterval)){
    xInterval <- 1
  }

  ltys1 <- rep(ltys1, length.out = ncol(file1))
  lwds1 <- rep(lwds1, length.out = ncol(file1))
  col1 <- rep(col1, length.out = ncol(file1))
  namesCol1 <- if(is.null(namesCol1)) col1 else rep(namesCol1, length.out = ncol(file1))

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

      if(isTRUE(showNames2)){
        mtext(text = colnames(file2)[i], side = 3, adj = namesAdj, line = namesLine2, cex = namesCex2,
              col = namesCol2[i])
      }
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

    mtext(text = colnames(file1)[i], side = 3, adj = namesAdj, line = namesLine1, cex = namesCex1,
          col = namesCol1[i])

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
