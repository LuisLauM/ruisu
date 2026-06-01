#' Plot length-frequency distributions
#'
#' Creates one or more length-frequency plots from length-class data stored
#' in matrices, data frames, or CSV files. Optionally overlays a second
#' dataset for comparison and displays the proportion of individuals below
#' a juvenile size threshold.
#'
#' @param file1 A matrix, data frame, or path to a CSV file containing
#' length-frequency data. Rows represent length classes and columns represent
#' groups (e.g. years, months, fleets, areas).
#' @param file2 Optional second dataset with the same structure as \code{file1},
#' used for comparison.
#' @param dataFactor Numeric multiplier applied to all frequency values.
#' @param newPlot Logical. If \code{TRUE}, opens a new graphics device.
#' @param profile Optional species profile name. When provided, default values
#' for \code{xlim}, \code{xInterval}, \code{juvLimit}, and \code{xlab} are
#' extracted from \code{speciesInfo}.
#' @param xlim Numeric vector of length two defining the x-axis limits.
#' @param xInterval Length-class interval for x-axis tick marks.
#' @param ylim Numeric vector of length two defining the y-axis limits.
#' @param yInterval Interval between y-axis tick marks.
#' @param ylimList Optional list of \code{ylim} values, one per panel.
#' @param yIntervalList Optional list of \code{yInterval} values, one per panel.
#' @param ltys1,lwds1,col1 Graphical parameters for lines corresponding to
#' \code{file1}.
#' @param alpha1 Transparency level for \code{file1} lines.
#' @param ltys2,lwds2,col2 Graphical parameters for lines corresponding
#'   to \code{file2}.
#' @param alpha2 Transparency level for \code{file2} lines.
#' @param juvLimit Numeric juvenile length threshold. A vertical reference
#'   line is drawn at this value and the percentage of individuals below
#'   the threshold can be displayed.
#' @param juvLty,juvLwd,juvCol Graphical parameters for the juvenile
#'   threshold line.
#' @param showJuv1,showJuv2 Logical. Whether to display juvenile percentages
#'   for \code{file1} and \code{file2}, respectively.
#' @param juvLine1,juvLine2 Line positions used when displaying juvenile
#'   percentages.
#' @param juvCex1,juvCex2 Character expansion factors for juvenile percentage
#' labels.
#' @param juvLabel1,juvLabel2 Labels preceding juvenile percentages.
#' @param namesLine1,namesLine2 Line positions for panel labels.
#' @param namesCex1,namesCex2 Character expansion factors for panel labels.
#' @param namesCol1,namesCol2 Colours used for panel labels.
#' @param showNames2 Logical. If \code{TRUE}, displays labels associated with
#' \code{file2}.
#' @param juvAdj Adjustment used for juvenile text placement.
#' @param juvRound Number of decimal places used when displaying juvenile percentages.
#' @param juvSide Margin side used for juvenile annotations.
#' @param juvTextCol1,juvTextCol2 Colours used for juvenile percentage labels.
#' @param cex.axis Character expansion factor for axis labels.
#' @param cex.lab Character expansion factor for axis titles.
#' @param ylab_line Margin line position for the y-axis title.
#' @param namesAdj Adjustment used for panel labels.
#' @param title_text Optional overall figure title.
#' @param title_line Margin line position for the title.
#' @param title_cex Character expansion factor for the title.
#' @param title_col Colour of the title.
#' @param title_font Font used for the title.
#' @param smooth Logical. If \code{TRUE}, smooths length-frequency curves
#' using natural cubic splines.
#' @param oma Numeric vector specifying outer margins passed to
#' \code{\link[graphics]{par}}.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param noDataLabel Text displayed when insufficient data are available for a
#' panel.
#' @param ylabFactor Numeric factor used to rescale y-axis labels without
#' modifying the plotted values.
#' @param relative Logical. If \code{TRUE}, frequencies are converted to
#' percentages within each column before plotting.
#'
#' @details
#' The first column of a matrix or data frame is assumed to contain the
#' length classes. Remaining columns are interpreted as independent
#' length-frequency distributions and are plotted in separate panels.
#'
#' When \code{file2} is provided, both datasets are displayed in the same
#' panels using independent graphical settings. Juvenile percentages are
#' calculated as the proportion of observations below \code{juvLimit}.
#'
#' If \code{relative = TRUE}, frequencies are standardised so that each
#' column sums to 100.
#'
#' @return
#' Invisibly returns \code{NULL}. The function is called for its side effect
#' of producing graphics.
#'
#' @examples
#' data(anchoveta_lengths)
#'
#' lengthFrequencyPlot(
#'   file1 = anchoveta_lengths,
#'   profile = "anchoveta",
#'   juvLimit = 12,
#'   relative = TRUE
#' )
#'
#' @export
lengthFrequencyPlot <- function(
    file1, file2 = NULL, dataFactor = 1, newPlot = FALSE, profile = NULL,
    xlim = NULL, xInterval = 1,
    ylim = c(0, 50), yInterval = NULL,
    ylimList = NULL, yIntervalList = NULL,
    ltys1 = "solid", lwds1 = "1", col1 = "black", alpha1 = 1,
    ltys2 = "solid", lwds2 = "1", col2 = "blue", alpha2 = 1,
    juvLimit = NULL, juvLty = "dotted", juvLwd = 1, juvCol = "red",
    showJuv1 = TRUE, juvLine1 = -2, juvCex1 = 1, juvLabel1 = "juveniles_1 = ",
    showJuv2 = TRUE, juvLine2 = -4, juvCex2 = 1, juvLabel2 = "juveniles_2 = ",
    namesLine1 = -2, namesCex1 = 1, namesCol1 = NULL,
    showNames2 = FALSE, namesLine2 = -2, namesCex2 = 1, namesCol2 = NULL,
    juvAdj = 0.99, juvRound = 0, juvSide = 3, juvTextCol1 = NULL, juvTextCol2 = NULL,
    cex.axis = 1, cex.lab = 1, ylab_line = 3, namesAdj = 0.01,
    title_text = NULL, title_line = 2, title_cex = NULL, title_col = "black", title_font = 1,
    smooth = FALSE, oma = NULL, xlab = NULL, ylab = "Frecuencia (%)",
    noDataLabel = "Sin datos", ylabFactor = 1, relative = TRUE
){

  # Preserve par values before make graphics
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  if(any(is.element(class(file1), c("data.frame", "matrix")))){

    message("First column will be taken as the values of length.")
    file1Names <- list(an(file1[,1]), colnames(file1)[-1])
    file1 <- data.frame(file1[,-1], stringsAsFactors = FALSE)
    dimnames(file1) <- file1Names
  }else if(length(file1) == 1 && is.character(file1) && file.exists(file1)){
    file1 <- read.csv(
      file = file1,
      row.names = 1,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
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
      file2 <- read.csv(
        file = file2,
        row.names = 1,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    if(ncol(file1) != ncol(file2)){
      stop("'file1' and 'file2' must have the same number of columns.")
    }

    if(isTRUE(relative)){
      file2Names <- dimnames(file2)
      file2 <- data.frame(
        sweep(
          x = as.matrix(file2),
          MARGIN = 2,
          STATS = colSums(file2, na.rm = TRUE),
          FUN = "/"
        )*100
      )
      dimnames(file2) <- file2Names
    }

    # file2[file2 <= 0 | is.na(file2)] <- 0

    file2 <- file2*dataFactor

    ltys2 <- rep(x = ltys2, length.out = ncol(file2))
    lwds2 <- rep(x = lwds2, length.out = ncol(file2))
    col2 <- rep(x = col2, length.out = ncol(file2))
    namesCol2 <- if(is.null(namesCol2)) col2 else rep(namesCol2, length.out = ncol(file2))
  }

  if(!is.null(profile)){

    speciesInfo <- ruisu::speciesInfo

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
      xlab <- sprintf(
        fmt = "Longitud %s (%s)",
        speciesInfo$lengthType[index],
        speciesInfo$unit[index]
      )
    }
  }

  file1[file1 <= 0 | is.na(file1)] <- 0

  if(isTRUE(relative)){
    file1 <- sweep(
      x = as.matrix(file1),
      MARGIN = 2,
      STATS = colSums(file1, na.rm = TRUE),
      FUN = "/"
    )*100
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

    plot(
      x = 1, y = 1, type = "n",
      axes = FALSE, xlab = NA, ylab = NA,
      xlim = xlim, ylim = ylim,
      pch = NA,
    )

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

    lines(allLengths, lty = ltys1[i], lwd = lwds1[i],
          col = ifelse(isTRUE(all.equal(alpha1, 1)), col1[i], adjustcolor(col1[i], alpha1)))

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

      lines(allLengths, lty = ltys2[i], lwd = lwds2[i],
            col = ifelse(isTRUE(all.equal(alpha2, 1)), col2[i], adjustcolor(col2[i], alpha2)))

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
        xInterval <- c(1, 0.5)
      }

      if(length(xInterval) == 2){
        axis(side = 1, at = seq(xlim[1], xlim[2], by = xInterval[2]), labels = NA, tcl = -0.25)
      }

      axis(side = 1, at = seq(xlim[1], xlim[2], by = xInterval[1]), cex.axis = cex.axis)
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
