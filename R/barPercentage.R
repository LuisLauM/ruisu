#' Pretty customizable barplot
#'
#' @param x Numeric vector for plotting. See Details.
#' @param cols Color palette used for plotting.
#' @param lwd Line width.
#' @param digits Number of digits for rounding.
#' @param val_perc Whether to show values and/or percentage in labels.
#' @param titleLab Label for title.
#' @param textPos A position specifier for the text.
#' @param sameColLines \code{logical} Do you prefer that the color of lines and
#' the bar sections will be the same?
#' @param tagPosDelay Relative origin of guide lines expresed in a single number
#' from -1 to 1.
#'
#' @details \code{x} must be numeric and the names of values will be used for
#' legend labels. If the vector does not have names, a temporal names will be
#' created on the form of 'Value_#'.
#'
#' @return Nothing but the pretty barplot.
#' @export
#'
#' @examples
#' barPercentage(x = c(55, 14, 10, 2))
barPercentage <- function(x, cols = NULL, lwd = 2, digits = 1,
                          val_perc = c(1, 2), titleLab = NA, textPos = 2,
                          sameColLines = TRUE, tagPosDelay = 0){
  # If x does not have names, create temporal ones
  if(is.null(names(x))) names(x) <- paste0("Value_", seq_along(x))

  x <- list(values = x,
            percentage = x/sum(x, na.rm = TRUE))

  # Preserve a copy of x values
  x2 <- x

  if(is.null(cols)) cols <- tim.colors(length(x$values))

  if(tagPosDelay > 1 || tagPosDelay < -1) stop("'tagPosDelay' must be [-1, 1].")

  textPos <- rep(x = textPos, length.out = length(x$percentage))

  if(sum(!is.element(textPos, c(2, 4))) > 0) stop("'textPos' must be a vector of values 2 or 4.")

  ylim <- c(-sum(is.element(textPos, 2))*0.5, 1.4 + sum(is.element(textPos, 4))*0.5)

  par(mar = c(0, 0.5, ifelse(is.na(titleLab), 0.5, 1.2), 0.5), xaxs = "i", yaxs = "i")

  barplot(as.matrix(x$percentage), horiz = TRUE, col = cols, border = NA, axes = FALSE,
          xlab = NA, ylab = NA, ylim = ylim, xlim = c(0, 1))

  tagPosDelay <- diff(cumsum(c(0, x$percentage)))*tagPosDelay/2
  tagPos <- rollmean(x = cumsum(c(0, x$percentage)), 2) + tagPosDelay

  pos2 <- 1
  pos4 <- sum(textPos == 4)
  for(i in seq_along(x$percentage)){

    yPos <- ifelse(textPos[i] == 4, 1.2, 0.2) + 0.5*ifelse(textPos[i] == 4, pos4, -pos2)

    lines(x = rep(tagPos[i], 2), y = c(0.25, yPos), lwd = lwd,
          col = ifelse(isTRUE(sameColLines), cols[i], "black"))

    tempLabels <- paste0(if(is.element(1, val_perc)) paste0("[", x$values[i], "]") else "",
                         if(is.element(2, val_perc)) paste0("[", round(x$percentage[i]*100, digits = digits), "%]") else "",
                         " ", names(x$percentage)[i])

    text(x = tagPos[i], y = yPos, pos = textPos[i], labels = tempLabels)

    pos2 <- pos2 + ifelse(textPos[i] == 2, 1, 0)
    pos4 <- pos4 - ifelse(textPos[i] == 2, 0, 1)
  }

  mtext(text = titleLab, side = 3, line = 0.2, cex = 1.2, font = 2)

  return(invisible())
}
