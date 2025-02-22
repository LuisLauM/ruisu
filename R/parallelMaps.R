#' @title Make plots with parallel effect
#'
#' @description
#' This function takes a \code{data.frame} of coordinates and uses a 'group'
#' column to make a plot placing the points in a parallel way.
#'
#'
#' @param x \code{data.frame} with coordinates and group column.
#' @param overlay \code{numeric} value indicating the level of overlay between
#' subplots (\eqn{[0; 0.5>}).
#' @param direction \code{numeric} On which side of the plot (1 = bottom,
#' 2 = left, 3 = top, 4 = right) the subplots will be placed?
#' @param labels.args Extra arguments for \link[graphics]{mtext} that is used to
#' specify the labels of groups. If \code{NULL}, labels will not be plotted.
#' @param map.args Extra arguments for \link[maps]{map} that is used to add map
#' shapes to subplots. If \code{NULL}, map lines will not be plotted.
#' @param final.par \code{list} with graphical settings applied to the final
#' plot. See the available options in \link[graphics]{par}.
#' @param saveFig \code{logical} Do you want to save final plot as a .png file?
#' If \code{saveFig = TRUE} (default), the plot will not be showed.
#' @param png.args \code{logical} If \code{saveFig = TRUE}, extra arguments
#' passed to \link[grDevices]{png}.
#' @param extra.args Extra arguments passed to some internal steps. See Details.
#' @param xlim,ylim \code{numeric} Range of subplots in X and Y axis.
#' @param col,pch,cex Features that will vary by groups.
#' @param interpolate a \code{logical} vector (or scalar) indicating whether to
#' apply linear interpolation to the image when drawing. Argument passed from
#' \link[graphics]{rasterImage}.
#' @param show.axis.labels \code{logical} that defines if the axis labels will
#' be plotted.
#' @param pos.axis.labels If \code{show.axis.labels = TRUE}, this arguments
#' indicates the position of the axis labels.
#' @param axis.labels.args If \code{show.axis.labels = TRUE}, extra arguments
#' passed to \link[graphics]{axis}.
#' @param quiet \code{logical} value to control informative messages.
#' @param extra.elements.bf,extra.elements.af Adicional elements could be
#' included in the subplots (by group) by this argument: \code{extra.elements.bf}
#' for including elements before drawing the points, otherwise
#'  \code{extra.elements.af}. See Details.
#' @param ... Extra arguments passed to \link{plot} that makes subplots.
#'
#' @details
#' \code{x} must be defined as a \code{data.frame} with at least 3 columns: lon,
#' lat and group. It is recommended that to define \strong{group} as a factor,
#' otherwise, it will be defined internaly as it using \link{as.factor}.
#'
#' This function plots and saves internaly one figure per group. The quality of
#' this subplots can be modified by \code{extra.args} setting \code{res}
#' (default 200) and \code{width} in pixels (default 2500).
#'
#' \code{extra.elements.bf} and \code{extra.elements.af} receive pieces of code
#' that will be placed and run before or after the drawing of points. The order
#' of drawing is an empty canvas, the \code{extra.elements.bf}-elements, the
#' points, the map, the labels of groups, and the
#' \code{extra.elements.af}-elements.
#'
#'
#' @return Depending on \code{saveFig} and \code{plot} values, this function
#' will return a plot
#' @export
#'
#' @examples
#' data(coord_example)
#'
#' parallelMaps(x = coord_example, direction = 2, overlay = 0.1,
#'              xlim = c(-82, -74), ylim = c(-16, -4),
#'              labels.args = list(adj = 0.5, side = 1),
#'              col = adjustcolor(c("blue", "red", "forestgreen", "gold2"), 0.5),
#'              pch = c(15, 16, 17, 18),
#'              map.args = list(database = "worldHires"),
#'              extra.elements.bf = {
#'                abline(h = -6, col = "purple")
#'                abline(h = -14, col = "gold4")})
parallelMaps <- function(x, overlay = 0, direction = 2,
                         labels.args = list(),
                         map.args = list(),
                         final.par = list(),
                         saveFig = FALSE,
                         png.args = list(),
                         extra.args = list(res = 450, width = 2000),
                         xlim = NULL, ylim = NULL,
                         col = "black", pch = 1, cex = 1,
                         interpolate = FALSE,
                         show.axis.labels = TRUE,
                         pos.axis.labels = ifelse(direction %% 2 == 0, 2, 1),
                         axis.labels.args = list(),
                         quiet = FALSE,
                         extra.elements.bf = NULL,
                         extra.elements.af = NULL,
                         ...){

  # Save initial date-time
  if(!isTRUE(quiet)){
    time_0 <- Sys.time()
  }

  if(!is.data.frame(x) || !all(is.element(c("lon", "lat", "group"), colnames(x)))){
    # Check 'x'
    stop("'x' must be a data.frame with columns 'lon', 'lat' and 'group'.")
  }else{

    # Lower case column names of 'x'
    colnames(x) <- tolower(colnames(x))


    x <- x %>%

      # Create a data frame from x just with desirable columns
      transmute(lon, lat, group = as.factor(group))
  }

  # Define xlim and ylim
  if(is.null(xlim)) xlim <- range(x$lon, na.rm = TRUE)
  if(is.null(ylim)) ylim <- range(x$lat, na.rm = TRUE)

  # Calculate dimensions of subplots
  dims <- round(x = extra.args$width*c(1, diff(ylim)/diff(xlim)), digits = 0)

  # Check value of direction
  if(all(!sapply(1:4, function(x) isTRUE(all.equal(target = x, current = direction))))){
    stop("'direction' must be a numeric value: 1 (bottom), 2 (left), 3 (top) or 4 (right).")
  }

  # Get levels of group columns
  allGroups <- levels(x$group)

  # If 'direction' is 2 or 3, revert order
  if(is.element(direction, 2:3)){
    allGroups <- rev(allGroups)
  }

  # Index to define of direction is vertical or not (horizontal)
  verDir <- is.element(direction, c(1, 3))

  # Check overlay value
  if(!is.numeric(overlay) || length(overlay) != 1 || overlay < 0 || overlay >= 0.5){
    stop("'overlay' must a numeric value between 0 and 0,5 (not included).")
  }

  # Important proportions
  # a: Total
  # b: a*overlay
  # c: a - b
  # d: a - 2*b
  # e: # Groups
  proportions <- list(a = dims[ifelse(verDir, 2, 1)],
                      b = NA,
                      c = NA,
                      d = NA,
                      e = length(allGroups))

  proportions$b <- with(proportions, floor(a*overlay))
  proportions$c <- with(proportions, a - b)
  proportions$d <- with(proportions, a - 2*b)

  # Define a list for point features
  ptsFeatures <- list(col = col, pch = pch, cex = cex) %>%

    lapply(rep, length.out = length(allGroups))

  # Run a loop along groups
  finalPlot <- list()
  for(i in seq_along(allGroups)){

    # Define a temporal path for subplots
    tempFilename <- tempfile(fileext = ".png")

    # Starting figure saving
    png(filename = tempFilename, width = dims[1], height = dims[2],
        res = extra.args$res, bg = "transparent")

    # Define graphical parameters
    par(mar = rep(0, 4), xaxs = "i", yaxs = "i")

    # Empty canvas
    plot(1, 1, type = "n", axes = FALSE, xlab = NA, ylab = NA,
         xlim = xlim, ylim = ylim, asp = 1)

    # Extra elements (before)
    eval(substitute(extra.elements.bf))

    # Take x...
    x %>%

      # ...filter group
      filter(group == allGroups[i]) %>%

      # ...plotting coordinates
      with(points(x = lon, y = lat,
                  col = ptsFeatures$col[i],
                  pch = ptsFeatures$pch[i],
                  cex = ptsFeatures$cex[i],
                  ...))

    # Add map
    if(!is.null(map.args) && is.list(map.args)){

      # Combine arguments for map
      map.args <- list(database = "world",
                       interior = FALSE) %>%

        modifyList(val = map.args) %>%

        modifyList(val = list(add = TRUE))

      # Add map
      do.call(what = map, args = map.args)
    }

    # Add group labels
    if(!is.null(labels.args) && is.list(labels.args)){

      # Combine arguments for labels (mtext)
      labels.args <- list(side = ifelse(verDir %% 2 == 0, 3, 2),
                          line = -1) %>%

        modifyList(val = labels.args) %>%

        modifyList(val = list(text = allGroups[i]))

      # Add group labels
      do.call(what = mtext, args = labels.args)
    }

    # Extra elements (after)
    eval(substitute(extra.elements.af))

    # Ending file saving
    dev.off()

    # Load subplot
    tempPlot <- readPNG(source = tempFilename)

    # Delete temporal file
    unlink(x = tempFilename)

    if(i == 1){
      # If i == 1, subsection type 1
      index <- seq(proportions$c)
      finalPlot[[length(finalPlot) + 1]] <- if(verDir) tempPlot[index,,] else tempPlot[,index,]
    }else{

      # Subsection type 2
      index1 <- seq(to = proportions$a, length.out = proportions$b, by = 1)
      index2 <- seq(from = 1, length.out = proportions$b, by = 1)

      tempMat1 <- list(if(verDir) prevPlot[index1,, 1:3] else prevPlot[,index1, 1:3],
                       if(verDir) tempPlot[index2,, 1:3] else tempPlot[,index2, 1:3]) %>%

        lapply(as.numeric) %>%

        do.call(what = pmin) %>%

        array(dim = c(dims[ifelse(verDir, 1, 2)], proportions$b, 3))

      # tempMat1 <- abind(if(verDir) prevPlot[index1,, 1:3] else prevPlot[,index1, 1:3],
      #                   if(verDir) tempPlot[index2,, 1:3] else tempPlot[,index2, 1:3],
      #                   along = 4) %>%
      #
      #   apply(MARGIN = 3, FUN = \(x) apply(x, 1:2, min), simplify = FALSE) %>%
      #
      #   abind(along = 3)

      if(verDir){
        tempMat2 <- prevPlot[index1,, 4] + tempPlot[index2,, 4]
      }else{
        tempMat2 <- prevPlot[,index1, 4] + tempPlot[,index2, 4]
      }

      finalPlot[[length(finalPlot) + 1]] <- abind(tempMat1, pmin(tempMat2, 1), along = 3)

      if(i != length(allGroups)){
        # Subsection type 3
        index <- seq(from = proportions$b + 1, length.out = proportions$d)
        finalPlot[[length(finalPlot) + 1]] <- if(verDir) tempPlot[index,,] else tempPlot[,index,]
      }else{
        # Subsection type 4
        index <- seq(to = proportions$a, length.out = proportions$c, by = 1)
        finalPlot[[length(finalPlot) + 1]] <- if(verDir) tempPlot[index,,] else tempPlot[,index,]
      }
    }

    # Save plot as previous plot
    if(i == length(allGroups)){
      suppressWarnings(rm(list = c("tempPlot", "prevPlot", "tempMat1", "tempMat2")))
    }else{
      prevPlot <- tempPlot
    }
  }

  # Combine arrays of subsections
  finalPlot <- abind(finalPlot, along = ifelse(verDir, 1, 2))

  if(isTRUE(saveFig)){

    # Combine png arguments for final plot
    png.args <- list(res = extra.args$res,
                     bg = "transparent") %>%

      modifyList(val = png.args) %>%

      modifyList(val = list(width = dim(finalPlot)[2],
                            height = dim(finalPlot)[1]))

    # Starting final figure saving
    do.call(what = png, args = png.args)
  }else{
    # Saving par for restoring at the end
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  # Defining graphical parameters
  list(xaxs = "i", yaxs = "i", oma = rep(0, 4),
       mar = c(if(verDir %% 2 == 0) c(1, 3) else c(3, 1), 1, 1)) %>%

    modifyList(val = final.par) %>%

    par

  # Defining values for canvas plot
  plotVals <- list(x = if(verDir) xlim else xlim - c(dim(finalPlot)[2]*diff(xlim)/proportions$a, 0),
                   y = if(verDir) ylim - c(dim(finalPlot)[1]*diff(ylim)/proportions$a, 0) else ylim)

  # Plotting empty canvas
  plot(x = plotVals$x, y = plotVals$y, type = "n", axes = FALSE,
       xlab = NA, ylab = NA)

  # Add final figure
  rasterImage(image = finalPlot,
              xleft = plotVals$x[1], ybottom = plotVals$y[1],
              xright = plotVals$x[2], ytop = plotVals$y[2],
              interpolate = interpolate)

  if(isTRUE(show.axis.labels)){

    # Defining values for axis labels
    axisLabs <- pretty(x = if(verDir) xlim else ylim)
    counterDir <- switch(direction, '1' = 3, '2' = 4, '3' = 1, '4' = 2)

    # Combining arguments for axis labels
    axis.labels.args <- list(side = if(verDir) 1 else 2,
                             at = axisLabs, las = 1,
                             labels = if(verDir) LonLabel(axisLabs) else LatLabel(axisLabs)) %>%

      modifyList(val = axis.labels.args)

    # Adding axis labels
    do.call(what = axis, args = axis.labels.args)
  }

  # Add box
  box()

  # Ending final figure saving
  if(isTRUE(saveFig)) dev.off()

  # Show informative messages
  if(!isTRUE(quiet)){

    xyLabels <- mapply(x = list(x = xlim, y = ylim),
                       f = list(metR::LonLabel, metR::LatLabel),
                       FUN = \(x, f, ...) paste(f(round(x, ...)), collapse = " - "),
                       MoreArgs = list(digits = 1),
                       SIMPLIFY = FALSE)

    cat(sprintf("\nxlim: [%s]\nylim: [%s]\n", xyLabels$x, xyLabels$y))
    cat(sprintf("\nSubplots' dimensions: %d x %d", dims[1], dims[2]))
    cat(sprintf("\nFinal figure dimensions: %d x %d\n",
                nrow(finalPlot), ncol(finalPlot)))
    cat(sprintf("\nProportion of overlay: %2.1 %%", proportions$overlay*100))

    if(isTRUE(saveFig)) cat(sprintf("Figured saved on: %s\n", png.args$filename))

    time_1 <- Sys.time()
    procTime <- difftime(time1 = time_1, time2 = time_0, units = "secs") %>% as.integer

    procTime <- c(procTime %/% 60^2, procTime %/% 60, procTime %% 60)

    cat(sprintf("\nProcessing time: %02d h %02d m %02d s\n",
                procTime[1], procTime[2] - procTime[1]*60, procTime[3]))
  }

  rm(finalPlot)

  return(invisible())
}
