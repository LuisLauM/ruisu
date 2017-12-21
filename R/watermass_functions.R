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

#' Title
#'
#' @param x A \code{data.frame} containing the variables required by the indicated \code{criteria}.
#' @param criteria A \code{chracter} indicating the method used for the definition.
#' @param newCriteria A \code{list} with information for a new watermass definition. See Details.
#' @param ... Extra arguments (at moment, with no sense).
#'
#' @return A \code{factor} vector with the definitions.
#'
#' @details The \code{newCriteria} argument allows the users to define their own criteria for classify the
#' water masses. To do that, \code{newCriteria} must be a list with values of ranges and definitions, just
#' like \code{swartzman2008_ranges} and \code{swartzman2008_definitions} objects.
getWatermasses <- function(x, criteria = "swartzman2008", newCriteria = NULL, ...){

  if(!is.null(newCriteria)){
    if(!is.list(newCriteria) || length(newCriteria) != 2){
      stop("newCriteria must be a list with ranges and definitions.")
    }else{
      criteriaRanges <-newCriteria$ranges
      criteriaDefinitions <- newCriteria$definitions
    }
  }else{
    criteriaRanges <- watermassDefinitions[[criteria]]$ranges
    criteriaDefinitions <- watermassDefinitions[[criteria]]$definitions
  }

  colnames(x) <- tolower(colnames(x))

  selectedDefinition <- getDefinitions(ranges = criteriaRanges, definitionTable = criteriaDefinitions)
  criteriaLevels <- sort(unique(selectedDefinition$watermass))

  if(!all(is.element(colnames(selectedDefinition)[-1], colnames(x)))){
    stop("x and the selected criteria have different variable names.")
  }else{
    x <- x[,colnames(selectedDefinition)[-1]]
  }

  output <- NULL
  for(i in 1:ncol(x)){
    output <- cbind(output,
                    anc(cut(x = x[[i]], breaks = criteriaRanges[[i]], labels = seq(length(criteriaRanges[[i]]) - 1))))

  }

  index <- match(apply(output, 1, paste, collapse = "-"), apply(selectedDefinition[,-1], 1, paste, collapse = "-"))

  return(factor(x = selectedDefinition$watermass[index], levels = criteriaLevels, labels = criteriaLevels))
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
