#' @title Download environmental data from Copernicus services, extract
#' variables and values, and then add them to a set of coordinates.
#'
#' @description This function takes a table of coordinates-date and makes a
#' selectively download of data (by now, from Copernicus server). Then it adds
#' the dowloaded values to the original table. The selectively process implies
#' that the environmental data will be downloaded for each date (day) just for
#' a section 'around' the coordinates. How 'around' will depend on the user (by
#' default, it is set in 2 degrees grid).
#'
#' @param pts \code{data.frame} or \code{tibble} of coordinates where the user
#' wants to add the environmental variables. See Details.
#' @param credentials A 2-length \code{character} list or vector with credentials
#' (user and password) to access Copernicus server.
#' @param vars \code{character} vector with the names of the variables that will
#' be accessed and added to \code{pts}.
#' @param depth_range 2-length \code{numeric} vector with the range of depths
#' the user wants to download environmental data.
#' @param dx \code{numeric} vector with the Size of the grid that will be used
#' to download environmental data selectively.
#' @param download \code{logical} if \code{TRUE}, allows to download and save
#' the environmental data. Otherwise, each file will be downloaded as temporal
#' file and will be deleted then.
#' @param outDir \code{character} with the directory path where the data will be
#' downloaded. Omitted if \code{download=FALSE}.
#' @param outSufix \code{character} with the suffix that will be added to
#' downloaded files. Omitted if \code{download=FALSE}.
#' @param quite \code{logical} if \code{TRUE}, the function will return progress
#' messages.
#' @param datasetID The dataset ID as it is specified in the Copernicus website.
#' @param datasetVersion The dataset version as it is specified in the
#' Copernicus website.
#' @param service one of the available services using the service name among
#' \code{['arco-geo-series', 'arco-time-series', 'omi-arco', 'static-arco', 'opendap', 'motu']}
#' or its short name among \code{['geoseries', 'timeseries', 'omi-arco', 'static-arco', 'opendap', 'motu']}.
#' @param compression Specify a compression level to apply on the NetCDF output
#' file. A value of 0 means no compression, and 9 is the highest level of
#' compression available. See Details.
#' @param extract_method \code{character}. Method for extracting values with
#' points ("simple" or "bilinear"). With "simple" values for the cell a point
#' falls in are returned. With "bilinear" the returned values are interpolated
#' from the values of the four nearest raster cells. This argument will be
#' transmitted to \link{extract}[terra] function (\code{method} argument).
#'
#' @details \code{pts} needs to have at least three columns called \code{lon},
#' \code{lat} and \code{date}. \code{date} values have to be of class \code{Date}
#' or \code{POSIX}, \code{lon} and \code{lat} must be \code{numeric}.
#'
#' The environmental data will be downloaded always (so it is important to have
#' a stable internet connection), but the user can decide whether to keep or not
#' the files (by the use of \code{download}, \code{outDir} and \code{outSufix}
#' arguments. If \code{download=TRUE}, the file names will preserve the ranges
#' of the downloaded data (date, lon, lat and depth).
#'
#' If \code{service = NULL}, the function will let \code{copernicusmarine}
#' choose the default value. Usually, it should be \code{arco-time-series} but
#' it could depend on the version.
#'
#' @return The \code{pts} object with extra columns corresponding to the
#' selected variables.
#' @export
#'
#' @examples
#' n <- 20
#'
#' set.seed(123)
#'
#' pts1 <- data.frame(lon = runif(n = n, min = 53, max = 98),
#'                   lat = runif(n = n, min = -32, max = -2),
#'                   date = as.Date("2010-1-1"))
#'
#' pts2 <- data.frame(lon = runif(n = n, min = 53, max = 98),
#'                   lat = runif(n = n, min = -32, max = -2),
#'                   date = seq(from = as.Date("2010-1-1"),
#'                              to = as.Date("2010-1-1"),
#'                              by = "day")[as.integer(runif(n = n, min = 1, max = 10))])
#'
#' outs <- downloadEnvir(pts = pts,
#'                       credentials = list(username = "USERNAME", password = "PASSWORD"),
#'                       datasetID = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
#'                       service = "arco-time-series",
#'                       vars = c("thetao", "zos"))
envir_downANDextract <- function(pts, credentials,
                                 datasetID, datasetVersion = NULL, service = NULL,
                                 vars, depth_range = c(5, 10), dx = 2,
                                 download = FALSE, outDir = "./", outSufix = "mercator",
                                 quite = FALSE, compression = FALSE,
                                 extract_method = "simple"){

  # List of breaks and labels values to define selectively download grid
  brksLabs <- list(lon = list(brks = seq(from = -180, to = 180, by = dx),
                              labs = seq(360/dx)),
                   lat = list(brks = seq(from = -90, to = 90, by = dx),
                              labs = seq(180/dx)))

  # Vector of unique dates
  allDates <- unique(pts$date)

  # List for storaging outputs
  output <- list()

  if(is.na(compression)){
    compression <- ""
  }else if(is.logical(compression)){
    if(isTRUE(compression)){
      compression <- "--netcdf-compression-enabled --netcdf-compression-level 2"

      message("\nCompression level was set at level 3 (of 9).")
    }else{
      compression <- ""
    }
  }else if(is.numeric(compression)){
    if(compression > 0){
      compression <- sprintf("--netcdf-compression-enabled --netcdf-compression-level %i",
                             compression)
    }else{
      compression <- ""
    }
  }

  service <- ifelse(test = is.null(service), yes = "",
                    no = sprintf("--service %s", service))

  datasetVersion <- ifelse(test = is.null(datasetVersion), yes = "",
                           no = sprintf("--dataset-version %s", datasetVersion))

  # Start a loop along the dates
  for(i in seq_along(allDates)){

    tempData <- pts |>

      # Filtering only with the date of loop
      filter(date == allDates[i]) %>%

      # Add empty columns to be filled with environmental values
      bind_cols(matrix(data = NA, nrow = nrow(.), ncol = length(vars),
                       dimnames = list(NULL, vars)))

    # Defining positions of each point on grid fro downloading
    gridIndices <- mapply(function(x, y) cut(x = x, breaks = y$brks, labels = y$labs),
                          x = as.list(dplyr::select(tempData, lon, lat)), y = brksLabs,
                          SIMPLIFY = FALSE) |>

      # get positions as numeric vector
      lapply(function(x) as.numeric(as.character(x))) |>

      # convert to tibble
      as_tibble() |>

      # keep grids without duplicates
      distinct() |>

      # sorting grids
      arrange(lat, lon)

    index <- list()
    for(j in unique(gridIndices$lat)){

      tempIndex <- gridIndices |>

        filter(lat == j) |>

        mutate(group = 1)

      if(nrow(tempIndex) > 1){
        for(k in seq(2, nrow(tempIndex))){
          tempIndex$group[k:nrow(tempIndex)] <- tempIndex$group[k] + ifelse(diff(tempIndex$lon[c(k - 1, k)]) > 1, 1, 0)
        }
      }

      index[[length(index) + 1]] <- tempIndex |>

        group_by(group) |>

        summarise(lon = brksLabs$lon$brks[c(min(lon), max(lon))] + c(0, dx),
                  lat = brksLabs$lat$brks[c(min(lat), max(lat))] + c(0, dx),
                  .groups = "drop") %>%

        mutate(what = rep(x = c("min", "max"), length.out = nrow(.))) |>

        pivot_longer(cols = lon:lat) |>

        pivot_wider(names_from = c(what, name))
    }

    index <- index |>

      bind_rows() |>

      dplyr::select(-group)


    for(j in seq(nrow(index))){

      if(isTRUE(download)){
        outFile <- file.path(outDir,
                             paste(outSufix,
                                   allDates[i],
                                   with(index[j,],
                                        sprintf("lon [%s ; %s], lat [%s ; %s], depth [%s ; %s].nc",
                                                min_lon, max_lon, min_lat, max_lat,
                                                depth_range[1], depth_range[2])),
                                   sep = " -- "))

        if(!dir.exists(outDir)) dir.create(path = outDir, showWarnings = FALSE, recursive = TRUE)
      }else{
        outFile <- paste0(tempfile(), ".nc")
      }

      cmd <- paste("copernicusmarine subset --force-download --overwrite-output-data",
                   "--dataset-id", datasetID,
                   paste(paste("--variable", vars), collapse = " "),
                   "--start-datetime", format(x = allDates[i], format = "%FT%00:00:00"),
                   "--end-datetime", format(x = allDates[i], format = "%FT%23:59:59"),
                   "--minimum-longitude", index$min_lon[j],
                   "--maximum-longitude", index$max_lon[j],
                   "--minimum-latitude", index$min_lat[j],
                   "--maximum-latitude", index$max_lat[j],
                   "--minimum-depth", depth_range[1],
                   "--maximum-depth", depth_range[2],
                   "--output-directory", dirname(outFile),
                   "--output-filename", basename(outFile),
                   "--username", credentials$username,
                   "--password", credentials$password,
                   datasetVersion, compression, service)

      system(cmd)

      for(k in seq_along(vars)){
        envirData <- terra::rast(x = outFile, subds = vars[k], lyrs = 1)

        varIndex <- tempData |>

          with(between(lon, index$min_lon[j], index$max_lon[j]) &
                 between(lat, index$min_lat[j], index$max_lat[j]))

        colVar <- match(vars[k], colnames(tempData))
        tempData[varIndex, colVar] <- terra::extract(x = envirData,
                                                     y = tempData[varIndex, c("lon", "lat")],
                                                     method = extract_method)[,-1]
      }

      if(!isTRUE(download)) unlink(x = outFile)

      cat("\n----------------------------------\n")
    }

    output[[length(output) + 1]] <- tempData

    if(!isTRUE(quite)){
      cat(sprintf("\n------------ Completed %04d/%04d (%s%%) [%s] ------------\n\n",
                  i, length(allDates), round(i/length(allDates)*100, 2),
                  as.character(Sys.time())))
    }
  }

  return(bind_rows(output))
}
