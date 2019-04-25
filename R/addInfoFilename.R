#' Add extra info to file
#'
#' @param albumFolder Folder where the files are located.
#' @param extraInfo String with the extra info that will be located at the end of the file.
#' @param musicExtension Extension of the files that will be matched for the substitution.
#'
#' @return \code{NULL}. This function just makes changes on filen names.
#' @export
addInfoFilename <- function(albumFolder, extraInfo, musicExtension = ".mp3"){

  fileList <- list.files(path = albumFolder, pattern = musicExtension, full.names = TRUE)

  myDirnames <- dirname(fileList)
  myFilenames <- substr(basename(fileList), start = 1, stop = nchar(basename(fileList)) - nchar(musicExtension))
  myFilenames <- paste0(myFilenames, " ", extraInfo, musicExtension)

  myFilenames <- file.path(myDirnames, myFilenames)

  print(basename(myFilenames))

  cat("\nConfirm these new names' (y/n): ")
  answer <- tolower(scan(what = character(), nmax = 1, quiet = TRUE))

  while(!is.element(answer, c("y", "n"))){
    cat("\nPlease, just write Y or N.\n")
    cat("\nConfirm these new names' (y/n): ")
    answer <- tolower(scan(what = character(), nmax = 1, quiet = TRUE))
  }

  if(answer == "y"){
    file.rename(from = fileList, to = myFilenames)
  }

  return(invisible())
}
