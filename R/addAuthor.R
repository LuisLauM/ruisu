#' Add artist name to (music) file
#'
#' @param albumFolder Folder where the files are located.
#' @param artistName Name of the artist. It will be located at the beginning of the file.
#' @param deleteSpaces Number of chracters that will be removed in order to replace with \code{artistName}.
#' @param musicExtension Extension of the files that will be matched for the substitution.
#'
#' @return \code{NULL}. This function just makes changes on filen names.
#' @export
addAuthor <- function(albumFolder, artistName, deleteSpaces, musicExtension = ".mp3"){

  fileList <- list.files(path = albumFolder, pattern = musicExtension, full.names = TRUE)

  myDirnames <- dirname(fileList)
  myFilenames <- substr(basename(fileList), start = deleteSpaces + 1, stop = 1e3)
  myFilenames <- paste(artistName, "-", myFilenames)

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
