#' @title Easy way to prepare a folder for a typical R Project
#'
#' @param folder \code{character}. Path of the folder that will be created.
#' @param type Indicates the type of sub directories that will be created (see Details).
#' @param folderList \code{character} vector which can be used to indicate the folders that will be
#' created inside the project folder.
#' @param addRProj If \code{TRUE} (default), it will load a RStudio project inside the \code{folder}.
#' @param openAtFinish If \code{TRUE} (default), the RStudio project will be opened in a RStudio window.
#'
#' @details You can indicate the set of directories that will be created using \code{type} as follows:
#' \itemize{
#'  \item{1: }{code, data, figures, outputs}
#'  \item{2: }{code, data, figures, outputs, presentations}
#'  \item{3: }{code, data, figures, outputs, presentations, docs}
#'  \item{4: }{code, data, figures, outputs, presentations, docs, bib}
#'  \item{5: }{code, data, figures, outputs, presentations, docs, bib, raw}
#' }
#'
#' The function will creates folder recursively, so the non created folders (or sub folders) specified
#' in 'folder' will be also created.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prepareProjFolder(folder = "../exampleFolder", type = 3)
#' }
prepareProjFolder <- function(folder, type = 1, folderList = NULL, addRProj = TRUE, openAtFinish = TRUE){

  if(!is.null(folderList)){
    if(!is.character(folderList) | length(folderList) < 1){
      stop("'folderList' must be a character vector.")
    }
  }else{
    folderList <- switch(type,
                         "1" = c("code", "data", "figures", "outputs"),
                         "2" = c("code", "data", "figures", "outputs", "presentations"),
                         "3" = c("code", "data", "figures", "outputs", "presentations", "docs"),
                         "4" = c("code", "data", "figures", "outputs", "presentations", "docs", "bib"),
                         "5" = c("code", "data", "figures", "outputs", "presentations", "docs", "bib", "raw"),
                         "Incorrect value for 'type'.")
  }

  lastChar <- substr(x = folder, start = nchar(folder), stop = nchar(folder))
  folder <- substr(x = folder, start = 1, stop = nchar(folder) - ifelse(lastChar == "/", 1, 0))

  sapply(file.path(folder, folderList), dir.create, showWarnings = FALSE, recursive = TRUE)

  if(isTRUE(addRProj)){
    initializeProject(path = folder)

    if(isTRUE(openAtFinish)){
      openProject(path = folder, newSession = TRUE)
    }
  }

  return(invisible())
}
