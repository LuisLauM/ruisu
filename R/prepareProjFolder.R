#' prepareProjFolder
#'
#' @param folder
#' @param type
#' @param deleteAll
#'
#' @return
#' @export
#'
#' @examples
prepareProjFolder <- function(folder, type = 1){

  folderList <- switch(type,
                       "1" = c("code", "data", "figures", "outputs"),
                       "2" = c("code", "data", "figures", "raw", "outputs", "bib"),
                       "3" = c("bib", "drafts", "presentations", "results/raw", "results/data", "results/figures",
                             "results/code/figures", "results/code/analysis"),
                       "Incorrect value for 'type'.")

  sapply(file.path(folder, folderList), dir.create, showWarnings = FALSE, recursive = TRUE)

  return(invisible())
}
