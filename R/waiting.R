
#' @title Function that runs (waits) until a specified time has elapsed
#'
#' @param time_ref Time limit after which the function will end. Internally,
#' \code{as.POSIXct} will be used to convert it to a date-time object.
#' @param time_check How often will \code{time_ref} be checked?
#' @param msg A message to be printed after the reference time has been reached.
#' @param ... Extra arguments passed to \code{as.POSIXct}.
#'
#' @details
#' Internally, \code{waiting} will use the local time (including the time zone).
#' It is important to bear this in mind before defining \code{time_ref}. For
#' example, if the time zone is UTC -05 and our PC is configured with this time
#' zone, the definition of \code{time_ref} should be something like
#' \code{“2025-09-12 09:00:00 -05”}. It is recommended to perform a quick test
#' \strong{before} implementing this function within a process.
#'
#'
#' @export
#'
#' @examples
#' waiting(time_ref = Sys.time() + 5,
#'         time_check = 5,
#'         msg = "\nIt works!\n")
waiting <- function(time_ref, time_check = 60, msg = NULL, ...){

  time_ref <- as.POSIXct(x = time_ref, ...)

  if(time_ref <= Sys.time()) stop("'time_ref' must set after than now.")

  startTime <- Sys.time()
  while(startTime < time_ref){
    Sys.sleep(time = time_check)
    startTime <- Sys.time()
  }

  if(!is.null(msg)){
    paste0(ifelse(test = grepl(x = msg, pattern = "^\\\n"), yes = "", no = "\n"),
           msg,
           ifelse(test = grepl(x = msg, pattern = "\\\n$"), yes = "", no = "\n")) |>

      cat()
  }

  invisible()
}
