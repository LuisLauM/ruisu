#' @title Password generator
#'
#' @description
#' A quite simple password generator.
#'
#'
#' @param len \code{numeric} Total length of the required password.
#' @param numbers,special_chars,upper_letters \code{logical} Do you want to
#' include some of this elements in your password? \code{TRUE} by default.
#'
#' @details
#' If the user just specifies the lenght, \code{passwordGen} will return a
#' password composed with 20% of numbers, 20% of special characters, 30% of
#' lower-case and 30% of upper-case letters.
#'
#' \code{passwordGen} will generate one password at time. If you want to
#' generate several passwords, you can use \code{sapply(rep(7, 10), passwordGen)}
#' to generate in that case 10 passwords of length 7.
#'
#'
#' @return A character vector of length 1.
#' @export
#'
#' @examples
#' passwordGen(len = 10)
passwordGen <- function(len = 10, numbers = TRUE, special_chars = TRUE, upper_letters = TRUE){

  if(!is.numeric(len) || length(len) != 1 || len < 5){
    stop("'len' must be a numeric vector of length 1 and greater than 5.")
  }

  allLens <- c(numbers       = ifelse(isTRUE(numbers), 0.2, 0),
               special_chars = ifelse(isTRUE(special_chars), 0.2, 0),
               upper_letters = ifelse(isTRUE(upper_letters), 0.3, 0),
               lower_letters = 0.3)

  allLens <- round(allLens*len, 0)

  if(sum(allLens) != len){
    index <- which.max(allLens)
    allLens[index] <- allLens[index] + (len - sum(allLens))
  }


  mapply(x = list(numbers       = 0:9,
                  special_chars = unlist(strsplit(x = "!#$%&()*+,-.:;<=>?@[]^_{|}~",
                                                  split = "")),
                  upper_letters = LETTERS,
                  lower_letters = letters),
         size = allLens,
         FUN = sample,
         MoreArgs = list(replace = TRUE)) %>%

    do.call(what = c) %>%

    sample(size = length(.)) %>%

    paste(collapse = "")
}
