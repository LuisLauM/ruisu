#' ATM banknotes' optimizer algorithm
#'
#' @param amount The amount to optimize.
#' @param d_Banknotes Denomination of banknotes.
#' @param n_Banknotes Quantity of available banknotes.
#' @param d_symbol Symbol of banknote currency.
#'
#' @details For now, the algorithm compares all the possible solutions, so it is a brute-force search.
#'
#' @return A vector with the quantity of banknotes.
#' @export
#'
#' @examples
#' amount <- 70
#'
#' d_Banknotes <- c(20, 50, 100, 200)
#' n_Banknotes <- rep(10, length(d_Banknotes))
#'
#' notebanks_ATM(amount = amount, d_Banknotes = d_Banknotes,
#'               n_Banknotes = n_Banknotes, d_symbol = "S/")
notebanks_ATM <- function(amount, d_Banknotes, n_Banknotes, d_symbol = "US$"){
  # Default output if no solutions were found
  defaultOut <- numeric(length(d_Banknotes))
  names(defaultOut) <- d_Banknotes

  # Make a table with all posible solutions
  out <- amount %/% d_Banknotes
  out <- expand.grid(sapply(out, seq, from = 0))

  # Calculate the sum of amounts for each solution
  outSums <- sweep(x = out, MARGIN = 2,  STATS = d_Banknotes, FUN = "*")

  # Which solutions are equal to the amount?
  index <- rowSums(outSums) == amount

  # If any solution is equal to amount, it returns a message
  if(sum(index) == 0){
    message("\nThe amount cannot be divided by banknotes of ",
            paste(paste0(d_symbol, d_Banknotes), collapse = ", "), ".\n")

    return(defaultOut)
  }else{
    out <- out[index,]
  }

  # Which solutions no exceed the quantity of notebanks?
  index <- rowSums(sweep(x = out, MARGIN = 2, STATS = n_Banknotes, FUN = ">")) == 0

  if(sum(index) == 0){
    message("\nNot enough banknotes to achive the amount.\n")
    return(defaultOut)
  }else{
    out <- out[index,]
  }

  # Weight the solutions accordng to the denomination of notebanks
  index <- sweep(x = out, MARGIN = 2, STATS = rev(d_Banknotes), FUN = "*")
  index <- which.min(rowSums(index))

  # Prepare output
  out <- as.numeric(out[index,])
  names(out) <- d_Banknotes

  return(out)
}
