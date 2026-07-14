#' @title Identify Ports from Free-Text Descriptions
#'
#' @description
#' Matches free-text location descriptions against a table of regular
#' expression patterns corresponding to ports and returns the associated
#' metadata.
#'
#' The function standardizes the input by converting it to uppercase,
#' collapsing multiple spaces, trimming leading and trailing spaces,
#' and transliterating accented Latin characters to their ASCII
#' equivalents before performing the pattern matching.
#'
#' Matching is performed using regular expressions stored in
#' \code{patron_puertos$REGEX}. If multiple patterns match the same input,
#' all matching records are returned.
#'
#' @param x A character vector containing the text descriptions to be
#'   matched.
#' @param patron_puertos A data frame containing the lookup table of
#'   ports. It must include a column named \code{REGEX} with the regular
#'   expressions used for matching, together with the metadata columns
#'   to be returned.
#' @param out_vars A character vector indicating which columns from
#'   `patron_puertos` should be returned. Defaults to
#'   \code{c("LABORATORIO", "ZONA", "DEPARTAMENTO", "LATITUD", "LONGITUD")}.
#'
#'   If `NULL` or a vector of length zero is supplied, the function
#'   returns the matched port names (\code{LUGAR}).
#' @param mostrar_no_encontrados Logical. If \code{TRUE}, a message is
#'   displayed listing the input values that could not be matched to
#'   any pattern.
#'
#' @return
#' If \code{out_vars} is not \code{NULL} and contains at least one variable, a
#' tibble with one row per element of `x` and the requested output
#' variables.
#'
#' If \code{out_vars} is \code{NULL} or empty, a character vector containing the
#' matched port names (\code{LUGAR}).
#'
#' Unmatched values are returned as \code{NA}.
#'
#' @details
#' Before matching, the input strings undergo the following
#' preprocessing steps:
#'
#' \itemize{
#'   \item Conversion to uppercase.
#'   \item Replacement of multiple internal spaces with a single space.
#'   \item Removal of leading and trailing spaces.
#'   \item Transliteration of accented Latin characters to their ASCII
#'   equivalents.
#' }
#'
#' @export
#'
#' @examples
#' identifica_puertos(
#'   x = c("Tambo de mora", "T. de mora", "Paita - DPA nuevo")
#' )
identifica_puertos <- function(
  x, patron_puertos = NULL,
  out_vars = c(
    "LABORATORIO", "ZONA", "DEPARTAMENTO",
    "LATITUD", "LONGITUD"
  ),
  mostrar_no_encontrados = FALSE
){

  xu <- tibble(x_orig = unique(x)) |>

    mutate(
      x_old = toupper(x_orig),

      x_old = gsub(
        x = x_old,
        pattern = "[[:blank:]]{2,}",
        replacement = " "
      ),

      x_old = gsub(
        x = x_old,
        pattern = "^[[:blank:]]{2,}",
        replacement = ""
      ),

      x_old = gsub(
        x = x_old,
        pattern = "[[:blank:]]{2,}$",
        replacement = ""
      ),

      x_old <- stri_trans_general(str = x_old, id = "Latin-ASCII")
    ) |>

    filter(!is.na(x_old))

  if(is.null(patron_puertos)){
    patron_puertos <- ruisu::patron_puertos
  }else{
    colnames(patron_puertos) <- toupper(colnames(patron_puertos))

    checkCols <- colnames(ruisu::patron_puertos) |>

      setdiff(y = colnames(patron_puertos))

    if(length(checkCols) > 0)
      cli_abort(
        "{.code patron_puertos} must contain at least the columns {colnames(ruisu::patron_puertos)}."
      )
  }

  allTests <- expand.grid(
    x = xu$x_old,
    pattern = patron_puertos$REGEX,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  ) |>

    mutate(
      status = mapply(
        x = x,
        pattern = pattern,
        FUN = grepl,
        MoreArgs = list(ignore.case = TRUE),
        SIMPLIFY = TRUE
      )
    ) |>

    filter(status)

  if(isTRUE(mostrar_no_encontrados)){
    noEncontrados <- xu |> anti_join(y = allTests, by = join_by(x_old == x)) |>

      pull(x_orig)

    cli_alert_danger(text = "No se encontraron las siguientes entradas: {noEncontrados}.")
  }

  comparador <- left_join(
    x = xu,
    y = allTests,
    by = join_by(x_old == x)
  ) |>

    select(x_orig, pattern) |>

    filter(!is.na(pattern)) |>

    left_join(
      y = patron_puertos,
      by = join_by(pattern == REGEX)
    )

  if(is.null(out_vars) | length(out_vars) == 0){
    out <- rep(x = NA, times = length(x))

    comparador$LUGAR[match(x = x, table = comparador$x_orig)]
  }else{
    tibble(x = x) |>

      left_join(
        y = comparador,
        by = join_by(x == x_orig)
      ) |>

      select(-pattern, all_of(out_vars))
  }
}
