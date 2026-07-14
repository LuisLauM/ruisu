patron_puertos <- readxl::read_excel(
  path = "inst/extdata/raw-data/coordenadas_caletas.xlsx",
  sheet = 1
) |>

  dplyr::mutate(
    REGEX = stringr::str_replace_all(
      string = REGEX,
      pattern = stringr::fixed("\\\\"),
      replacement = "\\"
    )
  ) |>

  dplyr::select(-CODIGO) |>

  dplyr::relocate(LUGAR, .before = 1)

usethis::use_data(patron_puertos, overwrite = TRUE)
