require(dplyr)
require(tidyr)
require(tibble)

anchoveta_lengths <- mapply(
  mean = c(7, 10, 15, 17),
  sd = c(1.2, 1.5, 2, 2.5),
  FUN = rnorm,
  MoreArgs = list(n = 1e3)
) |>

  apply(MARGIN = 2, FUN = as.integer) |>

  apply(MARGIN = 2, FUN = table, simplify = FALSE) |>

  lapply(FUN = as.data.frame) |>

  setNames(nm = month.abb[1:4]) |>

  bind_rows(.id = "month") |>

  pivot_wider(
    id_cols = Var1,
    names_from = month,
    values_from = Freq
  ) |>

  rename(length = Var1) |>

  mutate(length = as.character(length) |> as.integer()) |>

  as.matrix()

usethis::use_data(anchoveta_lengths, overwrite = TRUE)
