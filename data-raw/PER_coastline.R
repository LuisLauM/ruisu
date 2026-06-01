PER_coastline <- st_read("inst/extdata/raw-data/PER_coastline.gpkg")

usethis::use_data(PER_coastline, overwrite = TRUE)
