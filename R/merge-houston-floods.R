library(tidyverse)
library(sf)
library(assertthat)
library(mapview)

houston_dir <- "data/houston-inundation"
houston_flood_files <- list.files(houston_dir, full.names = TRUE, pattern = "\\.zip$")
lapply(houston_flood_files, unzip, exdir = houston_dir)

houston_shpfiles <- list.files(houston_dir, pattern = "\\.shp$", 
                               recursive = TRUE, full.names = TRUE) %>%
  grep("bnd", x = ., value = TRUE)
assert_that(length(houston_flood_files) == length(houston_shpfiles))

d <- houston_shpfiles %>%
  map(st_read) %>%
  data.table::rbindlist(fill = TRUE) %>%
  st_as_sf()
mapview(d)

st_write(d, "data/houston-floods.shp", delete_dsn = TRUE)
