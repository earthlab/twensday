# Purpose: Identify some hotspots for hazards and multihazards in CONUS

library(tidyverse)
library(data.table)
library(raster)
library(fasterize)

source("R/download_grid.R")

empty_grid <-
  download_grid() %>% 
  raster::raster()

multihazard_dt <- data.table::fread("output/multihazard-ecdf_conus.csv")