# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(gdalUtils)

source("R/download_grid.R")
source("R/download_hazard.R")

empty_grid <- 
  download_grid() %>%
  raster()

if(!dir.exists(file.path("output", "hazards"))) {
  dir.create(file.path("output", "hazards"), recursive = TRUE)
}

hazard_name <- "earthquake"
url <- "https://earthquake.usgs.gov/static/lfs/nshm/conterminous/2014/data/2014_pga2pct50yrs.dat.zip"
hazard_file <- "2014_pga2pct50yrs.dat"

download_hazard(url = url, 
                hazard_name = hazard_name, 
                hazard_file = hazard_file, 
                overwrite = TRUE)

hazard_path_src <- file.path("data", "hazards", hazard_name, hazard_file)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  hazard_path_tmp <- file.path("data", "hazards", hazard_name, paste0(hazard_name, "_temp.tif"))
  
  gdalwarp(srcfile = hazard_path_src, 
           dstfile = hazard_path_tmp, 
           t_srs = crs(empty_grid), 
           tr = c(250, 250), 
           overwrite = TRUE,
           s_srs = "EPSG:4326",
           r = "bilinear")
  
  hazard <- gdalUtils::align_rasters(unaligned = hazard_path_tmp, 
                           reference = empty_grid@file@name, 
                           dstfile = hazard_path_out, 
                           overwrite = TRUE,
                           output_Raster = TRUE)
  
  unlink(hazard_path_tmp)
  
  # Mask out the pixels outside of CONUS using the water mask derived from the 
  # USAboundaries package high resolution CONUS shapefile (rasterized to the Zillow
  # grid) and the flood hazard layer, with all values of 999 masked out (representing
  # persistent water bodies)
  if(!file.exists(file.path("output", "water-mask_zillow-grid.tif"))) {
    source("R/configure-flood.R")
  }
  
  mask <- raster::raster("output/water-mask_zillow-grid.tif")
  hazard <- raster::mask(x = hazard, mask = mask)
  
  # write to disk
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = TRUE)
}
