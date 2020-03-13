# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(gdalUtils)
library(raster)

source("R/download_grid.R")
source("R/download_hazard.R")

empty_grid <- 
  download_grid() %>%
  raster()

if(!dir.exists(file.path("output", "hazards"))) {
  dir.create(file.path("output", "hazards"), recursive = TRUE)
}

hazard_name <- "flood"
hazard_file <- "Projected_Flood100yr_FathomData/flood_100_NAD.tif"
hazard_id <- '1DkyxvuQi8DQm_gGpn7GF7nnzvvz--Oyp'
zip_path <- file.path("data", "hazards", "Projected_Flood100yr_FathomData.zip")

hazard_path_src <- file.path("data", "hazards", hazard_name, hazard_file)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  hazard_metadata <- drive_get(id = hazard_id)
  dir.create(file.path("data", "hazards"), recursive = TRUE, showWarnings = FALSE)
  drive_download(hazard_metadata, path = zip_path)
  unzip(zip_path, overwrite = FALSE, exdir = file.path("data", "hazards", hazard_name))
  unlink(zip_path)
  
  hazard_path_tmp <- file.path("data", "hazards", hazard_name, paste0(hazard_name, "_temp.tif"))
  
  gdalwarp(srcfile = hazard_path_src, 
           dstfile = hazard_path_tmp, 
           t_srs = crs(empty_grid), 
           tr = c(250, 250), 
           overwrite = overwrite)
  
  gdalUtils::align_rasters(unaligned = hazard_path_tmp, 
                           reference = empty_grid@file@name, 
                           dstfile = hazard_path_out, 
                           overwrite = overwrite)
  unlink(hazard_path_tmp)
  
  # for the flood layer, values of 999 appear to represent persistent water bodies
  # we want to mask these out (and then use this mask for all other layers too)
  hazard <- 
    raster::raster(hazard_path_out) %>% 
    raster::reclassify(rcl = cbind(999, NA))
  
  # we also want to mask outside of the boundaries of CONUS
  conus <- 
    USAboundaries::us_boundaries(type = "state", resolution = "high") %>%
    filter(!state_name %in% c("Alaska", "Hawaii") & jurisdiction_type == "state") %>%
    sf::st_transform(raster::projection(empty_grid)) 
  
  mask <-
    fasterize::fasterize(sf = conus, raster = empty_grid) %>% # create a mask outside of CONUS
    raster::mask(mask = hazard) # also mask based on persistent water bodies in the flood hazard layer
  
  # write the water mask to disk so we can use it for other layers
  raster::writeRaster(x = mask, filename = file.path("output", "water-mask_zillow-grid.tif"))
  
  # Mask out the pixels outside of CONUS using the fire hazard layer
  # (which already properly counts "no hazard" as 0 and NA as "outside of CONUS")
  hazard <- raster::mask(x = hazard, mask = mask)
  
  # write to disk
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = overwrite)
}