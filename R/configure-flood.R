# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(gdalUtils)
library(sf)
# devtools::install_github("JoshOBrien/rasterDT")
# devtools::install_github(repo = "mikoontz/rasterDT", ref = "masking-in-subsDT")
library(rasterDT)

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
  drive_download(hazard_metadata, path = zip_path)
  unzip(zip_path, overwrite = FALSE, exdir = file.path("data", "hazards", hazard_name))
  unlink(zip_path)
  
  hazard_path_tmp <- file.path("data", "hazards", hazard_name, paste0(hazard_name, "_temp.tif"))
  
  # 8 minutes for the bilinear gdalwarp
  gdalUtils::gdalwarp(srcfile = hazard_path_src, 
                      dstfile = hazard_path_tmp, 
                      t_srs = crs(empty_grid), 
                      tr = c(250, 250), 
                      overwrite = TRUE,
                      r = "bilinear",
                      srcnodata = 999)

  # overwrite R object `hazard` to be the newly aligned and warped layer
  hazard <- gdalUtils::align_rasters(unaligned = hazard_path_tmp, 
                           reference = empty_grid@file@name, 
                           output_Raster = TRUE)
  
  raster::NAvalue(hazard) <- -Inf
  
  # delete temporary raster file (the one that is just warped, but not yet aligned)
  unlink(hazard_path_tmp)
  
  # we want to mask outside of the boundaries of CONUS
  conus <- 
    USAboundaries::us_boundaries(type = "state", resolution = "high") %>%
    filter(!state_name %in% c("Alaska", "Hawaii") & jurisdiction_type == "state") %>%
    st_transform(projection(empty_grid)) 
  
  mask <-
    fasterize::fasterize(sf = conus, raster = empty_grid) %>% # create a mask outside of CONUS
    raster::mask(mask = hazard) # also mask based on persistent water bodies in the flood hazard layer
  
  # write the water mask to disk so we can use it for other layers
  raster::writeRaster(x = mask, 
                      filename = file.path("output", "water-mask_zillow-grid.tif"),
                      overwrite = TRUE)
  
  # Mask out the pixels outside of CONUS using the fire hazard layer
  # (which already properly counts "no hazard" as 0 and NA as "outside of CONUS")
  hazard <- raster::mask(x = hazard, mask = mask)
  
  # write to disk
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = TRUE)
}