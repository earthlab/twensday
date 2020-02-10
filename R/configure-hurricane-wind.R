# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(raster)
library(googledrive)
library(stormwindmodel)
library(hurricaneexposuredata)
library(pbapply)
library(sf)
library(gdalUtils)


# make the functions available to download the Zillow grid
source("R/download_grid.R")

# Ensure the directory structure for data output is present
if(!dir.exists(file.path("output", "hazards"))) {
  dir.create(file.path("output", "hazards"), recursive = TRUE)
}

# Get the empty template grid of the Zillow dataset
empty_grid <- 
  download_grid() %>%
  raster()

# These set up the variables to be used to get the hazard data and name
# new output files appropriately
hazard_name <- "hurricane-wind"

# Names of the files (to read and manipulate, and then what to call it upon
# export)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  # generate hazard raster
  grid <- expand.grid(glat = seq(20, 
                                 50,
                                 by = 1), 
                      glon = seq(-110,
                                 -60,
                                 by = 1)) %>%
    as_tibble %>%
    mutate(gridid = as.character(1:n()))
  
  cl <- parallel::makeCluster(parallel::detectCores())
  winds <- hurr_tracks %>%
    split(.$storm_id) %>%
    pblapply(get_grid_winds, grid_df = grid, cl = cl) %>%
    bind_rows(.id = "id") %>%
    as_tibble
  parallel::stopCluster(cl)
  
  annual_maxima <- winds %>%
    separate(id, into = c("name", "year")) %>%
    group_by(glat, glon, year) %>%
    summarize(max_gust = max(vmax_gust)) %>%
    ungroup 
  mean_annual_maxima <- annual_maxima %>%
    group_by(glat, glon) %>%
    summarize(mean_gust = mean(max_gust))
  
  gust_sf <- mean_annual_maxima  %>% 
    st_as_sf(coords = c("glon", "glat"),
             crs = 4326)
  grid_template <- raster(xmn = min(winds$glon), xmx = max(winds$glon), 
                          ymn = min(winds$glat), ymx = max(winds$glat), 
                          resolution = 1)
  
  hazard <- rasterize(gust_sf, grid_template, field = "mean_gust")
  hazard_path_src <- "data/hurricane-gust.tif"
  hazard_path_tmp <- paste0(tempfile(), ".tif")
  writeRaster(hazard, hazard_path_src)
  
  hazard_orig <- raster::raster(hazard_path_src)
  
  gdalwarp(srcfile = hazard_path_src, 
           dstfile = hazard_path_tmp, 
           t_srs = crs(empty_grid), 
           tr = c(250, 250), 
           overwrite = TRUE,
           s_srs = crs(hazard_orig))
  
  gdalUtils::align_rasters(unaligned = hazard_path_tmp, 
                           reference = empty_grid@file@name, 
                           dstfile = hazard_path_out, 
                           overwrite = TRUE)
  
  unlink(hazard_path_tmp)
  
  # Read the hazard layer using the raster package so we can mask it
  hazard <- 
    raster::raster(hazard_path_out) %>% 
    # Make 0/NA handling consistent by using a 0 within CONUS for "no hazard"
    raster::reclassify(rcl = cbind(NA, 0))
  
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

# Alternative source?
# hazard_file <- "gdcyc/gdcyc.asc"
# hazard_id <- '1whh-JSmF7v6vJm35lgQAAt5bs01Phb_t'
# zip_path <- file.path("data", "hazards", "gdcyc_cyclone.zip")
