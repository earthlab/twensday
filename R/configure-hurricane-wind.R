# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(raster)
library(googledrive)
# devtools::install_github(repo = "JoshOBrien/rasterDT")
library(rasterDT)
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
# hazard_file <- "CycloneFrequency_1980_2000_projected/gdcyc_NAD.tif"
# zip_path <- file.path("data", "hazards", "CycloneFrequency_1980_2000_projected.zip")
# 
# # The hurricane wind data is on the Google Drive
# hazard_id <- "1REzIWNeq4zwwZdiTT2YBa7UYXTYA-r2s"

hazard_file <- "gdcyc/gdcyc.asc"
zip_path <- file.path("data", "hazards", "gdcyc_cyclone.zip")
hazard_id <- "1whh-JSmF7v6vJm35lgQAAt5bs01Phb_t"

# Names of the files (to read and manipulate, and then what to call it upon
# export)
hazard_path_src <- file.path("data", "hazards", hazard_name, hazard_file)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  # download the raw data from Google Drive
  hazard_metadata <- googledrive::drive_get(id = hazard_id)
  googledrive::drive_download(hazard_metadata, path = zip_path)
  
  # unzip the data file
  unzip(zip_path, overwrite = TRUE, exdir = file.path("data", "hazards", hazard_name))
  unlink(zip_path)
  
  hazard_path_tmp <- file.path("data", "hazards", hazard_name, paste0(hazard_name, "_temp.tif"))
  
  hazard_orig <- raster::raster(hazard_path_src)
  
  gdalwarp(srcfile = hazard_path_src, 
           dstfile = hazard_path_tmp, 
           t_srs = crs(empty_grid), 
           tr = c(250, 250), 
           overwrite = TRUE,
           s_srs = crs(hazard_orig),
           r = "bilinear")
  
  hazard <- gdalUtils::align_rasters(unaligned = hazard_path_tmp, 
                                     reference = empty_grid@file@name, 
                                     dstfile = hazard_path_out, 
                                     overwrite = TRUE,
                                     output_Raster = TRUE)
  
  unlink(hazard_path_tmp)
  
    # Make 0/NA handling consistent by using a 0 within CONUS for "no hazard"
  hazard <- 
    rasterDT::subsDT(x = hazard, dict = data.table(by = NA, which = 0), subsWithNA = FALSE)

  # Mask out the pixels outside of CONUS using the water mask derived from the 
  # USAboundaries package high resolution CONUS shapefile (rasterized to the Zillow
  # grid) and the flood hazard layer, with all values of 999 masked out (representing
  # persistent water bodies)
  if(!file.exists(file.path("output", "water-mask_zillow-grid.tif"))) {
    source("R/configure-flood.R")
  }
  
  mask <- raster::raster("output/water-mask_zillow-grid.tif")
  hazard <- raster::mask(x = hazard, mask = mask)
  
  # This source represents records for 21 years (Jan 1, 1980 to Dec 31, 2000)
  # https://sedac.ciesin.columbia.edu/data/set/ndh-cyclone-hazard-frequency-distribution
  # One caveat is that there is a mask applied to 1 km grid cells to exclude
  # cells with <5 people per square km or without significant agriculture
  hazard_rate <- hazard / 21
  # assume an exponential distribution of waiting times, and then we can use
  # the cumulative distribution function for the exponential to ask what
  # is the probability that an event occurred before a specified waiting time
  # (one year, in our case) given a rate of the hazard (which we figured 
  # empirically)
  hazard_prob <- 1 - exp(-1 * hazard_rate)
  
  # write to disk
  raster::writeRaster(x = hazard_prob, filename = hazard_path_out, overwrite = TRUE)
  
}

# Alternative source?
# hazard_file <- "gdcyc/gdcyc.asc"
# hazard_id <- '1whh-JSmF7v6vJm35lgQAAt5bs01Phb_t'
# zip_path <- file.path("data", "hazards", "gdcyc_cyclone.zip")
