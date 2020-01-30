library(googledrive)
library(rnoaa)
library(raster)
library(fasterize)
library(sf)
library(magrittr)
library(tidyverse)
library(USAboundaries)
library(velox)
library(gdalUtils)

source("R/download_grid.R")

empty_grid <- download_grid() %>%
  raster

if(!dir.exists(file.path("output", "hazards"))) {
  dir.create(file.path("output", "hazards"), recursive = TRUE)
}

hazard_name <- "tornado"
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))
hazard_path_unmasked <- file.path("data", "hazards", hazard_name, paste0(hazard_name, "_unmasked.tif"))

if(!dir.exists(file.path("data", "hazards", hazard_name))) {
  dir.create(file.path("data", "hazards", hazard_name), recursive = TRUE)
}

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  if(!file.exists(hazard_path_unmasked) | overwrite) {
    # Get the tornado data
    tornado_paths <- tornadoes() %>%
      as('sf') %>%
      st_transform(crs = projection(empty_grid))
    
    # subset to a particular time interval
    # 1982 is when F-scale rating were consistently recorded
    max_year <- 2018
    min_year <- 1982
    n_year <- length(min_year:max_year)
    tornado_paths <- tornado_paths %>%
      filter(yr >= min_year, 
             yr <= max_year)
    
    # buffer at 300 m
    # https://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-13-0235.1
    # Fig. 6 showing mean tornado path widths approx 300m
    buffered_paths <- 
      st_buffer(tornado_paths, dist = 300)
    
    # Compute an annual empirical frequency for each grid cell
    # (# years with events / total # years)
    # and save to a tif file
    
    conus <- 
      USAboundaries::us_boundaries(type = "state", resolution = "high") %>%
      filter(!state_name %in% c("Alaska", "Hawaii") & jurisdiction_type == "state") %>% 
      st_transform(projection(empty_grid))
    
    tornado_counts <- buffered_paths %>%
      fasterize(raster = empty_grid, fun = 'sum', background = 0)
    
    tornado_rate <- tornado_counts / n_year
    # assume an exponential distribution of waiting times, and then we can use
    # the cumulative distribution function for the exponential to ask what
    # is the probability that an event occurred before a specified waiting time
    # (one year, in our case) given a rate of the hazard (which we figured 
    # empirically)
    tornado_prob <- 1 - exp(-1 * tornado_rate)
    
    raster::writeRaster(x = tornado_prob, 
                        filename = file.path("output", "hazards", "tornado-prob_raw.tif"),
                        overwrite = TRUE)
    
    # Aggregate to 1000m so we can use a smoother with a larger radius (thus fewer pixels in
    # each focal operation)
    
    gdalwarp(srcfile = file.path("output", "hazards", "tornado-prob_raw.tif"), 
             dstfile = file.path("output", "hazards", "tornado-prob_1000.tif"), 
             t_srs = crs(empty_grid), 
             tr = c(1000, 1000), 
             overwrite = TRUE,
             r = "bilinear")
    
    tornado_prob_1000 <- raster::raster(file.path("output", "hazards", "tornado-prob_1000.tif"))
    
    # Smoothing
    # Use 150 km radius for smoothing: https://doi.org/10.1175/2013EI000559.1
    # Kernel shape less important than the radius used: https://doi.org/10.1175/BAMS-D-11-00219.1
    # We set sigma to the desired radius divided by 3; the default radius is sigma times 3
    # if the radius represents 3 sigmas from the mean, then 99.7% of the mass of the Gaussian kernel
    # will be contained in the bounds of the radius; 
    # still would be considered a 'bounded Gaussian' distribution though.
    gf <- focalWeight(tornado_prob_1000, 150000 / 3, "Gauss")
    vx <- velox::velox(x = tornado_prob_1000)
    
    # meanFocal operation takes about 40 minutes on the Alienware. 
    # Velox should be quite a bit faster than the raster::focal()
    # implementation. See http://philipphunziker.com/velox/benchmark.html
    vx$meanFocal(weights = gf)
    rg <- vx$as.RasterLayer()
    
    # Mask out the ocean prior to scaling
    ocean_mask_1000 <- fasterize::fasterize(sf = conus, raster = tornado_prob_1000)
    ocean_mask_250 <- fasterize::fasterize(sf = conus, raster = tornado_prob)
    
    rg_ocean_masked <- raster::mask(x = rg, mask = ocean_mask_1000)
    
    raster::writeRaster(x = rg_ocean_masked, filename = hazard_path_unmasked, overwrite = TRUE)

    hazard_path_tmp <- file.path("data", "hazards", hazard_name, paste0(hazard_name, "_temp.tif"))
    
    gdalwarp(srcfile = hazard_path_unmasked, 
             dstfile = hazard_path_tmp, 
             t_srs = crs(empty_grid), 
             tr = c(250, 250), 
             overwrite = TRUE,
             r = "bilinear")
    
    hazard <- 
      gdalUtils::align_rasters(unaligned = hazard_path_tmp, 
                               reference = empty_grid@file@name, 
                               output_Raster = TRUE)
    
    tornado_prob_ocean_masked <- raster::mask(x = tornado_prob, mask = ocean_mask_250)
    
    # rescale the values of the cells such that the total probability across CONUS
    hazard_01 <- hazard / cellStats(hazard, stat = "sum")
    hazard_scaled <- hazard_01 * cellStats(tornado_prob_ocean_masked, stat = "sum")
    
    cellStats(rg_ocean_masked, stat = "sum")
    cellStats(hazard, stat = "sum")
    cellStats(tornado_prob_ocean_masked, stat = "sum")
    cellStats(hazard_scaled, stat = "sum")
    
    raster::writeRaster(x = hazard_scaled, filename = hazard_path_unmasked, overwrite = TRUE)
    
  }
  
  # Mask out the pixels outside of CONUS using the water mask derived from the 
  # USAboundaries package high resolution CONUS shapefile (rasterized to the Zillow
  # grid) and the flood hazard layer, with all values of 999 masked out (representing
  # persistent water bodies)
  if(!file.exists(file.path("output", "water-mask_zillow-grid.tif"))) {
    source("R/configure-flood.R")
  }
  
  mask <- raster::raster("output/water-mask_zillow-grid.tif")
  
  # overwrite R object `hazard` to be the newly aligned and warped layer
  hazard <-
    raster::raster(hazard_path_unmasked) %>% 
    raster::mask(mask = mask)
  
  writeRaster(x = hazard, 
              filename = hazard_path_out, 
              overwrite = TRUE)
}
