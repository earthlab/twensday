library(googledrive)
library(rnoaa)
library(raster)
library(fasterize)
library(sf)
library(magrittr)
library(tidyverse)
library(USAboundaries)
library(velox)

source("R/download_grid.R")

empty_grid <- download_grid() %>%
  raster

if(!dir.exists(file.path("output", "hazards"))) {
  dir.create(file.path("output", "hazards"), recursive = TRUE)
}

hazard_name <- "tornado"
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {

  conus <- 
    USAboundaries::us_boundaries(type = "state", resolution = "high") %>%
    filter(!state_name %in% c("Alaska", "Hawaii") & jurisdiction_type == "state") %>%
    st_transform(projection(empty_grid))
  
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
  buffered_paths <- st_buffer(tornado_paths, dist = 300)
  
  # Compute an annual empirical frequency for each grid cell
  # (# years with events / total # years)
  # and save to a tif file
  tornado_counts <- buffered_paths %>%
    group_by(yr) %>%
    summarize %>% 
    st_cast("MULTIPOLYGON") %>%
    fasterize(raster = empty_grid, fun = 'sum', background = 0)
  
  
  tornado_freq <- tornado_counts / n_year
  plot(tornado_freq)
  
  # Smoothing
  gf <- focalWeight(tornado_freq, 10000, "circle")
  vx <- velox::velox(x = tornado_freq)
  
  # meanFocal operation takes about 38 minutes on the Alienware. 
  # Velox should be quite a bit faster than the raster::focal()
  # implementation. See http://philipphunziker.com/velox/benchmark.html
  vx$meanFocal(weights = gf)
  rg <- vx$as.RasterLayer()
  
  plot(rg, col = viridis::viridis(100))
  
  rg_01 <- rg / cellStats(rg, stat = "sum")
  rg_scaled <- rg_01 * cellStats(tornado_freq, stat = "sum")
  
  cellStats(tornado_freq, stat = "sum")
  cellStats(rg_scaled, stat = "sum")
  
  # Mask out the pixels outside of CONUS using the water mask derived from the 
  # USAboundaries package high resolution CONUS shapefile (rasterized to the Zillow
  # grid) and the flood hazard layer, with all values of 999 masked out (representing
  # persistent water bodies)
  mask <- raster::raster("output/water-mask_zillow-grid.tif")
  rg_scaled <- raster::mask(x = rg_scaled, mask = mask)
  
  writeRaster(x = rg_scaled, 
              filename = hazard_path_out, 
              overwrite = TRUE)
}
