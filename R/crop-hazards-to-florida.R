# Crop multihazards to Florida for testing

library(tidyverse)
library(raster)
library(USAboundaries)
library(sf)

# read the local hazard layers into memory, downloading first
# if necessary
# the rasterStack is called "hazards"
source("R/download_configured-hazards.R")

# get the Florida shapefile
fl <- 
  USAboundaries::us_states(states = "Florida", resolution = "high") %>% 
  sf::st_transform(crs = crs(hazards))

# if the florida hazards directory doesn't exist, then create it
if(!dir.exists(file.path("output", "hazards_florida"))) {
  dir.create(file.path("output", "hazards_florida"))
}

# the list of local hazards
hazard_list <- list.files("output/hazards", full.names = TRUE)

lapply(hazard_list, FUN = function(x) {
  
  hazard_name <- 
    str_split(x, pattern = "_", simplify = TRUE)[1]
  hazard_name <- str_split(string = hazard_name, pattern = "/", simplify = TRUE)[3]
  
  hazard <- raster::raster(x)  
  hazard <- raster::crop(x = hazard, y = fl)
  
  raster::writeRaster(x = hazard, filename = file.path("output", "hazards_florida", paste0(hazard_name, "_florida.tif")), overwrite = TRUE)
  
})
