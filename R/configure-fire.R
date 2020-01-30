# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(gdalUtils)
library(viridis)

source("R/download_grid.R")
source("R/download_hazard.R")

empty_grid <- 
  download_grid() %>%
  raster()

if(!dir.exists(file.path("output", "hazards"))) {
  dir.create(file.path("output", "hazards"), recursive = TRUE)
}

hazard_name <- "fire"
hazard_file <- "Data/whp_2018_continuous/whp2018_cnt/dblbnd.adf"
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2015-0047-2/RDS-2015-0047-2.zip"
# Source:
# Dillon, Gregory K. 2018. Wildfire Hazard Potential (WHP) for the conterminous United States (270-m GRID), version 2018 continuous. 2nd Edition. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2015-0047-2

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
           r = "bilinear")
  
  unlink(paste0(hazard_path_tmp, ".aux.xml"))

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
  
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = TRUE)

}

# Alternative source. Just burn probability
# Short, Karen C.; Finney, Mark A.; Scott, Joe H.; Gilbertson-Day, Julie W.; Grenfell, Isaac C. 2016. Spatial dataset of probabilistic wildfire risk components for the conterminous United States. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2016-0034
# url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2016-0034/RDS-2016-0034.zip"
