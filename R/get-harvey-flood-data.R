library(raster)

if (!file.exists("data/lowres-harvey-flood.tif")) {
  download.file("https://portal.nersc.gov/cascade/Harvey/baseline.tif", 
                "data/baseline.tif")
  
  f <- raster("data/baseline.tif")
  f[f == 999] <- NA
  f <- projectRaster(f, crs = CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  system("gdalwarp -tr 250 250 data/harvey-flood.tif data/lowres-harvey-flood.tif")
  writeRaster(f, "data/harvey-flood.tif", overwrite = TRUE)
}
