# Download the configured and standardized hazard layers from
# our online source. For now, that is googledrive, but we could
# change this in the future to be Open Science Framework, figshare,
# or some other such repository

# load dependencies
library(tidyverse)
library(gdalUtils)
library(googledrive)
library(raster)

# What files are currently saved locally?
path_out <- file.path("output", "hazards_florida")
local_hazards <- list.files(path_out)

# if no configured hazards currently downloaded, create
# the directory where they can go
if(length(local_hazards) == 0) {
  dir.create(path_out, recursive = TRUE)
}

# get the googledrive info for all the configured hazards
drive_hazards <- googledrive::drive_ls(path = googledrive::as_id("1b-glwQtsGLTRxfOodMHAZKBsBznKcfhe"))

# we only need to download files that aren't currently on disk
to_download_idx <- !(drive_hazards$name %in% local_hazards)

# filter to just the hazards on the googledrive but that aren't
# on disk, then iterate through them to download to the 
# output/hazards directory
drive_hazards %>% 
  dplyr::filter(to_download_idx) %>% 
  purrr::pmap(.f = function(name, id, ...) {
    
    hazard_path <- file.path(path_out, name)
    invisible(googledrive::drive_download(file = as_id(id), path = hazard_path))
  })