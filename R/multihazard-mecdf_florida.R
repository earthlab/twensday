# Purpose: Implement a multidimensional empirical cumulative distribution function
# as a means to map the multihazard values at each pixel in CONUS to a [0, 1] 
# interval representing the probability of a set of multihazard values being 
# less than or equal to the set at each pixel.

library(tidyverse)
library(raster)
library(tdigest)
library(data.table)
library(Emcdf)
# install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")
library(USAboundaries)
library(sf)
library(mltools)
library(viridis)
library(colorspace)
library(cowplot)
library(gridGraphics)

# test case for Florida
# Get the hazard rasters and stack them.
hazard_list_fl <- list.files("output/hazards_florida/", full.names = TRUE)
hazards_fl <- lapply(hazard_list_fl, raster::raster) %>% raster::stack()

# Build a matrix of the values from the raster layers
multihazard_matrix_fl <- getValues(hazards_fl)

# create a data.table that has no NAs
complete_cases_idx_fl <- which(complete.cases(multihazard_matrix_fl))
no_na_hazmat_fl <- multihazard_matrix_fl[complete_cases_idx_fl, ]

# Calculate the multivariate empirical cumulative distribution function for all 6 hazards
# first initialize the object (using approach from the {Emcdf} package)
obj_fl <- initF(no_na_hazmat_fl, 10)

# Calculate the vector of Pr(X <= x) where x is the set of hazard values for each row of the
# values matrix
(start <- Sys.time())
hazard_mecdf_fl <- emcdf(obj_fl, no_na_hazmat_fl)
(Sys.time() - start)

# Convert to a data.table for fast manipulation
multihazard_dt <- as.data.table(multihazard_matrix_fl)
# initialize the Pr(X <= x) column to be NA
multihazard_dt[, multihazard_cdf := NA_real_]
# fill in the non-NA (i.e., non raster masked) values of the Pr(X <= x) data.table with the values
# calculated above
multihazard_dt$multihazard_cdf[complete_cases_idx_fl] <- hazard_mecdf_fl

# Create a new raster that uses the template of one of the standardized rasters
multihazard_raster <- hazards_fl[[1]]
# Set the values of the multihazard raster (both masked [i.e., NAs] and unmasked [the Pr(X <= x) values])
# of the multihazard raster to be the values calculated using the multivariate ecdf
values(multihazard_raster) <- multihazard_dt$multihazard_cdf

# Calculate the marginal CDFs for each hazard using the fast ranking technique
multihazard_dt[, `:=`(earthquake_prob = frankv(earthquake_florida, na.last = "keep", ties.method = "max") / length(which(!is.na(earthquake_florida))),
                      fire_prob = frankv(fire_florida, na.last = "keep", ties.method = "max") / length(which(!is.na(fire_florida))),
                      flood_prob = frankv(flood_florida, na.last = "keep", ties.method = "max") / length(which(!is.na(flood_florida))),
                      hurricane.stormsurge_prob = frankv(hurricane.stormsurge_florida, na.last = "keep", ties.method = "max") / length(which(!is.na(hurricane.stormsurge_florida))),
                      hurricane.wind_prob = frankv(hurricane.wind_florida, na.last = "keep", ties.method = "max") / length(which(!is.na(hurricane.wind_florida))),
                      tornado_prob = frankv(tornado_florida, na.last = "keep", ties.method = "max") / length(which(!is.na(tornado_florida))))]

# Create individual rasters of the marginal Pr(X <= x) values for each hazard
earthquake <- hazards_fl[[1]]
values(earthquake) <- multihazard_dt$earthquake_prob

fire <- hazards_fl[[1]]
values(fire) <- multihazard_dt$fire_prob

flood <- hazards_fl[[1]]
values(flood) <- multihazard_dt$flood_prob

hurricane.stormsurge <- hazards_fl[[1]]
values(hurricane.stormsurge) <- multihazard_dt$hurricane.stormsurge_prob

hurricane.wind <- hazards_fl[[1]]
values(hurricane.wind) <- multihazard_dt$hurricane.wind_prob

tornado <- hazards_fl[[1]]
values(tornado) <- multihazard_dt$tornado_prob

# write to disk
data.table::fwrite(x = multihazard_dt, file = file.path("output", "multihazard-ecdf_florida.csv"))

# Set up the plots for arranging in a multi-panel plot
p_earthquake <- ~plot(earthquake, col = colorspace::sequential_hcl(n = 100, palette = "BrwnYl"), main = "Relative earthquake risk")
p_fire <- ~plot(fire, col = viridis(100, option = "inferno"), main = "Relative fire risk")
p_flood <- ~plot(flood, col = colorspace::sequential_hcl(n = 100, palette = "Teal"), main = "Relative flood risk")
p_tornado <- ~plot(tornado, col = viridis(100, option = "E"), main = "Relative tornado risk")
p_hurricane.stormsurge <- ~plot(hurricane.stormsurge, col = colorspace::sequential_hcl(n = 100, palette = "Dark Mint"), main = "Relative hurricane stormsurge risk")
p_hurricane.wind <- ~plot(hurricane.wind, col = colorspace::sequential_hcl(n = 100, palette = "Purple-Blue"), main = "Relative hurricane wind risk")
p_multihazard <- ~plot(multihazard_raster, col = viridis(100), main = "Relative multihazard risk")

# put the individual hazard plots together
individual_hazards <- plot_grid(p_earthquake, p_fire, p_flood, p_tornado, p_hurricane.stormsurge, p_hurricane.wind, nrow = 2, ncol = 3)

# put the individual hazard plots with the multihazard plot (with the multihazard plot being bigger)
all_hazards <- plot_grid(individual_hazards, p_multihazard, ncol = 2, rel_widths = c(1.5, 1))

# Save the figure to disk
png("figures/multihazard-risk_florida.png", width = 30, height = 10, units = "in", res = 600)
all_hazards
dev.off()

###

# Treat hazards as independent, and multiply the CDFs

multihazard_dt <- data.table::fread(file = file.path("output", "multihazard-ecdf_florida.csv"))

multihazard_dt[, multihazard_independent_prob := 
                 earthquake_prob *
                 fire_prob *
                 flood_prob *
                 hurricane.stormsurge_prob *
                 hurricane.wind_prob *
                 tornado_prob]

multihazard_raster_independent <- hazards_fl[[1]]
# Set the values of the multihazard raster (both masked [i.e., NAs] and unmasked [the Pr(X <= x) values])
# of the multihazard raster to be the values calculated using the multivariate ecdf
values(multihazard_raster_independent) <- multihazard_dt$multihazard_independent_prob

p_multihazard <- ~plot(multihazard_raster, col = viridis(100), main = "Relative multihazard risk")
p_multihazard_independent <- ~plot(multihazard_raster_independent, col = viridis(100), main = "Relative multihazard risk\n(treated as independent hazards)")

# put the individual hazard plots with the multihazard plot (with the multihazard plot being bigger)
all_hazards_independent <- plot_grid(p_multihazard, p_multihazard_independent, ncol = 2, rel_widths = c(1, 1))

# Save the figure to disk
png("figures/multihazard-risk_dependent-vs-independent_florida.png", width = 20, height = 10, units = "in", res = 600)
all_hazards_independent
dev.off()

###

# Treat the hazards as independent, and add the CDFs

multihazard_dt[, multihazard_independent_prob_add := 
                 earthquake_prob +
                 fire_prob +
                 flood_prob +
                 hurricane.stormsurge_prob +
                 hurricane.wind_prob +
                 tornado_prob]

multihazard_raster_independent_add <- hazards_fl[[1]]
# Set the values of the multihazard raster (both masked [i.e., NAs] and unmasked [the Pr(X <= x) values])
# of the multihazard raster to be the values calculated using the multivariate ecdf
values(multihazard_raster_independent_add) <- multihazard_dt$multihazard_independent_prob_add

p_multihazard <- ~plot(multihazard_raster, col = viridis(100), main = "Relative multihazard risk")
p_multihazard_independent_add <- ~plot(multihazard_raster_independent_add, col = viridis(100), main = "Relative multihazard risk\n(treated as independent addititve hazards)")

# put the individual hazard plots with the multihazard plot (with the multihazard plot being bigger)
all_hazards_independent_add <- plot_grid(p_multihazard, p_multihazard_independent_add, ncol = 2, rel_widths = c(1, 1))

# Save the figure to disk
png("figures/multihazard-risk_dependent-vs-independent_addition_florida.png", width = 20, height = 10, units = "in", res = 600)
all_hazards_independent_add
dev.off()

# # For the whole country. Needs some work to make this fast enough to be useful.
# # Current approach will take ~168 days on 10 threads.
# # Potentially promising approaches:
# # Super parallelize (AWS goes up to 96 threads)
# # Get {MEPDF} package to work: https://www.tandfonline.com/doi/full/10.1080/03610918.2018.1554118
# # Get the Lee and Joe (2018) algorithm to work: https://link.springer.com/article/10.1007/s00180-017-0771-x

# hazard_list <- list.files("output/hazards/", full.names = TRUE)
# hazards <- lapply(hazard_list, raster::raster) %>% raster::stack()
# 
# multihazard_matrix <- getValues(hazards)
# 
# complete_cases_idx <- which(complete.cases(multihazard_matrix))
# no_na_hazmat <- multihazard_matrix[complete_cases_idx, ]
# 
# DT <- as.data.table(no_na_hazmat)
# 
# obj <- initF(no_na_hazmat, 10)
# 
# (start <- Sys.time())
# hazard_mecdf <- emcdf(obj, no_na_hazmat[1:1e2, ])
# (Sys.time() - start)
# 
# # Seems to take about 1 second per 6 billion comparisons (each row of 6 is compared to all other rows)
# 6*(120579008 * 120579008) / 6e9 / 60 / 60 / 24

# Florida case takes ~ 2.1 hours
# 2597194^2 / 9e8 / 60 / 60
