# Purpose: Implement a multidimensional empirical cumulative distribution function
# as a means to map the multihazard values at each pixel in CONUS to a [0, 1] 
# interval representing the probability of a set of multihazard values being 
# less than or equal to the set at each pixel.

library(tidyverse)
library(raster)
library(data.table)
# install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")
library(USAboundaries)
library(sf)
library(viridis)
library(colorspace)
library(cowplot)
library(gridGraphics)
library(coop)

# Get the hazard rasters and stack them.
hazard_list <- list.files("output/hazards/", full.names = TRUE, pattern = "zillow")
hazards <- lapply(hazard_list, raster::raster) %>% raster::stack()
hazard_names <- 
  list.files("output/hazards/", pattern  = "zillow") %>% 
  str_split(pattern = "_", simplify = TRUE) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  dplyr::pull(V1) %>% 
  str_replace(pattern = "-", replacement = ".")

# Build a matrix of the values from the raster layers
multihazard_matrix <- getValues(hazards)

# Convert to a data.table for fast manipulation
multihazard_dt <- as.data.table(multihazard_matrix) %>% setNames(hazard_names)

# Assign a unique cell ID to each row 
multihazard_dt[, cellID := 1:.N]

# create a data.table that has no NAs
complete_cases_idx <- which(complete.cases(multihazard_dt))
no_na_hazmat <- multihazard_dt[complete_cases_idx, ]

no_na_hazmat[, cellID_noNA := 1:.N]

# write raw values to disk
data.table::fwrite(x = no_na_hazmat, file = file.path("output", "multihazard-raw_conus.csv"))


# Calculate the marginal CDFs for each hazard using the fast ranking technique
multihazard_dt[, `:=`(earthquake_prob = frankv(earthquake, na.last = "keep", ties.method = "max") / length(which(!is.na(earthquake))),
                      fire_prob = frankv(fire, na.last = "keep", ties.method = "max") / length(which(!is.na(fire))),
                      flood_prob = frankv(flood, na.last = "keep", ties.method = "max") / length(which(!is.na(flood))),
                      hurricane.stormsurge_prob = frankv(hurricane.stormsurge, na.last = "keep", ties.method = "max") / length(which(!is.na(hurricane.stormsurge))),
                      hurricane.wind_prob = frankv(hurricane.wind, na.last = "keep", ties.method = "max") / length(which(!is.na(hurricane.wind))),
                      tornado_prob = frankv(tornado, na.last = "keep", ties.method = "max") / length(which(!is.na(tornado))))]

multihazard_dt[, `:=`(multivariate_multiply = earthquake_prob * fire_prob * flood_prob * hurricane.stormsurge_prob * hurricane.wind_prob * tornado_prob,
                      multivariate_add = earthquake_prob + fire_prob + flood_prob + hurricane.stormsurge_prob + hurricane.wind_prob + tornado_prob)]

# write to disk
data.table::fwrite(x = multihazard_dt, file = file.path("output", "multihazard-ecdf_conus.csv"))

# Create a new raster that uses the template of one of the standardized rasters
multivariate_multiply_raster <- hazards[[1]]
multivariate_add_raster <- hazards[[1]]

# Set the values of the multihazard raster (both masked [i.e., NAs] and unmasked [the Pr(X <= x) values])
# of the multihazard raster to be the values calculated using the multivariate ecdf
values(multivariate_multiply_raster) <- multihazard_dt$multivariate_multiply
values(multivariate_add_raster) <- multihazard_dt$multivariate_add

plot(multivariate_multiply_raster, col = viridis(100))

# Create individual rasters of the marginal Pr(X <= x) values for each hazard
earthquake <- hazards[[1]]
values(earthquake) <- multihazard_dt$earthquake_prob

fire <- hazards[[1]]
values(fire) <- multihazard_dt$fire_prob

flood <- hazards[[1]]
values(flood) <- multihazard_dt$flood_prob

hurricane.stormsurge <- hazards[[1]]
values(hurricane.stormsurge) <- multihazard_dt$hurricane.stormsurge_prob

hurricane.wind <- hazards[[1]]
values(hurricane.wind) <- multihazard_dt$hurricane.wind_prob

tornado <- hazards[[1]]
values(tornado) <- multihazard_dt$tornado_prob


# Set up the plots for arranging in a multi-panel plot
p_earthquake <- ~plot(earthquake, col = colorspace::sequential_hcl(n = 100, palette = "BrwnYl"), main = "Relative earthquake hazard")
p_fire <- ~plot(fire, col = viridis(100, option = "inferno"), main = "Relative fire hazard")
p_flood <- ~plot(flood, col = colorspace::sequential_hcl(n = 100, palette = "Teal"), main = "Relative flood hazard")
p_tornado <- ~plot(tornado, col = viridis(100, option = "E"), main = "Relative tornado hazard")
p_hurricane.stormsurge <- ~plot(hurricane.stormsurge, col = colorspace::sequential_hcl(n = 100, palette = "Dark Mint"), main = "Relative hurricane stormsurge hazard")
p_hurricane.wind <- ~plot(hurricane.wind, col = colorspace::sequential_hcl(n = 100, palette = "Purple-Blue"), main = "Relative hurricane wind hazard")

p_multihazard_multiply <- ~plot(multivariate_multiply_raster, col = viridis(100), main = "Relative multihazard (multiplicative)")
p_multihazard_add <- ~plot(multivariate_add_raster, col = viridis(100), main = "Relative multihazard (additive)")

# put the individual hazard plots together
hazard_plots <- plot_grid(p_earthquake, p_fire, p_flood, p_multihazard_multiply,
                          p_tornado, p_hurricane.stormsurge, p_hurricane.wind, p_multihazard_add, nrow = 2, ncol = 4)

# Save the figure to disk
png("figures/multihazard-and-single-hazard-cdf_conus.png", width = 30, height = 10, units = "in", res = 600)
hazard_plots
dev.off()

png("figures/multihazard-multiplicative_conus.png", width = 20, height = 10, units = "in", res = 600)
plot(multivariate_multiply_raster, col = viridis(100), main = "Relative multiplicative multihazard")
dev.off()

png("figures/multihazard-additive_conus.png", width = 20, height = 10, units = "in", res = 600)
plot(multivariate_add_raster, col = viridis(100), main = "Relative additive multihazard")
dev.off()


# show raw hazard values instead of cdf's for unihazards ------------------

# Create individual rasters of the marginal Pr(X <= x) values for each hazard
earthquake <- hazards[[1]]
values(earthquake) <- multihazard_dt$earthquake

fire <- hazards[[1]]
values(fire) <- multihazard_dt$fire

flood <- hazards[[1]]
values(flood) <- multihazard_dt$flood

hurricane.stormsurge <- hazards[[1]]
values(hurricane.stormsurge) <- multihazard_dt$hurricane.stormsurge

hurricane.wind <- hazards[[1]]
values(hurricane.wind) <- multihazard_dt$hurricane.wind

tornado <- hazards[[1]]
values(tornado) <- multihazard_dt$tornado


# Set up the plots for arranging in a multi-panel plot
p_earthquake <- ~plot(earthquake, col = colorspace::sequential_hcl(n = 100, palette = "BrwnYl"), main = "Earthquake hazard")
p_fire <- ~plot(fire, col = viridis(100, option = "inferno"), main = "Fire hazard")
p_flood <- ~plot(flood, col = colorspace::sequential_hcl(n = 100, palette = "Teal"), main = "Flood hazard")
p_tornado <- ~plot(tornado, col = viridis(100, option = "E"), main = "Tornado hazard")
p_hurricane.stormsurge <- ~plot(hurricane.stormsurge, col = colorspace::sequential_hcl(n = 100, palette = "Dark Mint"), main = "Hurricane stormsurge hazard")
p_hurricane.wind <- ~plot(hurricane.wind, col = colorspace::sequential_hcl(n = 100, palette = "Purple-Blue"), main = "Hurricane wind hazard")

p_multihazard_multiply <- ~plot(multivariate_multiply_raster, col = viridis(100), main = "Relative multihazard (multiplicative)")
p_multihazard_add <- ~plot(multivariate_add_raster, col = viridis(100), main = "Relative multihazard (additive)")

# put the individual hazard plots together
hazard_plots <- plot_grid(p_earthquake, p_fire, p_flood, p_multihazard_multiply,
                          p_tornado, p_hurricane.stormsurge, p_hurricane.wind, p_multihazard_add, nrow = 2, ncol = 4)

# Save the figure to disk
png("figures/multihazard-cdf-and-single-hazard-raw_conus.png", width = 30, height = 10, units = "in", res = 600)
hazard_plots
dev.off()
