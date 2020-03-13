library(raster)
library(mapview)
library(hurricaneexposuredata)
library(rnoaa)
library(tidyverse)
library(sf)
library(HURDAT)
library(patchwork)
library(vroom)

hurricane_harvey <- HURDAT::AL %>%
  filter(Name == "HARVEY", DateTime > "2017-01-01") %>%
  st_as_sf(coords = c("Lon", "Lat"), 
           crs = 4326, agr = "constant")

tornados <- tornadoes() %>%
  as('sf') %>%
  mutate(yr = parse_integer(yr)) %>%
  filter(yr >= 1988)

data("hurr_tracks")

hurr_tracks <- hurr_tracks %>%
  as_tibble

hurr <- st_as_sf(x = hurr_tracks,                         
                 coords = c("longitude", "latitude"),
                 crs = 4326) %>%
  st_transform(st_crs(tornados)) %>%
  group_by(storm_id) %>% 
  summarize(wind = mean(wind), do_union = FALSE) %>%
  st_cast(to = "LINESTRING") %>%
  separate(storm_id, into = c("name", "year"), sep = "-", remove = FALSE)

C <- st_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_20m.json")

hurricane_harvey <- st_transform(hurricane_harvey, st_crs(tornados))

tornado_subset <- tornados %>%
  filter(date >= as.Date("2017-08-17"), 
         date <= as.Date("2017-08-31"), 
         slon > -100, slon < -83, 
         slat > 25, slat < 40) %>%
  mutate(year = yr)

# 52 tornadoes matches the number provided in this report: 
# https://www.nhc.noaa.gov/data/tcr/AL092017_Harvey.pdf
# Harvey was a prolific tornado producer.  There were 52 tornadoes preliminarily 
# reported during Harvey (Fig.    17), about half of which occurred near and 
# south of the Houston metro area.Over  150  tornado  warnings  were  issued  
# during  the  event.    Tornadoes  were  also  noted  in  Louisiana, 
# Mississippi, Alabama and Tennessee as the cyclone moved near or over those 
# states. Fortunately,  almost  all  of  the  tornadoes  were  relatively  weak,
# of  EF-0  and  EF-1  intensity,  with  generally minor damage, few injuries 
# and no deaths attributed to them.
nrow(tornado_subset)
us <- st_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json")
counties <- st_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_5m.json")

# Visualize
tornado_subset %>%
  ggplot() + 
  geom_sf(data = us, fill = "black") +
  geom_sf(aes(color = date, size = loss)) + 
  scale_color_viridis_d()  +
  geom_sf(data = hurricane_harvey %>%
            mutate(date = lubridate::date(DateTime)), 
          aes(color = factor(date))) +
  coord_sf(xlim = c(-100, -83), ylim = c(25, 40)) + 
  xlab("") +
  ylab("") 


# Add the binned Zillow data (I think...)
# https://drive.google.com/open?id=1MaIr34PpxCjJ0ZTzNPU7MUWhq9B59y-e
r <- raster("data/zillow/V3_all_counts/temp_slice_pts_temp_inf_area_counts_2015.tif")
harv_proj <- st_transform(hurricane_harvey, crs = projection(r))
torn_proj <- st_transform(tornado_subset, crs = projection(r))
us_proj <- st_transform(us, crs = projection(r))
flood_proj <- st_read("data/houston-floods.shp") %>%
  st_transform(crs = projection(r))

focal_extent <- raster::extent(-100, -83, 25, 40)
focal_r <- raster(focal_extent)
projection(focal_r) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
focal_r <- projectRaster(focal_r, crs = projection(r))

harvey_r_raw <- crop(r, focal_r)
reclass_m <- c(0, 1, NA,
               1, 8, 1,
               8, 70.01, 2,
               70.01, Inf, 3) %>%
  matrix(ncol = 3,
         byrow = TRUE)
reclass_m

harvey_r <- reclassify(harvey_r_raw, reclass_m, right = FALSE)
values(harvey_r)[values(harvey_r_raw) == 70] %>% table

zillow_df <- as.data.frame(harvey_r, xy = TRUE, long = TRUE) %>%
  as_tibble
zillow_df <- zillow_df %>%
  filter(!is.na(value))
zillow_df

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

harvey_df <- sfc_as_cols(harv_proj) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  mutate(date = lubridate::date(DateTime))

torn_df <- torn_proj %>%
  st_cast("POINT") %>%
  sfc_as_cols() %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  mutate(doy = lubridate::yday(date))

harvey_bbox <- st_bbox(harvey_r)


date_label_df <- distinct(harvey_df, date) %>%
  mutate(id = 1:n(), 
         doy = lubridate::yday(date)) %>%
  filter(id %% 2 == 1) # label every other day

# buffer the flood bounding box
flood_bbox <- st_bbox(flood_proj)
flood_bbox["xmin"] = flood_bbox["xmin"] - 100000 
flood_bbox["xmax"] = flood_bbox["xmax"] + 10000 
flood_bbox["ymin"] = flood_bbox["ymin"] - 70000 
flood_bbox["ymax"] = flood_bbox["ymax"] + 50000

p1 <- us_proj %>%
  ggplot(aes(x, y)) + 
  geom_sf(inherit.aes = FALSE, fill = "white", size = .1) + 
  geom_sf(inherit.aes = FALSE, data = st_as_sfc(flood_bbox), 
          fill = NA) +
  annotate(x = flood_bbox[["xmin"]], y = flood_bbox[["ymax"]], geom = "text", 
           label = "B", vjust = 0, hjust = 0) +
  coord_sf(xlim = c(harvey_bbox["xmin"] + 100000, 
                    harvey_bbox["xmax"] - 100000), 
           ylim = c(harvey_bbox["ymin"] + 100000, 
                    harvey_bbox["ymax"] - 100000)) +
  theme_minimal() +
  geom_tile(aes(fill = fct_rev(as.factor(value))), 
            data = sample_frac(zillow_df), alpha = .8) + 
  scale_fill_grey("Building\ndensity", 
                  labels = c("> 70", "8 to 70", "1 to 7")) + 
  geom_path(data = torn_df, 
            aes(group = om, color = doy)) + 
  geom_point(data = torn_df, 
             aes(group = om, color = doy), alpha = .4) +
  geom_path(data = harvey_df %>%
              filter(y > 0, x < 2e6), 
            aes(color = lubridate::yday(date))) + 
  scale_color_viridis_c("Date", 
                        breaks = date_label_df$doy, 
                        labels = date_label_df$date) + 
  xlab("") + 
  ylab("") +
  ggtitle("A")
p1
ggsave("harvey.png", plot = p1, width = 6, height = 4)


p2 <- us_proj %>%
  ggplot(aes(x, y)) + 
  theme_minimal() +
  geom_sf(inherit.aes = FALSE, fill = "white", size = .1) + 
  geom_sf(data = flood_proj, fill = "dodgerblue", color = NA, 
          inherit.aes = FALSE) + 
  coord_sf(xlim = c(flood_bbox["xmin"], 
                    flood_bbox["xmax"]), 
           ylim = c(flood_bbox["ymin"], 
                    flood_bbox["ymax"])) +
  geom_tile(aes(fill = fct_rev(as.factor(value))), 
            data = sample_frac(zillow_df), alpha = .8) + 
  scale_fill_grey("Building\ndensity", guide = FALSE,
                  labels = c("> 70", "8 to 70", "1 to 7")) + 
  geom_path(data = torn_df, 
            aes(group = om), color = "red") + 
  geom_point(data = torn_df, 
             aes(group = om, size=loss), alpha = .3, color = "red") +
  geom_path(data = harvey_df %>%
              filter(y > 0, x < 2e6), size = 2,
            aes(color = lubridate::yday(date)), 
            lineend = "round") + 
  scale_color_viridis_c("Date", 
                        breaks = date_label_df$doy, 
                        labels = date_label_df$date, 
                        guide = FALSE) + 
  scale_size_continuous("Tornado\ninduced\nlosses ($)") + 
  xlab("") + 
  ylab("") +
  ggtitle("B") + 
  annotate(geom = "text", x = 100000, y = 540000, label = "Hurricane Harvey: centroid path", 
           color = "darkblue", alpha = .6) +
  annotate(geom = "text", x = 80000, y = 860000, label = "Flood inundation", 
           color = "dodgerblue") 
p2
ggsave("harvey-houston.png", plot = p2, width = 6, height = 4)

p_joined <- p1 + p2 + plot_layout(nrow = 2)
ggsave("harvey-multipanel.png", plot = p_joined, width = 6, height = 8)




# Add in the FEMA claims data ---------------------------------------------

download.file("https://www.fema.gov/media-library-data/1575491579309-2366ee38d902c1bc983370e132ee36cc/FIMA_NFIP_Redacted_Claims_Data_Set.zip", 
              destfile = "data/fema.zip")
unzip("data/fema.zip", exdir = "data")
claims <- vroom("data/openFEMA_claims20190831.csv") %>%
  filter(dateofloss >= as.Date("2017-08-17"), 
         dateofloss <= as.Date("2017-09-02"), 
         longitude > -100, longitude < -83, 
         latitude > 25, latitude < 40)

claims %>%
  group_by(dateofloss, longitude, latitude) %>%
  summarize(tot = sum(amountpaidonbuildingclaim, na.rm = TRUE)) %>%
  complete(dateofloss, longitude, latitude, fill = list(tot = 0)) %>%
  ggplot(aes(longitude, latitude, size = tot)) + 
  geom_point(alpha = .5) + 
  facet_wrap(~dateofloss) + 
  coord_equal()

claims %>%
  group_by(dateofloss, longitude, latitude) %>%
  summarize(tot = sum(amountpaidonbuildingclaim, na.rm = TRUE)) %>%
  ungroup %>%
  group_by(longitude, latitude) %>%
  filter(tot == max(tot)) %>%
  ggplot(aes(longitude, latitude, size = tot, 
             color = lubridate::yday(dateofloss))) + 
  geom_point(alpha = .5) + 
  coord_equal() + 
  scale_color_viridis_c()


claim_plot <- claims %>%
  select(dateofloss, starts_with("amountpaid")) %>%
  pivot_longer(cols = starts_with("amount")) %>% 
  group_by(dateofloss, name) %>%
  summarize(total = sum(value, na.rm = TRUE)) %>%
  mutate(Name = case_when(
    name == "amountpaidonbuildingclaim" ~ "Building claims", 
    name == "amountpaidoncontentsclaim" ~ "Contents claims", 
    TRUE ~ "Cost of compliance claims"
  )) %>%
  ggplot(aes(dateofloss, total)) + 
  geom_point() + 
  geom_line(alpha = .1) +
  facet_wrap(~Name, ncol = 1, scales = "free_y") + 
  ylab("Amount paid: FEMA NFIP claims (USD)") + 
  xlab("Date of loss") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
claim_plot

layout <- "
AAA#CC
BBB#CC
"
p_ts <- p1 + p2 + claim_plot + 
  plot_layout(design = layout)

ggsave("harvey-multipanel-ts.png", plot = p_ts, width = 12, height = 8)
