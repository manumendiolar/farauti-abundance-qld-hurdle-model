# Processing data to run models
# Manuela M.
# 18-01-2025


# This script generates a dataset with distance to waterbodies (brackish) at each given location.


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(tictoc)


# Directories ------------------------------------------------------------------

# Local
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"

# Bowen
# main_dir_input <- "Z:/work/data/"
# main_dir_output <- "Z:/work/outputs/"


# Mozzie data ------------------------------------------------------------------
data_mozzie <- read.csv(paste0(dir_input,"mozzie/Farauti_ss_North_QLD.csv"))

# Check
glimpse(data_mozzie)

# Formatting
data_mozzie <- data_mozzie%>%
  mutate(
    date = dmy(date),
    year = year(date),
    month = month(date),
    day = day(date)
  )

# Check
head(data_mozzie)

# Aggregate no.trap by all columns except trap (sum numbers from trap A and B)
data_mozzie <- data_mozzie %>%
  group_by(species, date, week, season, habitat, site, lat, lon, site2, year, month, day) %>%
  summarise(no.trap = sum(no.trap, na.rm = TRUE), .groups = "drop")

# Check
head(data_mozzie)

# Add a unique identifier column
data_mozzie <- data_mozzie %>% mutate(UID = 1:nrow(data_mozzie))

# Check
head(data_mozzie, 10)

# Create an sf object
points_sf <- st_as_sf(data_mozzie, coords = c("lon", "lat"), crs = 4326)  # EPSG:4326 for geographic coordinates

# Check 
points_sf

# Define the CRS as a PROJ string
world_mercator_crs <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Transform to projected CRS (e.g., Mercator)
points_sf <- st_transform(points_sf, crs = world_mercator_crs) 


# Water bodies layer -------------------------------------------------------
water_brackish <- st_read(paste0(dir_output, "gis/saline-or-bracksish-sedgelands-or-grasslands-AUS-shp-4326/saline-or-bracksish-sedgelands-or-grasslands-AUS-shp-4326.shp"))
water_brackish <- st_transform(water_brackish, crs = world_mercator_crs)


# Distances --------------------------------------------------------------------

# Initialize a vector for distances
distances <- numeric(nrow(points_sf))
tic("Computing distance to nearest water body system...")
for (i in 1:nrow(points_sf)){
  distances[i] <- apply(st_distance(points_sf[i, ], water_brackish), 1, min)
}
toc()

# Assign distances back
points_sf$dist_brackish_water_m <- distances

# Convert distances to kilometers
points_sf$dist_brackish_water_km <- points_sf$dist_brackish_water_m / 1000

# Reproject back to geographic CRS (EPSG:4326) to extract lat/lon
points_sf <- st_transform(points_sf, crs = 4326)

# Extract lat and lon as separate columns
points_sf <- points_sf %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

# Select required columns
points <- points_sf %>% dplyr::select(UID, dist_brackish_water_km) %>% st_drop_geometry()

# Check first 10 elements
head(points)

# Combine centroids_fst with centroids_data_original
data_mozzie_updated <-  merge(data_mozzie, points, by = "UID", all.x = TRUE)

# Check the resulting data frame
head(data_mozzie_updated)


# Visually check ---------------------------------------------------------------

# Calculate the nearest water body segments
points_sf <- st_transform(points_sf, crs = world_mercator_crs) 
nearest_segments <- st_nearest_points(points_sf, water_brackish)

# Transform all layers to EPSG:4326 for visualization
points_sf_geo <- st_transform(points_sf, 4326)
water_brackish_geo <- st_transform(water_brackish, 4326)
nearest_segments_geo <- st_transform(nearest_segments, 4326)

# Visualization with ggplot
library(ggplot2)
library(viridis)

ggplot() +
  geom_sf(data = water_brackish_geo, fill = "blue", color = "darkblue", alpha = 0.5) +  # Plot water bodies
  #geom_sf(data = points_sf_geo, aes(color = dist_water_km), size = 2) +  # Plot points with distance to water
  geom_sf(data = nearest_segments_geo, color = "red", size = 0.5) +  # Plot segments to water bodies
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Distances to Nearest Brakish Water Bodies",
    subtitle = "Each point is connected to the nearest water body (saline or brackish)",
    color = "Distance to Water (km)"
  ) +
  theme_minimal()


# Save the data_filtered data frame to a CSV file ------------------------------
write.csv(
  data_mozzie_updated, 
  paste0(dir_output, "waterbodies/Farauti_ss_North_QLD_with_dist_to_water_brackish.csv"), 
  row.names = FALSE
)


# Visualization of water layer -------------------------------------------------
library(dplyr)
library(stringr)

# Filter the layer based on the keywords
filtered_water_brackish <- water_brackish %>%
  filter(str_detect(SOURCE_DES, regex("brackish|coastal|saline|wetland", ignore_case = TRUE)))

# Check the resulting filtered dataset
head(filtered_water_brackish)

# Plot the filtered data
ggplot() +
  geom_sf(data = water_brackish, fill = "lightblue", color = "lightblue", alpha = 0.5) +  # Plot the polygons
  geom_sf(data = filtered_water_brackish, fill = "blue", color = "darkblue", alpha = 0.5) +  # Plot the polygons
  labs(
    title = "Filtered Brackish, Coastal, Saline, or Wetland Water Bodies",
    subtitle = "Displaying only features with specific keywords in SOURCE_DES",
    caption = "Data source: water_brackish"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


# Visualization ----------------------------------------------------------------

grid <- st_read(paste0(dir_output, "gis/grid-5x5-QLD-shp-4326/grid-5x5-QLD-shp-4326.shp"))
landmass <- st_read(paste0(dir_input,"maps/Australia/GADM/gadm41_AUS_shp/gadm41_AUS_0.shp"))  # better resolution

# Validate and clean the geometries if necessary
landmass <- st_make_valid(landmass)
water_brackish <- st_make_valid(water_brackish)


# Select a random single point
set.seed(123)  # For reproducibility
random_point <- points_sf[sample(nrow(points_sf), 1), ]

# Find the index of the nearest water body to the random point
nearest_water_index <- st_nearest_feature(random_point, water)

# Get the geometry of the nearest water body
nearest_water <- water[nearest_water_index, ]

# Create the segment connecting the random point to the nearest water body
nearest_segment <- st_nearest_points(random_point, nearest_water)

# Transform all layers to EPSG:4326 for visualization
random_point_geo <- st_transform(random_point, 4326)
water_geo <- st_transform(water, 4326)
nearest_water_geo <- st_transform(nearest_water, 4326)
nearest_segment_geo <- st_transform(nearest_segment, 4326)

# Validate and clean the water_geo layer
water_geo <- st_make_valid(water_geo)

# Define a bounding box around the random point for zooming
bbox_zoom <- st_buffer(random_point_geo, dist = 5000) %>%  # Buffer directly around the random point
  st_transform(crs = 4326)  # Align CRS with water_geo

# Crop the water layer to the buffered area for better focus
water_cropped <- st_crop(water_geo, bbox_zoom)

# Define the bounding box manually with min/max lat and lon
min_lon <- 145.4
max_lon <- 145.8
min_lat <- -16.9
max_lat <- -16.6

# Crop to a bounding box for a smaller region
bbox <- st_bbox(c(xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat), crs = st_crs(4326)) %>% st_as_sfc()
landmass_geo <- st_crop(landmass_geo, bbox)
water_geo <- st_crop(water_geo, bbox)


# Visualization with ggplot
ggplot() +
  geom_sf(data = landmass_geo, fill = "lightgray", color = "black") +  # Plot the landmass
  geom_sf(data = water_geo, fill = "blue", color = "darkblue", alpha = 0.5) +  # Plot water bodies
  geom_sf(data = random_point_geo, fill="green3", size = 3) +  # Plot the random point
  geom_sf(data = nearest_segment_geo, color = "red", size = 0.7) +  # Plot the single segment
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Distance to Nearest Water Body for a Random Point",
    subtitle = "A single point is connected to the nearest water body",
    color = "Distance to Water (km)"
  ) +
  coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat), expand = FALSE) +  # Set lat/lon limits
  theme_minimal()
