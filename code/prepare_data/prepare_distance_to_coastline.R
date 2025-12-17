# Processing data to run models
# Manuela M.
# 17-01-2025


# This script generates a dataset with distance to coastline at each given location.


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)



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


# Load or create coastline layer -----------------------------------------------
landmass <- st_read(paste0(dir_input,"maps/Australia/GADM/gadm41_AUS_shp/gadm41_AUS_0.shp"))  # better resolution

# Reproject to a suitable CRS for distance calculations (Australian Albers EPSG:3577)
landmass <- st_transform(landmass, crs = 3577)
points_sf <- st_transform(points_sf, crs = st_crs(landmass))

# Extract the boundary (coastline) of the landmass
coastline <- st_boundary(st_union(landmass))

# Compute distances from centroids to the coastline (by default in m)
distances <- st_distance(points_sf, coastline)

# Add to centroids_mainland (in km)
points_sf$dist_coast_km <- as.numeric(distances) / 1000

# Reproject back to geographic CRS (EPSG:4326) to extract lat/lon
points_sf <- st_transform(points_sf, crs = 4326)

# Extract lat and lon as separate columns
points_sf <- points_sf %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

# Select required columns
points <- points_sf %>%
  dplyr::select(UID, dist_coast_km) %>%
  st_drop_geometry()

# Check first 10 elements
head(points)

# Combine centroids_fst with centroids_data_original
data_mozzie_updated <-  merge(data_mozzie, points, by = "UID", all.x = TRUE)
  
# Check the resulting data frame
head(data_mozzie_updated)


# Save the data_filtered data frame to a CSV file ------------------------------
write.csv(
  data_mozzie_updated, 
  "outputs/Farauti_ss_North_QLD_with_dist_to_coast.csv", 
  row.names = FALSE
)

# Visually check ---------------------------------------------------------------

# Load the grid layer
grid_sf <- st_read(paste0(dir_output, "gis/grid-5x5-QLD-shp-4326/grid-5x5-QLD-shp-4326.shp"))

# Ensure CRS Alignment
points_sf <- st_transform(points_sf, crs = st_crs(coastline))  # Ensure points_sf aligns with coastline CRS
landmass <- st_transform(landmass, crs = st_crs(points_sf))  # Align landmass with points_sf

# Calculate bounding box around random points
random_points <- points_sf[sample(nrow(points_sf), 10), ]  # Select 10 random points
bbox_zoom <- st_bbox(random_points) %>%
  st_as_sfc() %>%
  st_transform(crs = st_crs(landmass)) %>%  # Align bbox with landmass CRS
  st_buffer(dist = 10000)  # Buffer in meters for better context

# Crop the landmass to the bounding box
landmass_cropped <- st_crop(landmass, bbox_zoom)

# Transform all layers to EPSG:4326 for visualization
landmass_cropped_geo <- st_transform(landmass_cropped, 4326)
random_points_geo <- st_transform(random_points, 4326)
nearest_lines_geo <- st_transform(nearest_lines, 4326)

# Visualization with ggplot
library(ggplot2)
library(viridis)

ggplot() +
  geom_sf(data = landmass_cropped_geo, fill = "lightgray", color = "black") +  # Cropped landmass
  geom_sf(data = nearest_lines_geo, color = "black", size = 0.5) +  # Nearest line segments
  geom_sf(data = random_points_geo, aes(color = dist_coast_km), size = 3) +  # Random points
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Zoomed-in view of locations and nearest coastline segments",
    subtitle = "Each point is connected to the nearest coastline point",
    color = "Distance to Coast (km)"
  ) +
  theme_minimal()




