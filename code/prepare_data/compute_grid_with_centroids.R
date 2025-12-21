# ==============================================================================
# GENERATE 5km × 5km GRID + CENTROIDS FOR QUEENSLAND (buffered mainland)
# 
# This script creates a regular grid (5km × 5km resolution) covering the state
# of Queensland (extent: 138°–154°E, -29°–-9°S). It then computes the centroids
# of each grid cell and retains only those that fall within a 3 km buffer around
# the Australian mainland to ensure coastal and near shore areas are included.
# Note that buffer around Australian mainland can be adjusted.
#
# The final output is saved as a `.fst` file and can be used for extracting
# environmental predictors or running spatial prediction models (e.g., mosquito 
# abundance or distribution).
#
# Author: Manuela M.
# Date: 2024-04-24
# ==============================================================================


# Packages
library(terra)
library(sf)
library(tidyverse)
library(fst)
library(rnaturalearth)
library(ggplot2)
library(leaflet)


# Create 5km x 5km grid over QLD -----------------------------------------------

# Geographical extent: xmin, xmax, ymin, ymax
ext_qld <- ext(138, 154, -29, -9)

# Cell resolution: approx. 5 km in degrees (5 km ≈ 0.045 degrees at equator)
grid_res_deg <- 5 / 111  # ≈ 0.045

# Create empty raster to define the grid structure
r <- rast(ext_qld, res = grid_res_deg, crs = "EPSG:4326")

# Convert raster to polygons (grid cells)
grid <- as.polygons(r)

# Add ID for each cell
grid$id <- 1:ncell(r)

# Compute centroids
centroids <- centroids(grid)

# Extract coordinates manually
coords <- crds(centroids, df = TRUE)

# Combine with grid ID
centroids_df <- coords %>%
  rename(lon = x, lat = y) %>%
  mutate(grid_id = grid$id)

# Check
head(centroids_df)


# Filter out only centroids within 3km of the mainland -------------------------

# Load Australia mainland
aus_mainland <- ne_countries(scale = "large", country = "Australia", returnclass = "sf")
aus_mainland <- st_geometry(aus_mainland)

# Buffer 3 km outward (3000 m), using projected CRS for accuracy
aus_proj <- st_transform(aus_mainland, 3577)  # GDA94 / Australian Albers
aus_buffered <- st_buffer(aus_proj, dist = 3000)  

# Reproject centroids to match buffer
centroids_sf <- st_as_sf(centroids_df, coords = c("lon", "lat"), crs = 4326)
centroids_proj <- st_transform(centroids_sf, 3577)

# Keep centroids within the buffer
centroids_in <- centroids_proj[st_intersects(centroids_proj, aus_buffered, sparse = FALSE), ]

# Convert back to lat/lon for saving
centroids_out <- st_transform(centroids_in, 4326) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(grid_id, lon, lat)


# Visual check -----------------------------------------------------------------

# Interactive map
leaflet(data = centroids_out) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 4,
    color = "yellow",
    fillOpacity = 0.7,
    stroke = FALSE
  ) %>%
  fitBounds(lng1 = 138, lat1 = -29, lng2 = 154, lat2 = -9) %>%
  addScaleBar(position = "bottomleft")

# Static map
ggplot(centroids_out) +
  geom_point(aes(x = lon, y = lat), color = "red", size = 0.2, alpha = 0.2) +
  coord_fixed(xlim = c(138, 154), ylim = c(-29, -9)) +
  labs(title = "Buffered Mainland Centroids (5km × 5km)", x = "Longitude", y = "Latitude") +
  theme_minimal() + theme(plot.title = element_text(size = 11))


# Save -------------------------------------------------------------------------

# Save in current folder
write_fst(
  centroids_out, 
  "C:/Users/men118/OneDrive - CSIRO/Documents/GD/Analyses/QLD/r-data-centroids-grid_5x5-QLD/centroids_buffered_land.fst", 
  compress = 50
)

# Save in local computer as well
write_fst(
  centroids_out, 
  "C:/Users/men118/Documents/gd-project/outputs/centroids/grid_5x5-QLD/centroids_buffered_land.fst", 
  compress = 50
  )
