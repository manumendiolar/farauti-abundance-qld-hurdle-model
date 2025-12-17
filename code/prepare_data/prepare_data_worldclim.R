# Processing data to run models
# Manuela M.
# 26-03-2024


# This script generates a dataset with worldclim data at each given location.


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(terra)
library(sp)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthhires)
library(raster)        


# Directories ------------------------------------------------------------------

# Local
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"

# HPC
# dir_input <- "work/data/"
# dir_output <- "work/outputs/"



# Mozzie data ------------------------------------------------------------------
data_mozzie <- read.csv(paste0(dir_input,"mozzie/Farauti_ss_North_QLD.csv"))

# Check
glimpse(data_mozzie)

# Formatting
data_mozzie <- data_mozzie %>%
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

# Years we have
cat("We have numbers in years: ", sort(unique(data_mozzie$year)))

# Plot
library(leaflet)
data_mozzie %>% 
  # filter(year %in% c(1991, 2000)) %>%  # closer to 1995-1997
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = ~sqrt(no.trap),
    color = ~ifelse(no.trap == 0, "blue", "red"),
    #popup = ~paste(
      #"UID:", UID, "<br>",
      #"Year:", year, "<br>",
      #"# mozzies:", no.trap, "<br>",
    #), # Add UIN and year to the popup
    label = ~paste("Year:", year, "| #:", no.trap), # Optional label
    labelOptions = labelOptions(direction = "auto")
  ) 


# Histogram (to explore zeros)
ggplot(data_mozzie, aes(x = no.trap)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ year) +
  labs(title = "", x = "# mozzies", y = "Frequency") +
  theme_minimal()


# Define extent (to speed up file processing) ----------------------------------
landmass <- st_read(paste0(dir_input,"maps/Australia/GADM/gadm41_AUS_shp/gadm41_AUS_0.shp"))  # better resolution

# Get the extent
australia_extent <- ext(landmass)  

# Add a buffer (e.g., 1 degree) around Australia's extent
buffered_extent <- extend(australia_extent, c(1, 1, 1, 1))  # Extend by 1 degree in all directions





# Elevation data ---------------------------------------------------------------
elevation <- terra::rast("Z:/work/data/elevation/WorldClim/historical/wc2.1_30s_elev/wc2.1_30s_elev.tif")
print(elevation)
plot(elevation, main = "Global Elevation Data")
elevation <- crop(elevation, buffered_extent)
plot(elevation, main = "Cropped Elevation Data")


# Relative humidity data -----------------------------------------------
humidity <- terra::rast(paste0(dir_output, "humidity/RH_mean_1979-2000_original_res.asc")) # doesn't include Tazzie
print(humidity)
plot(humidity, main = "Humidity", cex.main = 1)
humidity <- resample(humidity, elevation, method = "bilinear")
plot(humidity, main = "Humidity resampled", cex.main = 1)
humidity <- crop(humidity, buffered_extent)
plot(humidity, main = "Cropped Humidity resampled")


# Bioclimatic data -----------------------------------------------------
bioclim_files <- list.files(
  "Z:/work/data/bioclimatic/historical/wc2.1_30s_bio/", 
  pattern = "\\.tif$", 
  full.names = TRUE
)

# Read all .tif files at once as a SpatRasterCollection
bioclim_stack <- terra::rast(bioclim_files)

# Crop all layers in one go
bioclim_data <- crop(bioclim_stack, buffered_extent)

# Assign names to layers
names(bioclim_data) <- paste0("bio", 1:19)

# Check
head(bioclim_data)

# Convert to a list if needed
bioclim_data <- as.list(bioclim_data)

# Access individual rasters, e.g., bio1
plot(bioclim_data[[1]], main = paste0(names(bioclim_data[[1]]), " cropped"))

# Combine the bioclim_data list into a single SpatRaster stack
bioclim_stack <- rast(bioclim_data)

# Verify it is now a SpatRaster
inherits(bioclim_stack, "SpatRaster")



# Land cover data --------------------------------------------------------------
trees <- terra::rast("Z:/work/data/landuse/WorldCover_trees_30s.tif")
mangroves <- terra::rast("Z:/work/data/landuse/WorldCover_mangroves_30s.tif")
water <- terra::rast("Z:/work/data/landuse/WorldCover_water_30s.tif")
wetland <- terra::rast("Z:/work/data/landuse/WorldCover_wetland_30s.tif")
grassland <- terra::rast("Z:/work/data/landuse/WorldCover_grassland_30s.tif")
shrubs <- terra::rast("Z:/work/data/landuse/WorldCover_shrubs_30s.tif")
cropland <- terra::rast("Z:/work/data/landuse/WorldCover_cropland_30s.tif")
built <- terra::rast("Z:/work/data/landuse/WorldCover_built_30s.tif")
bare <- terra::rast("Z:/work/data/landuse/WorldCover_bare_30s.tif")

# Check one with a plot
# (fractional vegetation cover at 30-sec - roughly 1 km - resolution)
plot(mangroves, main = 'Mangroves cover\n Fractional vegetation cover at 30-sec - roughly 1 km - resolution')

# Crop 
trees <- crop(trees, buffered_extent)
mangroves <- crop(mangroves, buffered_extent)
water <- crop(water, buffered_extent)
wetland <- crop(wetland, buffered_extent)
grassland <- crop(grassland, buffered_extent)
shrubs <- crop(shrubs, buffered_extent)
cropland <- crop(cropland, buffered_extent)
built <- crop(built, buffered_extent)
bare <- crop(bare, buffered_extent)

# Check one
plot(mangroves, main = 'Mangroves  Cropped')

# If needed...
# # Aggregate each to 2.5-min spatial resolution (roughly 5 km)
# trees <- terra::aggregate(trees, fact=5, fun='mean')
# mangroves <- terra::aggregate(mangroves, fact=5, fun='mean')
# water <- terra::aggregate(water, fact=5, fun='mean')
# wetland <- terra::aggregate(wetland, fact=5, fun='mean')
# grassland <- terra::aggregate(grassland, fact=5, fun='mean')
# shrubs <- terra::aggregate(shrubs, fact=5, fun='mean')
# cropland <- terra::aggregate(cropland, fact=5, fun='mean')
# built <- terra::aggregate(built, fact=5, fun='mean')
# bare <- terra::aggregate(bare, fact=5, fun='mean')
# 
# # check
# plot(trees, main = 'Trees cover')


# Distance to coastline -----------------------------------------------------
# dist_coast <- terra::rast("Z:/work/outputs/coastline/dist_to_sea_australia.tif")
# plot(dist_coast)

# Distance to waterbodies -----------------------------------------------------
# dist_waterbodies <- terra::rast("Z:/work/outputs/waterbodies/distance_to_waterbodies_QLD_30s.asc")
# dist_waterbodies <- crop(dist_waterbodies, buffered_extent)
# plot(dist_waterbodies, main = "Distance to waterbodies (km)")


# Stack the Environmental Layers -----------------------------------------------

# Align extents, CRS, and resolutions


# Align extents and resolutions
ref_raster <- elevation # use elevation as the reference raster
elevation <- resample(elevation, ref_raster)
humidity <- resample(humidity, ref_raster)
for (i in 1:19) bioclim_stack[[i]] <- resample(bioclim_stack[[i]], ref_raster)
trees <- resample(trees, ref_raster)
mangroves <- resample(mangroves, ref_raster)
water <- resample(water, ref_raster)
wetland <- resample(wetland, ref_raster)
grassland <- resample(grassland, ref_raster)
shrubs <- resample(shrubs, ref_raster)
cropland <- resample(cropland, ref_raster)
built <- resample(built, ref_raster)
bare <- resample(bare, ref_raster)


# Align CRS (re project rasters)
ref_crs <- crs(elevation)
humidity <- project(humidity, ref_crs)
bioclim_stack <- project(bioclim_stack, ref_crs)
trees <- project(trees, ref_crs)
mangroves <- project(mangroves, ref_crs)
water <- project(water, ref_crs)
wetland <- project(wetland, ref_crs)
grassland <- project(grassland, ref_crs)
shrubs <- project(shrubs, ref_crs)
cropland <- project(cropland, ref_crs)
built <- project(built, ref_crs)
bare <- project(bare, ref_crs)

# Combine the rasters into a single SpatRaster stack
env_stack <- c(
  elevation, 
  humidity, 
  bioclim_stack,
  trees,
  mangroves,
  water,
  wetland,
  grassland,
  shrubs,
  cropland,
  built, 
  bare
)


# Check the combined stack
print(env_stack)

# Check names to ensure they are meaningful
names(env_stack) <- c("elevation", "humidity", paste0("bio", 1:19),
                      "trees", "mangroves", "water", "wetland", "grassland",
                      "shrubs", "cropland", "built", "bare")
# Inspect
head(env_stack)

# Clean env.
rm(
  elevation, 
  humidity, 
  bioclim_stack,
  trees,
  mangroves,
  water,
  wetland,
  grassland,
  shrubs,
  cropland,
  built, 
  bare
)
rm(bioclim_data, bioclim_files, bioclim_list)
gc()



# Extract raster values at point locations -------------------------------------

# Get points
points <- data_mozzie[ ,c("lon", "lat")]

# Check
head(points)

# Convert points to spatial object
point_spatial <- vect(points, geom = c("lon", "lat"), crs = "EPSG:4326")  # Assuming WGS84 (EPSG:4326)

# # Ensure points_sp is SpatialPoints
# points_sp <- as(point_spatial, "Spatial")


# Extract raster values at the points
extracted_values <- extract(env_stack, point_spatial, method = "bilinear")

# Include attributes when creating the SpatVector
point_spatial <- vect(data_mozzie, geom = c("lon", "lat"), crs = "EPSG:4326")

# Check the SpatVector
print(point_spatial)

# Combine extracted values with original mosquito data
data_mozzie_updated <- cbind(data_mozzie, extracted_values)

# Check the final dataset
head(data_mozzie_updated)

# Inspect
summary(data_mozzie_updated)

# Check missing values or NaNs
data_mozzie_updated[which(is.na(data_mozzie_updated$elevation)), ]


# Save -------------------------------------------------------------------------
write.csv(
  data_mozzie_updated, 
  paste0(dir_output, "climate/Farauti_ss_North_QLD_with_worldclim.csv"), 
  row.names = FALSE
)


# # Handling missing values ------------------------------------------------------
# library(FNN)
# columns_to_fill <- c("elevation", "humidity", paste0("bio", 1:19))
# 
# # Rows with no NA values in the specified columns
# data_complete <- data_mozzie_updated[complete.cases(data_mozzie_updated[, columns_to_fill]), ]
# 
# # Rows with NA values in the specified columns
# data_with_na <- data_mozzie_updated[!complete.cases(data_mozzie_updated[, columns_to_fill]), ]
# 
# # Coordinates of complete and incomplete points
# coords_complete <- data_complete[, c("lon", "lat")]
# coords_na <- data_with_na[, c("lon", "lat")]
# 
# # Find nearest neighbors
# nn <- get.knnx(coords_complete, coords_na, k = 1)
# 
# # Indices of nearest neighbors
# nearest_indices <- nn$nn.index[, 1]
# 
# # Replace NA values with nearest neighbor values
# for (col in columns_to_fill) {
#   data_with_na[[col]] <- ifelse(
#     is.na(data_with_na[[col]]),
#     data_complete[nearest_indices, col],
#     data_with_na[[col]]
#   )
# }
# # Combine complete and updated rows
# data_filled <- rbind(data_complete, data_with_na)
# 
# # Check the updated dataset
# summary(data_filled)
# 
# # Rename
# data_mozzie_updated <- data_filled
# 
# # Clean env.
# rm(data_filled)
# rm(col, columns_to_fill, coords_complete, coords_na)
# rm(data_complete, data_with_na)
# rm(i, nn, nearest_indices, ref_crs, ref_raster)
# gc()
# 
# 
# # Check UINs: 438 and 546
# data_mozzie_updated %>% filter(UIN %in% seq(436, 440, 1))
# 
# 
# # Save -------------------------------------------------------------------------
# write.csv(
#   data_mozzie_updated, 
#   paste0(dir_output, "Farauti_ss_North_QLD_with_worldclim.csv"), 
#   row.names = FALSE
# )





