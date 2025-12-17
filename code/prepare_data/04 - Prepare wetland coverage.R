# Preparing centroids data to run models
# Manuela M.
# 28-01-2025

# This script computes the % coverage of specific 'water body' for several km (1-9)
# buffer around each centroid of 5km x 5km grid of QLD.
# Given a wetland layer (shp file  from 'JoinAustraliaVegetation gbd file, we will
# consider: brackish | coastal | saline | wetland to compute the coverage.


# Libraries --------------------------------------------------------------------
Pkgs2Load <- c(
  "tidyverse", 
  "sf", 
  "dplyr", 
  "fst", 
  "data.table", 
  "tictoc",
  "ggplot2",
  "viridis",
  "tmap",
  "lubridate"
  )
invisible(lapply(Pkgs2Load, library, character.only = TRUE))


# Directories ------------------------------------------------------------------

# Petrichor
# dir_input <- "data/"
# dir_output <- "outputs/"

# Local
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"


process_batch <- function(data_batch, radius, landmass_cropped, veg_layer, veg_type) {
  
  # Create buffers for all centroids in the batch
  buffers <- st_buffer(data_batch, dist = radius)
  
  # Ensure geometry validity
  buffers <- st_make_valid(buffers)
  
  # Perform intersections for all buffers
  buffers_on_mainland <- st_intersection(buffers, landmass_cropped)
  veg_within_buffers <- st_intersection(buffers, veg_layer)
  
  # Calculate total buffer areas
  total_buffer_areas <- as.numeric(st_area(buffers)) / 1e6  # Convert to km^2
  
  # Calculate mainland buffer areas
  if (!is.null(buffers_on_mainland)) {
    mainland_buffer_areas <- buffers_on_mainland %>%
      mutate(mainland_buffer_area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
      st_drop_geometry() %>%
      as.data.table()
    
    mainland_buffer_areas <- mainland_buffer_areas[, .(mainland_buffer_area_km2 = sum(mainland_buffer_area_km2, na.rm = TRUE)), by = UID]
  } else {
    mainland_buffer_areas <- data.table(UID = data_batch$UID, mainland_buffer_area_km2 = 0)
  }
  
  # Calculate vegetation areas
  if (!is.null(veg_within_buffers)) {
    veg_areas <- veg_within_buffers %>%
      mutate(veg_area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
      st_drop_geometry() %>%
      as.data.table()
    
    veg_areas <- veg_areas[, .(veg_area_km2 = sum(veg_area_km2, na.rm = TRUE)), by = UID]
  } else {
    veg_areas <- data.table(UID = data_batch$UID, veg_area_km2 = 0)
  }
  
  # Align areas with grid_id
  mainland_buffer_areas <- merge(
    data.table(UID = data_batch$UID),
    mainland_buffer_areas, 
    by = "UID",
    all.x = TRUE
  )
  mainland_buffer_areas[is.na(mainland_buffer_area_km2), mainland_buffer_area_km2 := 0]
  
  veg_areas <- merge(
    data.table(UID = data_batch$UID),
    veg_areas, 
    by = "UID",
    all.x = TRUE
  )
  veg_areas[is.na(veg_area_km2), veg_area_km2 := 0]
  
  
  # Compile final results
  results <- merge(mainland_buffer_areas, veg_areas, by = "UID")
  results[, buffer_radius := radius]
  results[, total_buffer_area_km2 := total_buffer_areas]
  results[, veg_type := veg_type]
  
  # Re-order columns
  results <- results[, .(UID, buffer_radius, total_buffer_area_km2, mainland_buffer_area_km2, veg_area_km2, veg_type)]
  
  # Add veg coverage pct
  results[, coverage_pct := (veg_area_km2 / mainland_buffer_area_km2) * 100]
  results[, coverage_pct2 := (veg_area_km2 / total_buffer_area_km2) * 100] #  using total buffer area
  
  return(results)
}


# Mozzie data ------------------------------------------------------------------
data_mozzie <- read.csv(paste0(dir_input,"mozzie/Farauti_ss_North_QLD.csv"))
glimpse(data_mozzie)
data_mozzie <- data_mozzie %>%
  mutate(date = dmy(date), year = year(date), month = month(date), day = day(date))
head(data_mozzie)

# Aggregate no.trap by all columns except trap (sum numbers from trap A and B)
data_mozzie <- data_mozzie %>%
  group_by(species, date, week, season, habitat, site, lat, lon, site2, year, month, day) %>%
  summarise(no.trap = sum(no.trap, na.rm = TRUE), .groups = "drop")
head(data_mozzie)

# Add a unique identifier column
data_mozzie <- data_mozzie %>% mutate(UID = 1:nrow(data_mozzie))

# Check
head(data_mozzie, 10)

data_mozzie_sf <- st_as_sf(data_mozzie , coords = c("lon", "lat"), crs = 4326)
world_mercator_crs <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
data_mozzie_sf <- st_transform(data_mozzie_sf, crs = world_mercator_crs)


# Landmass ---------------------------------------------------------------------
landmass <- st_read(paste0(dir_input,"maps/Australia/GADM/gadm41_AUS_shp/gadm41_AUS_0.shp"))  # better resolution
landmass <- st_transform(landmass, crs = st_crs(data_mozzie_sf))
landmass_cropped <- st_crop(landmass, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
landmass_cropped <- st_make_valid(landmass_cropped)
landmass_cropped <- landmass_cropped[!st_is_empty(landmass_cropped), ] # Filter Non-Empty Geometries


# Water layers -----------------------------------------------------------------
file_freshwater <- paste0(dir_output, "gis/freshwater-dams-lakes-lagoons-or-aquatic-plants-AUS-shp-4283/freshwater-dams-lakes-lagoons-or-aquatic-plants-AUS-shp-4283.shp")
freshwater <- st_read(file_freshwater)
freshwater <- st_zm(freshwater, drop = TRUE)
freshwater <- st_transform(freshwater, crs = world_mercator_crs)
freshwater_cropped <- st_crop(freshwater, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
freshwater_cropped <- st_make_valid(freshwater_cropped)

file_saline <- paste0(dir_output, "gis/saline-or-brackish-sedgelands-or-grasslands-AUS-shp-4283/saline-or-brackish-sedgelands-or-grasslands-AUS-shp-4283.shp")
saline <- st_read(file_saline)
saline <- st_zm(saline, drop = TRUE)
saline <- st_transform(saline, crs = world_mercator_crs)
saline_cropped <- st_crop(saline, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
saline_cropped <- st_make_valid(saline_cropped)

file_salt <- paste0(dir_output, "gis/salt-lakes-and-lagoons-AUS-shp-4283/salt-lakes-and-lagoons-AUS-shp-4283.shp")
salt <- st_read(file_salt)
salt <- st_zm(salt, drop = TRUE) # Drop the Z-dimension
salt <- st_make_valid(salt)     # Repair invalid geometries
salt <- st_transform(salt, crs = world_mercator_crs)
salt_cropped <- st_crop(salt, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
salt_cropped <- st_make_valid(salt_cropped)

file_sea <- paste0(dir_output, "gis/sea-estuaries-included-seagrass-AUS-shp-4283/sea-estuaries-included-seagrass-AUS-shp-4283.shp")
sea <- st_read(file_sea)
sea <- st_zm(sea, drop = TRUE) # Drop the Z-dimension
sea <- st_make_valid(sea)     # Repair invalid geometries
sea <- st_transform(sea, crs = world_mercator_crs)
sea_cropped <- st_crop(sea, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
sea_cropped <- st_make_valid(sea_cropped)

file_waterbodies1 <- paste0(dir_output, "gis/waterbodies-combined-1-4-AUS-shp-4283/waterbodies-combined-1-4-AUS-shp-4283.shp")
waterbodies1 <- st_read(file_waterbodies1)
waterbodies1 <- st_zm(waterbodies1, drop = TRUE) # Drop the Z-dimension
waterbodies1 <- st_transform(waterbodies1, crs = world_mercator_crs)
waterbodies1_cropped <- st_crop(waterbodies1, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
waterbodies1_cropped <- st_make_valid(waterbodies1_cropped)

file_waterbodies2 <- paste0(dir_output, "gis/waterbodies-combined-2-4-AUS-shp-4283/waterbodies-combined-2-4-AUS-shp-4283.shp")
waterbodies2 <- st_read(file_waterbodies2)
waterbodies2 <- st_zm(waterbodies2, drop = TRUE) # Drop the Z-dimension
waterbodies2 <- st_make_valid(waterbodies2)
waterbodies2 <- st_transform(waterbodies2, crs = world_mercator_crs)
waterbodies2_cropped <- st_crop(waterbodies2, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
waterbodies2_cropped <- st_make_valid(waterbodies2_cropped)

# Original
# file_water <- paste0(dir_output, "gis/saline-or-bracksish-sedgelands-or-grasslands-AUS-shp-4326/saline-or-bracksish-sedgelands-or-grasslands-AUS-shp-4326.shp")
# water <- st_read(file_water)
# water <- st_transform(water, crs = world_mercator_crs)
# water_cropped <- st_crop(water, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
# water_cropped <- st_make_valid(water_cropped)


# Gather all in a list
veg_layers <- list(
  freshwater = freshwater_cropped,
  saline = saline_cropped,
  salt = salt_cropped,
  sea = sea_cropped,
  waterbodies1 = waterbodies1_cropped,
  waterbodies2 = waterbodies2_cropped
  )

# Clean env.
rm(file_freshwater, freshwater, freshwater_cropped,
   file_saline, saline, saline_cropped,
   file_salt, salt, salt_cropped,
   file_sea, sea, sea_cropped,
   file_waterbodies1, waterbodies1, waterbodies1_cropped,
   file_waterbodies2, waterbodies2, waterbodies2_cropped
   )
gc()


# Batch Processing Setup -------------------------------------------------------
batch_size <- 100  # Number of points per batch
batches <- split(data_mozzie_sf, ceiling(seq_along(data_mozzie_sf$UID) / batch_size))


# Processing batches -----------------------------------------------------------
radii <- c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000)

# Start timing the overall process
tic("Processing multiple radii and water layers")

# Initialize a list to store all results
all_results <- list()

for (veg_type in names(veg_layers)) {
  veg_layer <- veg_layers[[veg_type]]
  
  for (radius in radii) {
    cat(sprintf("Processing radius: %d meters for vegetation type: %s\n", radius, veg_type))
    
    # Initialize a list to store batch results
    results_list <- vector("list", length = length(batches))

    for (batch_idx in seq_along(batches)) {
      cat(sprintf("Processing batch %d of %d\n", batch_idx, length(batches)))
      results_list[[batch_idx]] <- process_batch(batches[[batch_idx]], radius, landmass_cropped, veg_layer, veg_type)
    }
    
    # Combine all batch results for this radius and vegetation type
    combined_results <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
    
    # Append combined results to the all_results list
    all_results[[paste(veg_type, radius, sep = "_")]] <- combined_results
  }
}
# Combine all results into a single data.table for final output
final_results <- rbindlist(all_results, use.names = TRUE, fill = TRUE)

# Stop timing the process
toc()

# Check
head(final_results)
unique(final_results$veg_type)
summary(final_results)



# Save -------------------------------------------------------------------------
write_fst(
  final_results, 
  paste0(dir_output, "waterbodies/Farauti_ss_North_QLD_with_water_coverage_longformat.fst"),
  compress = 50
  )

# write.csv(
#   final_results, 
#   paste0(dir_output, "/Farauti_ss_North_QLD_with_veg_coverage_longformat.csv"),
#   row.names = FALSE
# )
  

# Visualization for a random point near the coastline -----------------------
library(tmap)  # For interactive mapping

# Select a random centroid near the coastline
set.seed(1234)  # For reproducibility
random_points <- data_mozzie_sf[sample(nrow(data_mozzie_sf), 30), , drop = FALSE]

# Create a buffer around the random centroid
buffer_m <- st_buffer(random_points, dist = 10000)

# Ensure mangroves and landmass layers are in the same CRS as the buffer
water <- veg_layers$waterbodies1
water <- st_transform(water, crs = st_crs(buffer_m))
landmass <- st_transform(landmass, crs = st_crs(buffer_m))

# Find mangrove vegetation within the buffer
water_within_buffer <- st_intersection(buffer_m, water)
water_within_buffer

# Find buffer area that falls on the mainland
buffer_on_mainland <- st_intersection(buffer_m, landmass)

# Visualize interactively using tmap
tmap_mode("view")

tm_shape(landmass) + tm_borders(col = "black", lwd = 1) +
  tm_shape(buffer_m) + tm_fill(col = "grey", alpha = 0.3) +  # Full buffer (including sea) in grey
  tm_shape(buffer_on_mainland) + tm_fill(col = "blue", alpha = 0.2) +  # Mainland buffer area in blue
  tm_shape(water_within_buffer) + tm_fill(col = "green4", alpha = 0.8) +  # Mangroves within buffer in green
  tm_shape(random_points) + tm_symbols(col = "black", size = 0.0005) +  # Random centroid as a small black dot
  tm_layout(
    title = "Interactive Map: Buffer, Mainland Area, and Waterbodies1",
    frame = FALSE
  )




# Final results in wide-format -------------------------------------------------
veg_types <- unique(final_results$veg_type)

# Allocate
final_results_wide <- data.frame(matrix(NA, nrow = nrow(data_mozzie), ncol = 200))

# UID column
i = 1
final_results_wide[ ,i] <- 1:nrow(data_mozzie)
names(final_results_wide)[i] <- "UID"

# Buffer and veg columns...
for (radius in radii){
  tmp <- final_results %>% filter(buffer_radius == radius)
  
  final_results_wide[ ,i+1] <- tmp$total_buffer_area_km2[1]
  final_results_wide[ ,i+2] <- tmp$mainland_buffer_area_km2[1]
  
  names(final_results_wide)[(i+1):(i+2)] <- c(
    paste0("total_buffer_", radius/1000, "km_area_km2"),
    paste0("mainland_buffer_", radius/1000, "km_area_km2")
  )
  i = i + 2
  for (veg_name in names(veg_layers)){
    tmp <- final_results %>% filter(veg_type == veg_name & buffer_radius == radius)
    
    final_results_wide[ ,i+1] <- tmp$veg_area_km2
    final_results_wide[ ,i+2] <- tmp$coverage_pct
    final_results_wide[ ,i+3] <- tmp$coverage_pct2
    
    names(final_results_wide)[(i+1):(i+3)] <- c(
      paste0(veg_name, "_", radius/1000, "km_area_km2"),
      paste0(veg_name, "_", radius/1000, "km_pct"),
      paste0(veg_name, "_", radius/1000, "km_pct2")
    )
    i = i + 3
  }
}

head(final_results_wide)

final_results_wide <- final_results_wide[ ,1:181]
head(final_results_wide)

# Save
write_fst(
  final_results_wide, 
  paste0(dir_output, "waterbodies/Farauti_ss_North_QLD_with_water_coverage_wideformat.fst"), 
  compress = 50
)

# write.csv(
#   final_results_wide, 
#   paste0(dir_output, "waterbodies/Farauti_ss_North_QLD_with_water_coverage_wideformat.csv"),
#   row.names = FALSE
# )
