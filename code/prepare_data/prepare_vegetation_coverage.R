# Preparing centroids data to run models
# Manuela M.
# 28-01-2025

# This script computes the % coverage of specific vegetation for several km (1-9)
# buffer around each centroid of 5km x 5km grid of QLD.


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
  
  # Add mangrove coverage pct
  results[, coverage_pct := (veg_area_km2 / mainland_buffer_area_km2) * 100]
  
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


# Vegetation layers ------------------------------------------------------------
file_mangroves <- paste0(dir_output, "gis/mangroves-AUS-shp-4283/mangroves-AUS-shp-4283.shp")
mangroves <- st_read(file_mangroves)
mangroves <- st_zm(mangroves, drop = TRUE)
mangroves <- st_transform(mangroves, crs = world_mercator_crs)
mangroves_cropped <- st_crop(mangroves, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
mangroves_cropped <- st_make_valid(mangroves_cropped)

file_rainforest <- paste0(dir_output, "gis/tropical-or-subtropical-rainforest-AUS-shp-4283/tropical-or-subtropical-rainforest-AUS-shp-4283.shp")
rainforest <- st_read(file_rainforest)
rainforest <- st_zm(rainforest, drop = TRUE)
rainforest <- st_transform(rainforest, crs = world_mercator_crs)
rainforest_cropped <- st_crop(rainforest, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
rainforest_cropped <- st_make_valid(rainforest_cropped)

file_cleared <- paste0(dir_output, "gis/cleared-non-native-vegetation-buildings-AUS-shp-4283/cleared-non-native-vegetation-buildings-AUS-shp-4283.shp")
cleared <- st_read(file_cleared)
cleared <- st_zm(cleared, drop = TRUE)
cleared <- st_transform(cleared, crs = world_mercator_crs)
cleared_cropped <- st_crop(cleared, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
cleared_cropped <- st_make_valid(cleared_cropped)

file_acacia <- paste0(dir_output, "gis/acacia-forests-and-woddlands-AUS-shp-4283/acacia-forests-and-woddlands-AUS-shp-4283.shp")
acacia <- st_read(file_acacia)
acacia <- st_zm(acacia, drop = TRUE)
acacia <- st_transform(acacia, crs = world_mercator_crs)
acacia_cropped <- st_crop(acacia, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
acacia_cropped <- st_make_valid(acacia_cropped)

file_euca1 <- paste0(dir_output, "gis/eucalyptus-low-open-woodlands-with-tussock-grass-AUS-shp-4283/eucalyptus-low-open-woodlands-with-tussock-grass-AUS-shp-4283.shp")
euca1 <- st_read(file_euca1)
euca1 <- st_zm(euca1, drop = TRUE)
euca1 <- st_transform(euca1, crs = world_mercator_crs)
euca1_cropped <- st_crop(euca1, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
euca1_cropped <- st_make_valid(euca1_cropped)

file_sand_rock <- paste0(dir_output, "gis/naturally-bare-sand-rock-claypan-mudflat-AUS-shp-4283/naturally-bare-sand-rock-claypan-mudflat-AUS-shp-4283.shp")
sand_rock <- st_read(file_sand_rock)
sand_rock <- st_zm(sand_rock, drop = TRUE)
sand_rock <- st_transform(sand_rock, crs = world_mercator_crs)
sand_rock_cropped <- st_crop(sand_rock, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
sand_rock_cropped <- st_make_valid(sand_rock_cropped)

file_mangroves_and_rainforest <- paste0(dir_output, "gis/mangroves-rainforest-or-sub-tropical-rainforest-AUS-shp-4283/mangroves-rainforest-or-sub-tropical-rainforest-AUS-shp-4283.shp")
mangroves_and_rainforest <- st_read(file_mangroves_and_rainforest)
mangroves_and_rainforest <- st_zm(mangroves_and_rainforest, drop = TRUE)
mangroves_and_rainforest <- st_transform(mangroves_and_rainforest, crs = world_mercator_crs)
mangroves_and_rainforest_cropped <- st_crop(mangroves_and_rainforest, st_bbox(data_mozzie_sf) + c(-10000, -10000, 10000, 10000))
mangroves_and_rainforest_cropped <- st_make_valid(mangroves_and_rainforest_cropped)

# Gather all in a list
veg_layers <- list(
  mangroves = mangroves_cropped,
  rainforest = rainforest_cropped,
  cleared = cleared_cropped,
  euca1 = euca1_cropped,
  acacia = acacia_cropped,
  sand_rock = sand_rock_cropped,
  mangroves_and_rainforest = mangroves_and_rainforest_cropped
  )

# Clean env.
rm(mangroves, mangroves_cropped, file_mangroves,
   rainforest, rainforest_cropped, file_rainforest,
   cleared, cleared_cropped, file_cleared,
   euca1, euca1_cropped, file_euca1,
   acacia, acacia_cropped, file_acacia,
   sand_rock, sand_rock_cropped, file_sand_rock,
   mangroves_and_rainforest, mangroves_and_rainforest_cropped, file_mangroves_and_rainforest
   )
gc()


# Batch Processing Setup -------------------------------------------------------
batch_size <- 100  # Number of centroids per batch
batches <- split(data_mozzie_sf, ceiling(seq_along(data_mozzie_sf$UID) / batch_size))


# Processing batches -----------------------------------------------------------
radii <- c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000)

# Start timing the overall process
tic("Processing multiple radii and vegetation layers")

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


# Save -------------------------------------------------------------------------
write_fst(
  final_results, 
  paste0(dir_output, "vegetation/Farauti_ss_North_QLD_with_veg_coverage_longformat.fst"),
  compress = 50
  )

# write.csv(
#   final_results, 
#   paste0(dir_output, "vegetation/Farauti_ss_North_QLD_with_veg_coverage_longformat.csv"),
#   row.names = FALSE
# )
  

# Visualization for a random centroid near the coastline -----------------------
library(tmap)  # For interactive mapping

# Select a random centroid near the coastline
set.seed(1234)  # For reproducibility
random_points <- data_mozzie_sf[sample(nrow(data_mozzie_sf), 3), , drop = FALSE]

# Create a buffer around the random centroid
buffer_m <- st_buffer(random_points, dist = 5000)

# Ensure mangroves and landmass layers are in the same CRS as the buffer
mangroves <- veg_layers$mangroves
mangroves <- st_transform(mangroves, crs = st_crs(buffer_m))
landmass <- st_transform(landmass, crs = st_crs(buffer_m))

# Find mangrove vegetation within the buffer
mangroves_within_buffer <- st_intersection(buffer_m, mangroves)

# Find buffer area that falls on the mainland
buffer_on_mainland <- st_intersection(buffer_m, landmass)

# Visualize interactively using tmap
tmap_mode("view")

tm_shape(landmass) + tm_borders(col = "black", lwd = 1) +
  tm_shape(buffer_m) + tm_fill(col = "grey", alpha = 0.3) +  # Full buffer (including sea) in grey
  tm_shape(buffer_on_mainland) + tm_fill(col = "blue", alpha = 0.2) +  # Mainland buffer area in blue
  tm_shape(mangroves_within_buffer) + tm_fill(col = "green4", alpha = 0.8) +  # Mangroves within buffer in green
  tm_shape(random_points) + tm_symbols(col = "black", size = 0.0005) +  # Random centroid as a small black dot
  tm_layout(
    title = "Interactive Map: Buffer, Mainland Area, and Mangroves",
    frame = FALSE
  )

data_mozzie %>% filter(UID %in% c(284,336,406))
final_results %>% filter(veg_type == "mangroves", buffer_radius == 5000, UID %in% c(284,336,406))




# Final results in wide-format -------------------------------------------------

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
  final_results_wide[ ,i+1] <- tmp$mainland_buffer_area_km2[1]
  
  names(final_results_wide)[(i+1):(i+2)] <- c(
    paste0("total_buffer_", radius/1000, "km_area_km2"),
    paste0("mainland_buffer_", radius/1000, "km_area_km2")
  )
  i = i + 2
  for (veg_name in names(veg_layers)){
    tmp <- final_results %>% filter(veg_type == veg_name & buffer_radius == radius)
    
    final_results_wide[ ,i+1] <- tmp$veg_area_km2
    final_results_wide[ ,i+2] <- tmp$coverage_pct
    
    names(final_results_wide)[(i+1):(i+2)] <- c(
      paste0(veg_name, "_", radius/1000, "km_area_km2"),
      paste0(veg_name, "_", radius/1000, "km_pct")
    )
    i = i + 2
  }
}

head(final_results_wide)
final_results_wide <- final_results_wide[ ,1:145]
head(final_results_wide)

# Check
data.frame(
  UID = c(284,336,406), 
  mangroves_5km_pct = final_results_wide[which(final_results_wide$UID %in% c(284,336,406)), ]$mangroves_5km_pct
  )

data.frame(
  UID = c(284), 
  pct = final_results_wide[which(final_results_wide$UID %in% c(284)), ]$mangroves_4km_pct
)

# Save
write_fst(
  final_results_wide, 
  paste0(dir_output, "vegetation/Farauti_ss_North_QLD_with_veg_coverage_wideformat.fst"),
  compress = 50
)

# write.csv(
#   final_results_wide, 
#   paste0(dir_output, "vegetation/Farauti_ss_North_QLD_with_veg_coverage_wideformat.csv"),
#   row.names = FALSE
# )
