## R Script for extracting monthly climatic variables from WorldClim (Batch Processing) at specific locations
## Manuela, M.
## 04-03-2025

# Libraries --------------------------------------------------------------------
library(terra)       # For handling raster data
library(data.table)  # Faster data manipulation
library(fst)         # Fast file saving
library(tidyr)
library(leaflet)
library(tidyverse)


# Directories ------------------------------------------------------------------

# Petrichor
#dir_input <- "data/"
#dir_output <- "outputs/"

# When running via local machine
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"




# Load Mozzie Locations --------------------------------------------------------
data_andrew <- read.csv(paste0(dir_input, "mozzie/Farauti_numbers_North_QLD-Andrew_aggreg_trap.csv"))

# Check
head(data_andrew)

# Convert into a spatial object
locations <- vect(data_andrew[, c("UID", "lon", "lat")], geom = c("lon", "lat"), crs = "EPSG:4326")




# Tmax Data --------------------------------------------------------------------

# Folder with all tif files
tif_folder <- paste0(dir_input, "temperature/WorldClim/historical/tmax/wc2.1_cruts4.06_2.5m_tmax_1960-2021/")

# List all .tif files
tif_files <- list.files(path = tif_folder, pattern = "\\.tif$", full.names = TRUE)

# Extract year and month from file names
tif_info <- data.table(
  file = tif_files,
  year = as.integer(sub(".*_(\\d{4})-(\\d{2})\\.tif", "\\1", tif_files)),
  month = as.integer(sub(".*_(\\d{4})-(\\d{2})\\.tif", "\\2", tif_files))
)

# Process .tif files in **BATCHES by year**
for (yr in unique(tif_info$year)[1]) {
  
  # Subset files for this year (12 months)
  year_files <- tif_info[year == yr]
  
  # Create an empty data.table for this year
  year_data <- data.table(UID = data_andrew$UID, year = yr)
  
  tic(paste0("Processing year: ", yr))
  # Process each file separately
  for (i in 1:nrow(year_files)[1]) {
    
    # Load raster for this month
    raster <- rast(year_files$file[i])
    
    # Fill missing raster values using moving window
    # 3x3	Small gaps, detailed smoothing, high-resolution raster
    # 5x5	Medium gaps, balanced interpolation
    # 7x7	Large missing areas, strong smoothing
    focal_matrix <- matrix(1, nrow = 5, ncol = 5)  # adjust the size for wider interpolation
    
    # Apply focal function to fill NA values with the mean of surrounding pixels
    filled_raster <- focal(raster, w = focal_matrix, fun = mean, na.rm = TRUE, expand = TRUE)
    
    # Extract tmax values for locations
    tmax_values <- terra::extract(filled_raster, locations)
    
    # Convert to data.table and merge with year_data
    month_col <- paste0("tmax_", year_files$month[i])  # e.g., "tmax_1", "tmax_2"
    year_data[, (month_col) := tmax_values[, 2]]
    
  }
  toc()
  # Save intermediate results to disk (optional, for very large data)
  temp_output <- paste0(dir_output, "temperature/Worldclim/wc2.1_cruts4.06_2.5m_tmax_1960-2021_andrew_locations_temp_results_")
  write_fst(year_data, paste0(temp_output, yr, ".fst"), compress = 50)
  
  # Free up memory
  rm(year_data)
  gc()
}

# ------------------------------------------------------------------------------
# STEP 2: MERGE ALL INTERMEDIATE RESULTS INTO A FINAL DATASET
# ------------------------------------------------------------------------------

# List all saved intermediate files
temp_files <- list.files(path = paste0(dir_output, "temperature/Worldclim/"), 
                         pattern = "wc2.1_cruts4.06_2.5m_tmax_1960-2021_andrew_locations_temp_results_.*\\.fst$", 
                         full.names = TRUE)

# Read and combine all intermediate results
final_results <- rbindlist(lapply(temp_files, read_fst), use.names = TRUE, fill = TRUE)

# Display First Few Rows
head(final_results)

# Save Final Results
file_name <- paste0(dir_output, "temperature/Worldclim/wc2.1_cruts4.06_2.5m_tmax_1960-2021_at_Farauti_numbers_North_QLD-Andrew_locations.fst")
write_fst(final_results, file_name, compress = 50)

# Read
final_results <- read_fst(file_name)
head(final_results)


# ------------------------------------------------------------------------------
# STEP 3: INTERACTIVE MAPS
# ------------------------------------------------------------------------------

# Load Libraries
library(leaflet)
library(data.table)
library(sf)

# Ensure both datasets are data.tables
setDT(final_results)
setDT(data_andrew)

# Select only relevant columns from data_nigel
data_andrew_selected <- data_andrew[, .(UID, lon, lat)]

# Merge datasets using UIN as key
final_results <- merge(final_results, data_andrew_selected, by = "UID", all.x = TRUE)

# Filter data for a selected year and month
selected_year <- 1996
selected_month <- 6

# Convert month number to column name (e.g., "tmax_1")
month_col <- paste0("tmax_", selected_month)

# Extract tmax values for selected year
data_selected <- final_results[year == selected_year, .(UID, lon, lat, tmax = get(month_col))]

# Identify missing values
data_selected[, missing := is.na(tmax)]

# Convert to sf object for spatial plotting
data_sf <- st_as_sf(data_selected, coords = c("lon", "lat"), crs = 4326)  # WGS84 (EPSG:4326)

# Check how many NA values exist
sum(is.na(data_selected$tmax))

# View summary of tmax values
summary(data_selected$tmax)

# Define color scale with 0 as the minimum
min_tmax <- 0  # Force legend to start at 0
max_tmax <- max(data_selected$tmax, na.rm = TRUE)  # Keep max as the real max value

# Define color palette
pal <- colorNumeric(palette = "OrRd", domain = c(min_tmax, max_tmax), na.color = "gray")

# Create interactive Leaflet map with tmax values
map <- leaflet(data_selected) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  # Change Tile Layer Here
  addControl(
    html = paste0("<h4 style='text-align:left; font-weight:bold;'>tmax map: ",selected_year, "-", selected_month,"<br>Andrew's locations </h4>"), 
    position = "topright"
  ) %>%  
  addCircleMarkers(
    ~lon, ~lat,
    color = ~pal(tmax),
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0("<b>UID:</b> ", UID, "<br>",
                    "<b>Year:</b> ", selected_year, "<br>",
                    "<b>Month:</b> ", selected_month, "<br>",
                    "<b>Precipitation:</b> ", round(tmax, 2), " mm")
  ) %>%
  addLegend(
    "bottomright", pal = pal, values = c(min_tmax, max_tmax),
    title = "tmax (C)",
    opacity = 1
  )

# Explore map
map

# Save map as an interactive HTML file
library(htmlwidgets)
saveWidget(
  map, 
  file = paste0(dir_output, "temperature/Worldclim/wc2.1_cruts4.06_2.5m_tmax_map_at_Andrew_locations_",selected_year,"-",selected_month,".html"),
  selfcontained = TRUE
)


# ------------------------------------------------------------------------------
# STEP 4: COMBINE MOZZIE DATA WITH WORLDCLIM DATA 
# ------------------------------------------------------------------------------

# Ensure both datasets are data.tables
setDT(final_results)
setDT(data_andrew)

# Keep only years present in data_nigel
filtered_results <- final_results[year %in% unique(data_andrew$year)]

# columns to keep
columns_to_keep <- c("UID", "year", paste0("tmax_", 1:12))  # Define required columns

# Use .. to properly select columns in data.table
filtered_results <- filtered_results[, ..columns_to_keep]


# Merge with data_nigel to retain locations
data_mozzie_updated <- merge(
  data_andrew,
  filtered_results, 
  by = c("UID", "year"), 
  all.x = TRUE # Ensures all `data_nigel` rows remain
)

# View the first few rows
head(data_mozzie_updated)

# Check missing values or NaNs
summary(data_mozzie_updated)


# Save
write_fst(
  data_mozzie_updated,
  path = paste0(dir_output, "temperature/Farauti_numbers_North_QLD-Andrew_with_worldclim_tmax.fst"),
  compress = 50
)
