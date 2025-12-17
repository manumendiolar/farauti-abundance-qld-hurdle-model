## R Script for extracting and computing mean monthly rh_tmax values from SILO (Batch Processing) at specific locations
## Manuela, M.
## 04-03-2025


# Libraries --------------------------------------------------------------------
library(ncdf4)
library(dplyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(tictoc)
library(data.table)
library(fst)
library(tictoc)
library(zoo)
library(sf)


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
locations <- data_andrew[, c("UID", "lon", "lat")]



# Function to pre-compute indices for locations --------------------------------
precompute_indices <- function(ncfile, locations) {
  
  # Open NetCDF file
  nc <- nc_open(ncfile)
  
  # Extract lon and lat
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  # Pre-allocate data.table
  indices <- data.table(
    UID = locations$UID,
    lon = locations$lon,
    lat = locations$lat,
    lon_index = NA_integer_,
    lat_index = NA_integer_
  )
  
  # Find nearest grid indices for each location
  for (i in 1:nrow(locations)) {
    lon_index <- which.min(abs(lon - locations$lon[i]))
    lat_index <- which.min(abs(lat - locations$lat[i]))
    
    indices$lon_index[i] <- lon_index
    indices$lat_index[i] <- lat_index
  }
  
  # Close NetCDF file
  nc_close(nc)
  
  return(indices)
}

# Function to fill missing values (using mean of nearest neighbors) ------------
fill_na_with_neighbors <- function(nc_data, lon_idx, lat_idx, time_idx, buffer_size = 3, max_buffer_size = 21) {
  
  # Generate a grid of neighboring points based on the current buffer size
  buffer <- expand.grid(lon_idx + (-floor(buffer_size/2):floor(buffer_size/2)), 
                        lat_idx + (-floor(buffer_size/2):floor(buffer_size/2)))
  
  # Ensure neighbors are within valid range
  buffer <- buffer[
    buffer[,1] > 0 & buffer[,1] <= dim(nc_data)[1] &   # Longitude bounds
      buffer[,2] > 0 & buffer[,2] <= dim(nc_data)[2],       # Latitude bounds
  ]
  
  # Extract neighbor values at the specified time index
  neighbor_values <- sapply(1:nrow(buffer), function(j) {
    nc_data[buffer[j, 1], buffer[j, 2], time_idx]
  })
  
  # Filter out NA values
  valid_values <- neighbor_values[!is.na(neighbor_values)]
  
  # If there are valid values, return their mean; otherwise, increase the buffer size and try again
  if (length(valid_values) > 0) {
    return(mean(valid_values, na.rm = TRUE))
  } else if (buffer_size < max_buffer_size) {
    # Increase buffer size (adjust step as needed)
    buffer_size <- buffer_size + 2 
    # Re-compute with new buffer size
    return(fill_na_with_neighbors(nc_data, lon_idx, lat_idx, time_idx, buffer_size = buffer_size, max_buffer_size = max_buffer_size))
  } else {
    # Return NA if maximum buffer size has been reached without finding any valid neighbors
    return(NA)  
  }
}

# Function to extract rh_tmax values & Fill NA at locations --------------------
process_nc_file <- function(ncfile, indices) {
  
  # Open NetCDF file
  nc <- nc_open(ncfile)
  
  # Extract time and `rh_tmax` values
  time <- ncvar_get(nc, "time")
  rh_tmax <- ncvar_get(nc, "rh_tmax")
  
  # Convert time to actual dates
  time_units <- ncatt_get(nc, "time", "units")$value
  time_origin <- as.POSIXct(sub("days since ", "", time_units), tz = "UTC")
  dates <- time_origin + time * 86400
  
  # Extract values for each location
  results <- lapply(1:nrow(indices), function(i) {
    lon_idx <- indices$lon_index[i]
    lat_idx <- indices$lat_index[i]
    
    rh_values <- sapply(1:length(dates), function(t) {
      val <- rh_tmax[lon_idx, lat_idx, t]
      
      # If NA, fill using mean of neighbors
      if (is.na(val)) {
        val <- fill_na_with_neighbors(rh_tmax, lon_idx, lat_idx, t)
      }
      
      return(val)
    })
    
    data.table(
      UID = indices$UID[i],
      lon = indices$lon[i], lat = indices$lat[i],
      date = dates, year = year(dates), month = month(dates),
      rh_tmax_value = rh_values
    )
  })
  
  # Close NetCDF file
  nc_close(nc)
  
  return(rbindlist(results))
}


# Process All NetCDF Files & Compute Monthly Means -----------------------------

# List all NetCDF files
nc_files <- list.files(
  path = paste0(dir_input, "humidity/SILO/rh_tmax/"), 
  pattern = "\\.nc$", 
  full.names = TRUE
)

# Extract years from file names
nc_files_years <- as.integer(sub(".*?/([0-9]{4})\\..*", "\\1", nc_files))

# Keep only NetCDF files matching data_nigel years
target_years <- unique(data_andrew$year)
nc_files <- nc_files[nc_files_years %in% target_years]

# Precompute indices from the first NetCDF file
tic("Precompute Indices")
indices <- precompute_indices(nc_files[1], locations)
toc()

# Process all NetCDF files
tic("Processing NetCDF files")
all_results <- list()

for (file in nc_files) {
  cat("Processing:", file, "\n")
  tic(paste("Processing file:", file))
  
  file_results <- process_nc_file(file, indices)
  
  # Compute mean rh_tmax for each UID, year, and month
  file_results <- file_results[, .(rh_tmax_mean = mean(rh_tmax_value, na.rm = TRUE)), 
                               by = .(UID, year, month)]
  
  # Save intermediate results to disk (optional, for very large data)
  temp_output <- paste0(dir_output, "humidity/SILO/rh_tmax_1900-2024_andrew_locations_temp_results_")
  write_fst(file_results, paste0(temp_output, unique(file_results$year), ".fst"), compress = 50)
  toc()
}


# ------------------------------------------------------------------------------
# STEP 2: MERGE ALL INTERMEDIATE RESULTS INTO A FINAL DATASET
# ------------------------------------------------------------------------------

# List all saved intermediate files
temp_files <- list.files(path = paste0(dir_output, "humidity/SILO/"), 
                         pattern = "rh_tmax_1900-2024_andrew_locations_temp_results_.*\\.fst$", 
                         full.names = TRUE)

# Read and combine all intermediate results
final_results <- rbindlist(lapply(temp_files, read_fst), use.names = TRUE, fill = TRUE)

# Display First Few Rows
head(final_results)

# Save Final Results
file_name <- paste0(dir_output, "humidity/SILO/rh_tmax_1900-2024_at_Farauti_numbers_AUS-Nigel_locations.fst")
write_fst(final_results, file_name, compress = 50)

# Optional: Delete intermediate files after merging (Uncomment if needed)
#file.remove(temp_files)

# Read
# file_name <- paste0(dir_output, "humidity/SILO/rh_tmax_1985-2000_at_Farauti_numbers_AUS-Nigel_locations.fst")
# final_results <- read_fst(file_name)
# head(final_results)



# ------------------------------------------------------------------------------
# STEP 3: INTERACTIVE MAPS
# ------------------------------------------------------------------------------

# Ensure both datasets are data.tables
setDT(final_results)
setDT(data_andrew)

# Select only relevant columns from data_nigel
data_andrew_selected <- data_andrew[, .(UID, lon, lat)]

# Merge datasets using UIN as key
final_results <- merge(final_results, data_andrew_selected, by = "UID", all.x = TRUE)

# Filter data for a selected year and month
selected_year <- 1995
selected_month <- 4

# Convert month number to column name (e.g., "precip_1")
month_col <- paste0("rh_tmax_mean_", selected_month)

# Extract rh_tmax_mean values for selected year
data_selected <- final_results[year == selected_year & month == selected_month, .(UID, lon, lat, rh_tmax_mean)]

# Identify missing values
data_selected[, missing := is.na(rh_tmax_mean)]

# Convert to sf object for spatial plotting
data_sf <- st_as_sf(data_selected, coords = c("lon", "lat"), crs = 4326)  # WGS84 (EPSG:4326)

# Check how many NA values exist
sum(is.na(data_selected$rh_tmax_mean))

# View summary of rh_tmax_mean values
summary(data_selected$rh_tmax_mean)

# Define color scale with 0 as the minimum
min_rh_tmax_mean <- min(data_selected$rh_tmax_mean, na.rm = TRUE)  # Force legend to start at 0
max_rh_tmax_mean <- max(data_selected$rh_tmax_mean, na.rm = TRUE)  # Keep max as the real max value

# Define color palette
library(viridis)
pal <- colorNumeric(palette = "viridis", domain = c(min_rh_tmax_mean, max_rh_tmax_mean), na.color = "gray")

# Create Interactive Leaflet Map with Precipitation Values
map <- leaflet(data_selected) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  # Change Tile Layer Here
  addControl(
    html = paste0("<h4 style='text-align:left; font-weight:bold;'>RH tmax map: ",selected_year, "-", selected_month,"<br>Andrew's locations </h4>"), 
    position = "topleft"
  ) %>%  
  addCircleMarkers(
    ~lon, ~lat,
    color = ~pal(rh_tmax_mean),
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0("<b>UID:</b> ", UID, "<br>",
                    "<b>Year:</b> ", selected_year, "<br>",
                    "<b>Month:</b> ", selected_month, "<br>",
                    "<b>RH tmax:</b> ", round(rh_tmax_mean, 2), "%")
  ) %>%
  addLegend(
    "bottomright", pal = pal, values = c(min_rh_tmax_mean, max_rh_tmax_mean),
    title = "RH tmax",
    opacity = 1
  )

# Explore map
map

# Save map as an interactive HTML file
library(htmlwidgets)
saveWidget(
  map, 
  file = paste0(dir_output, "humidity/SILO/rh_tmax_1900-2024_map_at_Andrews_locations_",selected_year,"-",selected_month,".html"),
  selfcontained = TRUE
)


# ------------------------------------------------------------------------------
# STEP 4: COMBINE MOZZIE DATA WITH HUMIDITY DATA 
# ------------------------------------------------------------------------------

# Ensure both datasets are data.tables
setDT(final_results)
setDT(data_andrew)

# Keep only years present in data_nigel
filtered_results <- final_results[year %in% unique(data_andrew$year)]

# Reshape data from long to wide format
filtered_results_wide <- dcast(
  filtered_results, 
  UID + year + lon + lat ~ month, 
  value.var = "rh_tmax_mean"
)

# Rename columns to reflect months
setnames(filtered_results_wide, paste0(1:12), paste0("rh_tmax_mean_", 1:12))

# Display the first few rows
head(filtered_results_wide)

# columns to keep
columns_to_keep <- c("UID", "year", paste0("rh_tmax_mean_", 1:12))  # Define required columns

# Use .. to properly select columns in data.table
filtered_results_wide <- filtered_results_wide[, ..columns_to_keep]


# Merge with data_nigel to retain locations
data_mozzie_updated <- merge(
  data_andrew,
  filtered_results_wide, 
  by = c("UID", "year"), 
  all.x = TRUE # Ensures all `data_andrew` rows remain
)

# View the first few rows
head(data_mozzie_updated)

# Check missing values or NaNs
summary(data_mozzie_updated)

# Rename
setnames(data_mozzie_updated, 
         old = paste0("rh_tmax_mean_", 1:12), 
         new = paste0("rh_tmax_", 1:12))
# Check
head(data_mozzie_updated)

# Save
write_fst(
  data_mozzie_updated,
  path = paste0(dir_output, "humidity/Farauti_numbers_North_QLD-Andrew_with_SILO_monthly_mean_rh_tmax.fst"),
  compress = 50
)
