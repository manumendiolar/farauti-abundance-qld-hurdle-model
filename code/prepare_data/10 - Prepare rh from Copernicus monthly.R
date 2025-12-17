## R Script for extracting and computing mean monthly RH values from Copernicus
## (Batch Processing) at specific locations
## Manuela, M.
## 05-03-2025


# Libraries --------------------------------------------------------------------
library(ncdf4)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(fst)
library(stringr)
library(tictoc)
library(sf)


# Directories ------------------------------------------------------------------

# HPC
#dir_input <- "data/"
#dir_output <- "outputs/"

# When running via local machine
dir_input <- "Z:/work/data/" #dir_base
dir_output <- "Z:/work/outputs/"




# Load Mozzie Locations --------------------------------------------------------
data_andrew <- read.csv(paste0(dir_input, "mozzie/Farauti_numbers_North_QLD-Andrew_aggreg_trap.csv"))

# Check
head(data_andrew)

# Convert into a spatial object
locations <- data_andrew[, c("UID", "lon", "lat")]



# Function to pre-compute indices for locations --------------------------------
precompute_indices <- function(example_ncfile, locations) {
  
  # Open NetCDF file
  nc <- nc_open(example_ncfile)
  
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
    indices$lon_index[i] <- which.min(abs(lon - locations$lon[i]))
    indices$lat_index[i] <- which.min(abs(lat - locations$lat[i]))
  }
  
  # Close NetCDF file
  nc_close(nc)
  
  return(indices)
}


# Function to fill missing values (using mean of nearest neighbors) ------------
fill_na_with_neighbors <- function(nc_data, lon_idx, lat_idx, 
                                   buffer_size = 3, max_buffer_size = 21) {
  
  # Generate a grid of neighboring points based on the current buffer size
  buffer <- expand.grid(
    lon_idx + (-floor(buffer_size/2):floor(buffer_size/2)), 
    lat_idx + (-floor(buffer_size/2):floor(buffer_size/2))
  )
  
  # Ensure neighbors are within valid range
  buffer <- buffer[
    buffer[,1] > 0 & buffer[,1] <= dim(nc_data)[1] &
      buffer[,2] > 0 & buffer[,2] <= dim(nc_data)[2], 
  ]
  
  # Extract neighbor values at the specified time index
  neighbor_values <- sapply(seq_len(nrow(buffer)), function(j) {
    nc_data[buffer[j, 1], buffer[j, 2]]
  })
  
  # Filter out NA values
  valid_values <- neighbor_values[!is.na(neighbor_values)]
  
  if (length(valid_values) > 0) {
    return(mean(valid_values, na.rm = TRUE))
  } else if (buffer_size < max_buffer_size) {
    # Increase buffer size (adjust step as needed)
    buffer_size <- buffer_size + 2
    # Re-compute with new buffer size
    return(fill_na_with_neighbors(nc_data, lon_idx, lat_idx,
                                  buffer_size = buffer_size, 
                                  max_buffer_size = max_buffer_size))
  } else {
    # Return NA if maximum buffer size reached without finding valid neighbors
    return(NA)
  }
}


# Function to process a single NetCDF file and return daily data for all locations ------
process_nc_file <- function(ncfile, indices, varname = "Relative_Humidity_2m_15h") {
  
  # Open NetCDF file
  nc <- nc_open(ncfile)
  
  # Extract longitude, latitude, and time
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
  
  # Convert time to actual dates
  time_units <- ncatt_get(nc, "time", "units")$value
  time_origin <- as.POSIXct(sub("days since ", "", time_units), tz = "UTC")
  dates <- time_origin + time * 86400
  
  # Read the main variable
  rh_data <- ncvar_get(nc, varname)
  
  nc_close(nc)
  
  # For each location, extract the daily value
  # (If the file has only one day, there's just one time index)
  results <- lapply(seq_len(nrow(indices)), function(i) {
    lon_idx <- indices$lon_index[i]
    lat_idx <- indices$lat_index[i]
    
    # If the file has multiple time steps, sapply over them
    # If it’s only one time step, you can directly extract
    rh_values <- sapply(seq_along(dates), function(t_idx) {
      val <- rh_data[lon_idx, lat_idx]
      # Fill NA if needed
      if (is.na(val)) {
        val <- fill_na_with_neighbors(rh_data, lon_idx, lat_idx)
      }
      return(val)
    })
    
    # Pre-allocate results
    data.table(
      UID = indices$UID[i],
      lon = indices$lon[i],
      lat = indices$lat[i],
      closest_lon = indices$closest_lon,
      closest_lat = indices$closest_lat,
      date = dates,
      rh_value = rh_values
    )
  })
  
  # Combine all locations
  daily_dt <- rbindlist(results)
  
  return(daily_dt)
}



# List all NetCDF files --------------------------------------------------------

# List all folders in the main directory
folders <- list.dirs(
  paste0(dir_input, "humidity/Copernicus/2m-relative-humidity/"), 
  full.names = TRUE, 
  recursive = FALSE
)

# Extract years from folders
folders_years <- sub(".*_(\\d{4})$", "\\1", folders)

# Keep only NetCDF files matching data_nigel years
target_years <- unique(data_andrew$year)
# Extract the year from the folder name (assuming it's the last 4 digits)
extracted_years <- as.integer(str_extract(basename(folders), "\\d{4}$"))
# Filter folders based on target_years
folders <- folders[extracted_years %in% target_years]


# Precompute indices using one example file from the first year directory
tic("Precompute Indices")
example_year_dir <- folders[1]
example_file <- list.files(example_year_dir, pattern = "\\.nc$", full.names = TRUE)[1]
indices <- precompute_indices(example_file, locations)
toc()


# Process all NetCDF files
tic("Processing NetCDF files")

for (folder in folders) {
  
  tic(paste0("Processing folder:", folder))
  
  # List all daily NetCDF files in this folder
  nc_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  if (length(nc_files) == 0) next  # Skip if no files
  
  daily_list <- lapply(nc_files, function(f) {
    process_nc_file(f, indices, varname = "Relative_Humidity_2m_15h")  
  })
  
  # Combine daily data for the entire year
  year_data <- rbindlist(daily_list)
  
  # Compute year, month from the date
  year_data[, `:=`(
    year  = year(date),
    month = month(date)   
  )]
  
  # Compute monthly means for each UIN
  monthly_means <- year_data[, .(
    rh_mean = mean(rh_value, na.rm = TRUE)
  ), by = .(UID, year, month)]
  
  # Save intermediate results to disk (optional, for very large data)
  temp_output <- paste0(dir_output, "humidity/Copernicus/rh_1979-2024_andrew_locations_temp_results_")
  write_fst(monthly_means, paste0(temp_output, unique(monthly_means$year), ".fst"), compress = 50)
  
  toc()
}
toc()


# ------------------------------------------------------------------------------
# STEP 2: MERGE ALL INTERMEDIATE RESULTS INTO A FINAL DATASET
# ------------------------------------------------------------------------------

# List all saved intermediate files
temp_files <- list.files(path = paste0(dir_output, "humidity/Copernicus/"), 
                         pattern = "rh_1979-2024_andrew_locations_temp_results_.*\\.fst$", 
                         full.names = TRUE)

# Read and combine all intermediate results
final_results <- rbindlist(lapply(temp_files, read_fst), use.names = TRUE, fill = TRUE)

# Display First Few Rows
head(final_results)



# ------------------------------------------------------------------------------
# STEP 3: INTERACTIVE MAPS
# ------------------------------------------------------------------------------
library(leaflet)

# Ensure both datasets are data.tables
setDT(final_results)
setDT(data_andrew)

# Select only relevant columns from data_nigel
data_andrew_selected <- data_andrew[, .(UID, lon, lat)]

# Merge datasets using UIN as key
final_results <- merge(final_results, data_andrew_selected, by = "UID", all.x = TRUE)

# Filter data for a selected year and month
selected_year <- 1995
selected_month <- 6

# Convert month number to column name (e.g., "precip_1")
month_col <- paste0("rh_mean_", selected_month)

# Extract rh_tmax_mean values for selected year
data_selected <- final_results[year == selected_year & month == selected_month, .(UID, lon, lat, rh_mean)]

# Identify missing values
data_selected[, missing := is.na(rh_mean)]

# Convert to sf object for spatial plotting
data_sf <- st_as_sf(data_selected, coords = c("lon", "lat"), crs = 4326)  # WGS84 (EPSG:4326)

# Check how many NA values exist
sum(is.na(data_selected$rh_mean))

# View summary of rh_tmax_mean values
summary(data_selected$rh_mean)

# Define color scale with 0 as the minimum
min_rh_mean <- min(data_selected$rh_mean, na.rm = TRUE)  # Force legend to start at 0
max_rh_mean <- max(data_selected$rh_mean, na.rm = TRUE)  # Keep max as the real max value

# Define color palette
library(viridis)
pal <- colorNumeric(palette = "viridis", domain = c(min_rh_mean, max_rh_mean), na.color = "gray")

# Create Interactive Leaflet Map with Precipitation Values
map <- leaflet(data_selected) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  # Change Tile Layer Here
  addControl(
    html = paste0("<h4 style='text-align:left; font-weight:bold;'>RH map: ",selected_year, "-", selected_month,"<br>Andrew's locations </h4>"), 
    position = "topleft"
  ) %>%  
  addCircleMarkers(
    ~lon, ~lat,
    color = ~pal(rh_mean),
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0("<b>UID:</b> ", UID, "<br>",
                    "<b>Year:</b> ", selected_year, "<br>",
                    "<b>Month:</b> ", selected_month, "<br>",
                    "<b>RH:</b> ", round(rh_mean, 2), "%")
  ) %>%
  addLegend(
    "bottomright", pal = pal, values = c(min_rh_mean, max_rh_mean),
    title = "RH",
    opacity = 1
  )

# Explore map
map

# Save map as an interactive HTML file
library(htmlwidgets)
saveWidget(
  map, 
  file = paste0(dir_output, "humidity/Copernicus/rh_1979-2024_map_at_Andrew_locations_",selected_year,"-",selected_month,".html"),
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
  UID + year ~ month, 
  value.var = "rh_mean"
)

# Rename columns to reflect months
setnames(filtered_results_wide, paste0(1:12), paste0("rh_mean_", 1:12))

# Display the first few rows
head(filtered_results_wide)

# columns to keep
columns_to_keep <- c("UID", "year", paste0("rh_mean_", 1:12))  # Define required columns

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

# Rename
setnames(data_mozzie_updated, 
         old = paste0("rh_mean_", 1:12), 
         new = paste0("rh_", 1:12))

# Check
head(data_mozzie_updated)

# Check missing values or NaNs
summary(data_mozzie_updated)


# Save
write_fst(
  data_mozzie_updated,
  path = paste0(dir_output, "humidity/Farauti_numbers_North_QLD-Andrew_with_Copernicus_monthly_mean_rh.fst"),
  compress = 50
)



