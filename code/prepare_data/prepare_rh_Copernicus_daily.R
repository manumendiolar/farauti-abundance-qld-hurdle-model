# Preparing data to run models
# Manuela, M.
# 22-01-2025

# Compute rh and mean rh over 21d prior of target date using Copernicus  data


# Libraries --------------------------------------------------------------------
library(ncdf4)
library(dplyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(tictoc)
library(data.table)
library(fst)
library(future.apply) 
library(tictoc)
library(zoo)


# Functions --------------------------------------------------------------------

precompute_indices <- function(ncfile, points, bounding_box) {
  
  # Function to precompute indices of ncfile
  # - Maps each centroid to the closest longitude and latitude in the NetCDF grid.
  # - Optionally crops the grid to a bounding box for performance.
  # Returns a data.table with:
  # grid_id: unique identifier in points dataset
  # original_lon: centroid coordinates
  # original_lat: centroid coordinates
  # closest_lon: closest longitude in NetCDF grid
  # closest_lat: closest latitude in NetCDF grid,
  # lon_index: index in NetCDF grid for closest_lon
  # lat_index: index in NetCDF grid for closest_lat
  
  
  # Open the NetCDF file
  nc <- nc_open(ncfile)
  
  # Extract longitude and latitude
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  # Option to crop the nc_file
  if ( !any(is.na(bounding_box)) ){
    
    # Find indices within the bounding box
    lon_indices <- which(lon >= bounding_box$lon_min & lon <= bounding_box$lon_max)
    lat_indices <- which(lat >= bounding_box$lat_min & lat <= bounding_box$lat_max)
    
    # Subset longitude and latitude
    lon <- lon[lon_indices]
    lat <- lat[lat_indices]
  }
  
  # Vectorized computation of closest indices
  closest_lon <- sapply(points$lon, function(x) which.min(abs(lon - x)))
  closest_lat <- sapply(points$lat, function(y) which.min(abs(lat - y)))
  
  # Create a data.table with precomputed indices
  results <- data.table(
    UID = points$UID,
    original_lon = points$lon,
    original_lat = points$lat,
    closest_lon = lon[closest_lon],
    closest_lat = lat[closest_lat],
    lon_index = closest_lon,
    lat_index = closest_lat
  )
  
  # Close the NetCDF file
  nc_close(nc)
  
  return(results)
}


process_nc_file <- function(ncfile, indices, bounding_box) {
  
  # Function to process a NetCDF file using pre-computed indices
  # - Extracts relative humidity (RH) values for given centroids based on their closest grid indices.
  # - Optionally crops the NetCDF data to a specified bounding box for better performance.
  # - Converts time variable to actual dates.
  # - Uses vectorized operations to extract RH values across time steps.
  # Returns a data.table containing:
  # grid ID
  # original_lon, original_lat: centroid coordinates
  # closest_lon, closest_lat: closest grid points
  # date
  # rh_value
  
  # Open the NetCDF file
  nc <- nc_open(ncfile)
  
  # Extract longitude, latitude, and time
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
  
  # Convert time to actual dates
  time_units <- ncatt_get(nc, "time", "units")$value
  time_origin <- as.POSIXct(sub("days since ", "", time_units), tz = "UTC")
  dates <- time_origin + time * 86400
  
  # Crop data to bounding box
  if (!any(is.na(bounding_box))) {
    lon_indices <- which(lon >= bounding_box$lon_min & lon <= bounding_box$lon_max)
    lat_indices <- which(lat >= bounding_box$lat_min & lat <= bounding_box$lat_max)
    lon <- lon[lon_indices]
    lat <- lat[lat_indices]
    rh_data <- ncvar_get(nc, "Relative_Humidity_2m_15h")[lon_indices, lat_indices]
  } else {
    rh_data <- ncvar_get(nc, "Relative_Humidity_2m_15h")
  }
  
  # Vectorized extraction of RH values
  rh_values <- mapply(function(lon_idx, lat_idx) {
    rh_data[lon_idx, lat_idx ]
  }, indices$lon_index, indices$lat_index)
  
  # Pre-allocate results
  n <- nrow(indices) * length(dates)
  results <- data.table(
    UID = rep(indices$UID, each = length(dates)),
    original_lon = rep(indices$original_lon, each = length(dates)),
    original_lat = rep(indices$original_lat, each = length(dates)),
    closest_lon = rep(indices$closest_lon, each = length(dates)),
    closest_lat = rep(indices$closest_lat, each = length(dates)),
    date = rep(dates, times = nrow(indices)),
    rh_value = as.numeric(t(rh_values))
  )
  
  # Close the NetCDF file
  nc_close(nc)
  
  return(results)
}


process_folder <- function(folder, indices, bounding_box, output_dir) {
  
  # Define function to process a single folder
  # As of now it is quite a local function...
  
  # Extract year
  year <- substr(basename(folder), nchar(basename(folder)) - 3, nchar(basename(folder)))
  cat("Processing folder for year:", year, "\n")
  
  # List all NetCDF files
  nc_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  
  # Process all NetCDF files in parallel
  results <- future_lapply(nc_files, function(nc_file) {
    process_nc_file(nc_file, indices, bounding_box)
  })
  
  # Combine results into a single data.table
  folder_results <- rbindlist(results, use.names = TRUE, fill = TRUE)
  
  # Save results to .fst
  output_file <- paste0(output_dir, "humidity/Copernicus/relative_humidity_at_Farauti_ss_North_QLD_", year, ".fst")
  write_fst(folder_results, output_file, compress = 50)
  cat("Results saved for year:", year, "\n")
}


# Directories ------------------------------------------------------------------

# Bowen
# dir_input <- "work/data/"
# dir_output <- "work/outputs/"

# Local
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"


# Bounding box -----------------------------------------------------------------
bounding_box <- data.frame(
  lon_min = 135, lon_max = 155,
  lat_min = -30, lat_max = -8
)

# More specific (for grid)
# bounding_box <- data.frame(
#   lon_min = 138, lon_max = 154,
#   lat_min = -29, lat_max = -9.1
#   )



# Define the points ------------------------------------------------------------
data <- read.csv(paste0(dir_input,"mozzie/Farauti_ss_North_QLD.csv"))
data <- data %>% mutate(date = dmy(date), year = year(date), month = month(date), day = day(date))

# Aggregate no.trap by all columns except trap
data <- data %>%
  group_by(species, date, week, season, habitat, site, lat, lon, site2, year, month, day) %>%
  summarise(no.trap = sum(no.trap), .groups = "drop")

# Add a unique identifier column
data <- data %>% mutate(UID = 1:dim(data)[1])

# Check
head(data, 10)

# Keep only coordinates
points <- data[ , c("UID","lon","lat")] # according to the colnames in file read above

# Inspect
head(points)


# List all NetCDF files --------------------------------------------------------

# List all folders in the main directory
folders <- list.dirs(
  paste0(dir_input, "humidity/Copernicus/2m-relative-humidity/"), 
  full.names = TRUE, 
  recursive = FALSE
)

# Extract years from folders
folders_years <- sub(".*_(\\d{4})$", "\\1", folders)

# Specify the years you're looking for
target_years <- as.character(seq(min(year(data$date)), max(year(data$date)), 1))

# Find nc_files within time window of data
folders <- folders[which(folders_years %in% target_years)]

# Precompute indices using the first folder's NetCDF file
first_nc_file <- list.files(folders[1], pattern = "\\.nc$", full.names = TRUE)[1]
tic("Computing indices...")
indices <- precompute_indices(first_nc_file, points, bounding_box = bounding_box)
toc()

# Main script
tic("Overall processing time")
for (folder in folders)  process_folder(folder, indices, bounding_box, dir_output)
toc()




# Combine files and filter -----------------------------------------------------

# Load only 1995, 1996 and 1997 
file_paths <- c(
  paste0(dir_output, "humidity/Copernicus/relative_humidity_at_Farauti_ss_North_QLD_1995.fst"),
  paste0(dir_output, "humidity/Copernicus/relative_humidity_at_Farauti_ss_North_QLD_1996.fst"),
  paste0(dir_output, "humidity/Copernicus/relative_humidity_at_Farauti_ss_North_QLD_1997.fst")
)

# Read and combine the files into a single data.table
final_results <- rbindlist(
  lapply(file_paths, read_fst, as.data.table = TRUE), 
  use.names = TRUE
)

# Check the combined data
head(final_results)
summary(final_results)



# Compute mean rh_tmax over the 'x' days prior ---------------------------------

# Ensure the date column is in Date format
final_results[, date := as.Date(date)]

# Ensure `final_results` is sorted by location and date
setorder(final_results, closest_lon, closest_lat, date)

# Calculate the mean rh_tmax over the last 21 days
final_results <- final_results %>%
  group_by(UID, closest_lon, closest_lat) %>% # Group by UID, lon, lat
  arrange(date) %>%                           # Ensure data is ordered by date
  mutate(
    rh_value_21d = rollapply(
      rh_value, 
      width = 21, 
      FUN = mean, 
      align = "right", 
      fill = NA,
      na.rm = TRUE
    )) %>%
  ungroup()

# Inspect first 25 rows
head(final_results, 25)

# Formatting
setDT(data)
setDT(final_results)

# Ensure lon and lat columns in both datasets have the same type (num or int) for proper matching
final_results[, `:=`(UID = as.numeric(UID))]
data[, `:=`(UID = as.numeric(UID))]

# Merge the tables on 'date' and 'UID', keeping all rows in 'data' (left join)
merged_data <- merge(
  data,
  final_results[, .(date, UID, rh_value, rh_value_21d)], 
  by = c("date", "UID"),
  all.x = TRUE
)

# Check
head(merged_data)

# Rename
setnames(merged_data, "rh_value", "rh")
setnames(merged_data, "rh_value_21d", "mean_rh_21d")

# Check
head(merged_data)


# Save -------------------------------------------------------------------------
write_fst(
  merged_data, 
  paste0(dir_output, "humidity/Copernicus/Farauti_ss_North_QLD_with_Copernicus_rh_1995-1997.fst")
)

write.csv(
  merged_data, 
  paste0(dir_output, "humidity/Copernicus/Farauti_ss_North_QLD_with_Copernicus_rh_1995-1997.csv"),
  row.names = FALSE
)



