# Preparing data to run models
# Manuela, M.
# 22-01-2025


# Compute total rainfall over 21d prior of target date using NCI data
# This script computes the:
#   - accumulated precipitation during the 21 days prior to a target date
#   - accumulated precipitation of a target date
# at each Farauti observation location


# Libraries --------------------------------------------------------------------
library(ncdf4)
library(dplyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(fst)
library(tictoc)
library(zoo)



# Function to pre-compute indices for the points -------------------------------
precompute_indices <- function(ncfile, points) {
  
  # This is a more general function
  
  # Open the NetCDF file
  nc <- nc_open(ncfile)
  
  # Extract lon and lat
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  # Pre-allocate a data frame to store results
  results <- data.frame(
    UID = points$UID,
    original_lon = points$lon,
    original_lat = points$lat,
    closest_lon = NA,
    closest_lat = NA,
    lon_index = NA,
    lat_index = NA
  )
  
  # Loop through each point and find the closest lon/lat indices
  for (i in 1:nrow(points)) {
    # Compute distances to all grid points
    lon_diff <- abs(lon - points$lon[i])
    lat_diff <- abs(lat - points$lat[i])
    
    # Find the closest indices
    lon_index <- which.min(lon_diff)
    lat_index <- which.min(lat_diff)
    
    # Store results
    results$closest_lon[i] <- lon[lon_index]
    results$closest_lat[i] <- lat[lat_index]
    results$lon_index[i] <- lon_index
    results$lat_index[i] <- lat_index
  }
  
  # Close the NetCDF file
  nc_close(nc)
  
  # Return the results
  return(results)
}


# Function to process a file using pre-computed indices ------------------------
process_nc_file <- function(ncfile, indices) {
  
  # This function is specific for this type of file
  
  # Open the NetCDF file
  nc <- nc_open(ncfile)
  
  # Extract time and maximum temperature data
  time <- ncvar_get(nc, "time")
  precip <- ncvar_get(nc, "precip")
  
  # Convert time to actual dates
  time_units <- ncatt_get(nc, "time", "units")$value
  time_origin <- as.POSIXct(sub("days since ", "", time_units), tz = "UTC")
  dates <- time_origin + time * 86400
  
  # Extract precipitation values for the pre-computed indices
  results <- lapply(1:nrow(indices), function(i) {
    lon_idx <- indices$lon_index[i]
    lat_idx <- indices$lat_index[i]
    
    data.frame(
      UID = indices$UID[i],
      closest_lon = indices$closest_lon[i],
      closest_lat = indices$closest_lat[i],
      date = dates,
      precip_value = precip[lon_idx, lat_idx, ]
    )
  })
  
  # Close the NetCDF file
  nc_close(nc)
  
  # Combine results
  combined <- rbindlist(results)
  
  return(combined)
}


# Directories ------------------------------------------------------------------

# Petrichor
# dir_input <- "data/"
# dir_output <- "outputs/"

# Local
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"



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
nc_files <- list.files(
  path = paste0(dir_input, "precipitation/NCI/Daily total precipitation/"), 
  pattern = "\\.nc$", 
  full.names = TRUE
)

# Extract years from the file paths
nc_files_years <- sub(".*_(\\d{4})\\.nc$", "\\1", nc_files)

# Specify the years you're looking for
target_years <- as.character(seq(min(year(data$date)), max(year(data$date)), 1))

# Find nc_files within time window of data
nc_files <- nc_files[which(nc_files_years %in% target_years)]


# Pre-compute indices from the first file --------------------------------------
tic("Precompute Indices")
indices <- precompute_indices(nc_files[1], points)
toc()

# Check 
head(indices)



# Process all NetCDF files -----------------------------------------------------
tic("Processing some NetCDF files")

# Initialize an empty list to store results
all_results <- list()

# Loop through each NetCDF file
for (file in nc_files) {
  cat("Processing:", file, "\n")
  file_results <- process_nc_file(file, indices)
  all_results[[file]] <- file_results
}
# Combine all results into a single data table
final_results <- rbindlist(all_results)
toc()

# Inspect first few rows
head(final_results)

# Remove unnecessary elements
rm(file)
rm(all_results)
rm(file_results)
rm(nc_files, nc_files_years)
rm(target_years)


# Add year, month, and day columns  --------------------------------------------

# If final_results is a data.table this is a faster alternative
final_results[, `:=`(
  year = year(date),
  month = month(date),
  day = day(date)
)]

# Check
head(final_results)

# Identify duplicate rows
duplicate_rows <- duplicated(final_results)

# Summarize the result
any_duplicates <- any(duplicate_rows)  # TRUE if duplicates exist, FALSE otherwise
if(any_duplicates){
  cat("Are there duplicates in the data.table? ", any_duplicates, "\n")
  # Remove duplicate rows
  final_results <- unique(final_results)
} else {
  cat("Are there duplicates in the data.table? ", any_duplicates, "\n")
}
rm(any_duplicates)
rm(duplicate_rows)


# Compute accumulated precipitation over 'x' days ------------------------------

# Ensure the date column is in Date format
final_results[, date := as.Date(date)]

# Check
glimpse(final_results)

# Calculate the accumulated precipitation over the last 21 days
final_results <- final_results %>%
  group_by(UID, closest_lon, closest_lat) %>% # Group by UID, lon, lat
  arrange(date) %>%                          # Ensure data is ordered by date
  mutate(
    accum_precip_21d = rollapply(
      precip_value, 
      width = 21, 
      FUN = sum, 
      align = "right", 
      fill = NA,
      na.rm = TRUE
      )) %>%
  ungroup()

# Inspect first 25 rows
head(final_results, 25)



# Combine mozzie data and final_results ----------------------------------------

# Convert mozzie data to data.table
setDT(data)
setDT(final_results)

# Ensure lon and lat columns in both datasets have the same type (num or int) for proper matching
final_results[, `:=`(UID = as.numeric(UID))]
data[, `:=`(UID = as.numeric(UID))]

# Merge the tables on 'date' and 'UID', keeping all rows in 'data' (left join)
merged_data <- merge(
  data,
  final_results[, .(date, UID, precip_value, accum_precip_21d)], 
  by = c("date", "UID"),
  all.x = TRUE
)

# Check
head(merged_data)

# Rename
setnames(merged_data, "precip_value", "precip")

# Clean env.
rm(final_results)
gc()

# Save to .csv file ------------------------------------------------------------
write_fst(
  merged_data, 
  paste0(dir_output, "precipitation/Farauti_ss_North_QLD_with_NCI_precip_1995-1997.fst")
)

write.csv(
  merged_data, 
  paste0(dir_output, "precipitation/Farauti_ss_North_QLD_with_NCI_precip_1995-1997.csv"),
  row.names = FALSE
)

