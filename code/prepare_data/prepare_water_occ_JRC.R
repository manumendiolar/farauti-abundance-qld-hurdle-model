# ==============================================================================
# WATER OCCURRENCE 
#
# The output of this script is a .tif raster file that shows where surface water
# occurred between 1984 and 2021 and provides info concerning overall water dynamics. 
# This product comes from JRC GSW (https://global-surface-water.appspot.com/download)
# and captures both the intra and  inter-annual variability and changes.
# 
# Description (from the paper)
# The frequency with which water was present on the surface from March 1984 to 
# December 2021 was captured in a single product called surface water occurrence (SWO). 
# To compute SWO, the water detections (WD) and valid observations (VO) from the 
# same months are summed, that is, water detections and valid observations from 
# March 1984 are added to water detections and valid observations from March 1985
# and so on, such that SWO_month = ∑ WD_month / ∑ VO_month 
# Averaging the results of all monthly SWO_month calculations gives the long-term
# overall surface water occurrence. 
#
# 04-04-2025


# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------
library(terra)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rasterVis)


# ------------------------------------------------------------------------------
# Paths and Setup
# ------------------------------------------------------------------------------
base_dir <- "Z:/work/data/hydro/JRC/"
ref_raster <- rast("current/elevation.asc")  # reference raster at ~5km resolution
output_dir <- "current"
dir.create(output_dir, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Select relevant tiles (for Queensland)
# ------------------------------------------------------------------------------
tile_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
keep_tiles <- c("130E_0N", "140E_0N", "150E_0N",
                "130E_10S", "140E_10S", "150E_10S",
                "130E_20S", "140E_20S", "150E_20S")
tile_dirs <- tile_dirs[basename(tile_dirs) %in% keep_tiles]


# ------------------------------------------------------------------------------
# Load and resample each occurrence raster
# ------------------------------------------------------------------------------
resampled_rasters <- list()

for (d in tile_dirs) {
  tif_files <- list.files(d, pattern = "^occurrence.*\\.tif$", full.names = TRUE)
  
  if (length(tif_files) > 0) {
    cat("Processing tile:", basename(d), "\n")
    r <- try(rast(tif_files[1]), silent = TRUE)
    
    if (!inherits(r, "try-error")) {
      r_resampled <- resample(r, ref_raster, method = "bilinear")
      resampled_rasters[[basename(d)]] <- r_resampled
    } else {
      warning(paste("Failed to read:", tif_files[1]))
    }
  } else {
    warning(paste("No occurrence raster found in", d))
  }
}

# ------------------------------------------------------------------------------
# Mosaic all resampled tiles (now same resolution as elevation)
# ------------------------------------------------------------------------------
mosaic_raster <- do.call(mosaic,unname(resampled_rasters))

# Optional: crop to elevation extent
mosaic_raster <- crop(mosaic_raster, ref_raster)

# Check
plot(mosaic_raster, main = "Water occurrence (%) (255: No data)")

# ------------------------------------------------------------------------------
# Save output
# ------------------------------------------------------------------------------
writeRaster(
  mosaic_raster,
  filename = "data/hydro/JCR/water_occurrence_qld.tif",
  overwrite = TRUE,
  NAflag = -9999
)


# Plot with Symbology ----------------------------------------------------------

#Load the raster
r <- rast("data/hydro/JCR/water_occurrence_qld.tif")

# Define extent for Queensland (bounding box)
lon_min <- 138
lon_max <- 154
lat_min <- -29
lat_max <- -9.0
qld_extent <- ext(lon_min, lon_max, lat_min, lat_max)

# Crop the raster
r_qld <- crop(r, qld_extent)

# Plot
plot(r_qld, main = "Queensland Occurrence: Waterbodies\nFrom https://global-surface-water.appspot.com/download", cex.main= 0.75)

# Re-code values > 100 to 100 (ocean = 100% water)
r_qld[r_qld > 100] <- NA

# Plot
plot(r_qld, main = "Queensland Occurrence: Waterbodies\nFrom https://global-surface-water.appspot.com/download", cex.main= 0.75)

# Save cropped raster
writeRaster(
  r_qld,
  filename = "data/hydro/JCR/water_occurrence_qld_maxent.tif",
  overwrite = TRUE,
  NAflag = -9999
 )

# Quick visual to see if there are values between 100 and 255
hist(r_qld, breaks = 256, main = "Value Distribution")


r_manual <- raster(r_qld)
NAvalue(r_manual) <- -9999

color_vector <- rep(NA, 256)
color_vector[1]   <- "#FFFFFF"
color_vector[2:101] <- colorRampPalette(c("#FFCCCC", "#0000FF"))(100)
color_vector[256] <- "#CCCCCC"

par(mar = c(5, 4, 4, 6))  # increase right margin

# Use base R plot first (you already have this)
plot(r_manual,
     col = color_vector,
     breaks = -0.5:255.5,
     legend = FALSE,
     main = "Queensland Occurrence: Waterbodies\nFrom https://global-surface-water.appspot.com/download",
     cex.main= 1)

# Then add the legend at precise coordinates
legend(x = par("usr")[2] + 0.2,  # just to the right of the plot
       y = par("usr")[4] - 0,      # top of the plot
       legend = c("0% (Not water)", "25%", "50%", "75%", "100%", "No data"),
       fill = c("#FFFFFF", "#FF9999", "#CC6699", "#6633CC", "#0000FF", "#CCCCCC"),
       border = "black",
       bty = "n",
       cex = 0.8,
       pt.cex = 1.5,
       xpd = TRUE)  # allow drawing outside plot area




# HAVEN'T USED THE FOLLOWING
# # Build distance to permanent waterbodies --------------------------------------
# 
# # Step 0: Load the raster (already processed, with NA values set properly)
# r <- terra::rast("data/hydro/JCR/queensland_occurrence_maxent.tif")
# 
# # Step 0.5 (Optional): Aggregate to lower resolution (e.g., factor of 4)
# r <- aggregate(r, fact = 4, fun = mean, na.rm = TRUE) # Speeds up processing a lot, adjust `fact` as needed (try 2, 4, 5)
# 
# # Step 1: Identify permanent water (occurrence ≥ 90)
# perm_water <- r >= 90
# 
# # Step 2: Set NA where data is missing
# perm_water[is.na(r)] <- NA
# 
# # Step 3: Compute distance to nearest permanent water pixel (in meters)
# dist_to_perm <- distance(perm_water)
# 
# # Optional: convert to km
# dist_to_perm_km <- dist_to_perm / 1000
# 
# # Step 4 (Optional): Resample back to original resolution
# dist_to_perm_km <- resample(dist_to_perm_agg_km, r, method = "bilinear")
# 
# # Step 5: Plot
# plot(dist_to_perm_km, main = "Distance to Permanent Water (≥90%)")
# 
# # Step 6: Save for later use
# writeRaster(dist_to_perm_km, "data/hydro/JCR/distance_to_perm_water90_km.tif", overwrite = TRUE, NAflag = -9999)

