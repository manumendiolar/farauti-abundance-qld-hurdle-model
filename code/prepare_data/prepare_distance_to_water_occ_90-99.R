library(terra)

# Reference raster (for alignment)
ref_raster <- rast("current/elev.asc")

# Load water occurrence raster
water_occ <- rast("current/water_occurrence.asc")

# Convert to points once (filter NA first)
water_pts <- as.points(water_occ, na.rm = TRUE)

# Define thresholds
water_occ_thresholds <- 90:99

# Output directory
dir.create("data/distances", showWarnings = FALSE, recursive = TRUE)

for (thres in water_occ_thresholds) {
  # 1. Filter permanent water points above threshold
  perm_water_pts <- water_pts[water_pts$water_occurrence > thres, ]
  
  # 2. Skip if no points at this threshold
  if (nrow(perm_water_pts) == 0) {
    warning(paste("No points found for threshold >", thres))
    next
  }
  
  # 3. Distance raster
  dist_raw <- distance(water_occ, perm_water_pts)
  dist_km <- dist_raw / 1000
  
  # 4. Align to reference raster (same extent, resolution, CRS)
  dist_km_aligned <- project(dist_km, ref_raster, method = "bilinear")
  
  # 5. Plot
  png_path <- paste0("data/distances/dist_to_water_occurrence_", thres, ".png")
  png(png_path, width = 1600, height = 1200, res = 200)
  plot(dist_km_aligned,
       main = paste0("Distance to nearest permanent water (occurrence >", thres, ") [km]"),
       cex.main = 0.9)
  dev.off()
  
  # 6. Save raster
  asc_path <- paste0("data/distances/dist_to_water_occurrence_", thres, ".asc")
  writeRaster(dist_km_aligned, asc_path, overwrite = TRUE, NAflag = -9999)
}

