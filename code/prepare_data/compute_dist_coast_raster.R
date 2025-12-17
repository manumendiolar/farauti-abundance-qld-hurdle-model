#
# DISTANCE TO COASTLINE RASTER
#


# Libraries
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Data: Australia polygon and Natural Earth coastline (lines)
aus_sf   <- rnaturalearth::ne_countries(scale = "large", country = "Australia", returnclass = "sf")
coast_ln <- rnaturalearth::ne_download(scale = "large", type = "coastline",
                                       category = "physical", returnclass = "sf")

# 2) Project both to a national CRS (meters)
crs_au <- "EPSG:3577"   # GDA94 / Australian Albers
aus_sf   <- st_transform(aus_sf, crs_au)
coast_ln <- st_transform(coast_ln, crs_au)

# Clip coastline to Australia’s bbox (speeds up)
coast_ln <- st_intersection(coast_ln, st_as_sfc(st_bbox(aus_sf)))

# 3) Raster template (set your target resolution in meters)
res_m <- 5000  # 5 km
r_tmpl <- rast(ext(vect(aus_sf)), resolution = res_m, crs = crs_au)

# 4) Distance-to-coast (meters) from cell centers to nearest coastline line
#    terra::distance will compute distances on the raster grid to the vector geometry
d_coast_m <- distance(r_tmpl, vect(coast_ln))

# 5) Keep only land (mask with Australia polygon) and convert to kilometers
d_coast_m <- mask(d_coast_m, vect(aus_sf))
d_coast_km <- d_coast_m / 1000
names(d_coast_km) <- "dist_coast_km"

# 6) (Optional) Reproject to WGS84 for sharing; keep EPSG:3577 for analysis
d_coast_km_ll <- project(d_coast_km, "EPSG:4326", method = "bilinear")

# 7) Quick plots
plot(d_coast_km, main = "Distance to Coastline (km) — EPSG:3577")
plot(d_coast_km_ll, main = "Distance to Coastline (km) — WGS84")  # optional

# 8) Save GeoTIFFs
writeRaster(d_coast_km,   "distance_coast_km_3577.tif", overwrite = TRUE)
writeRaster(d_coast_km_ll,"distance_coast_km_wgs84.tif", overwrite = TRUE)


# Align to match other rasters
library(terra)

# path to your template (the WorldClim bio1 you uploaded)
ref_raster <- rast("C:/Users/men118/OneDrive - CSIRO/Documents/GitHub/farab/data/bio1.asc")   # CRS will be EPSG:4326, 0.008333° (≈1 km) or 0.0167°/0.05°, etc.
ref_raster


# Helper: align any raster to the template
align_to_template <- function(x, template, method = "bilinear") {
  # Continuous rasters: method='bilinear' (probabilities, distances, climate)
  # Categorical rasters: method='near'
  if (!same.crs(x, template)) {
    x <- project(x, template, method = method)   # reproject + put on template grid
  } else {
    x <- resample(x, template, method = method)  # just resample to template grid
  }
  x <- mask(x, template)                         # exact footprint & NAs as template
  x
}

# Apply to your rasters (distance-to-coast and model predictions)

# Examples (all are continuous -> bilinear)
d_coast_km_aligned <- align_to_template(d_coast_km, ref_raster, method = "bilinear")   # if in EPSG:3577
d_coast_km_ll_aligned <- align_to_template(d_coast_km_ll, ref_raster, method = "bilinear")# if already WGS84


# Save as .asc files
names(d_coast_km_aligned) <- "dist_coast"
terra::writeRaster(
  d_coast_km_aligned, 
  "C:/Users/men118/OneDrive - CSIRO/Documents/GitHub/farab/data/dist_coast.asc", 
  NAflag = -9999, 
  overwrite = TRUE)

dist_coast.asc
