# ------------------------------------------------------------------------------
# This code generates predictions of trap counts within each 5 km grid cell, 
# based on a hurdle model combining a 'distribution model' for pres-abs component
# and an 'abundance model' for positive counts component. Grid cells are highlighted 
# where predicted presence exceeds a threshold (e.g., 0.00), with warmer
# colours indicating higher relative abundance. 
# ------------------------------------------------------------------------------

# FIRST RUN:
# 02_binary_models.R
# 03_count_models.R
# 04_zi_models.R


# Auxiliary functions / setup --------------------------------------------------
source(file.path("code", "00_setup.R"))


# Coastline
aus <- terra::vect("data/maps/gadm41_AUS_0.shp")
mainland <- aus[aus$COUNTRY == "Australia", ]
mainland_sf <- sf::st_as_sf(mainland) 

# Queensland 5 x 5 km grid info
centroids <- fst::read_fst(file.path(dir_data, "centroids_5x5_qld.fst")) |>  
  dplyr::mutate(
    date   = as.Date(date),
    year   = as.factor(year),
    month  = as.factor(month),
    season = as.factor(season)
  )

# Check
glimpse(centroids)
summary(centroids)

# Check NA values in predictors and impute using mean of k nearest neighbors (use flag_NAs_pi_at_cenotrids.R to visualise)
centroids <- impute_pi_knn(centroids, value_col = "rhm21", k = 8, lon_col = "lon", lat_col = "lat")


# Add predictions form zero-inflated approach binary and count components from all models explored
centroids$zip_abund <- as.numeric(predict(ZIPa, newdata = centroids, type = "response"))
centroids$zinb_abund <- as.numeric(predict(ZINBa, newdata = centroids, type = "response"))


# Add predictions from binary component from all models explored
rf_prediction <- rast(file.path(dir_pred, "rf_prediction.asc"))
brt_prediction <- rast(file.path(dir_pred, "brt_prediction.asc"))
max_prediction <- rast(file.path(dir_pred, "max_prediction.asc"))
glm_prediction <- rast(file.path(dir_pred, "glm_prediction.asc"))
gam_prediction <- rast(file.path(dir_pred, "gam_prediction.asc"))
ens_prediction <- rast(file.path(dir_pred, "ens_prediction.asc"))
pts <- vect(centroids, geom = c("lon","lat"), crs = "EPSG:4326") # centroids spatial formatting
if (!is.na(crs(rf_prediction)) && crs(pts) != crs(rf_prediction)) pts <- project(pts, crs(rf_prediction)) # just one as a reference!
vals <- terra::extract(rf_prediction, pts); centroids$pi_rf <- vals[[names(rf_prediction)]]
vals <- terra::extract(brt_prediction, pts); centroids$pi_brt <- vals[[names(brt_prediction)]]
vals <- terra::extract(max_prediction, pts); centroids$pi_max <- vals[[names(max_prediction)]]
vals <- terra::extract(glm_prediction, pts); centroids$pi_glm <- vals[[names(glm_prediction)]]
vals <- terra::extract(gam_prediction, pts); centroids$pi_gam <- vals[[names(gam_prediction)]]
vals <- terra::extract(ens_prediction, pts); centroids$pi_ens <- vals[[names(ens_prediction)]]
# Check NA values and impute using mean of k nearest neighbors (use flag_NAs_pi_at_cenotrids.R to visualise)
pi_cols <- c("pi_rf", "pi_brt", "pi_max", "pi_glm", "pi_gam", "pi_ens")
centroids <- impute_pi_knn(centroids, value_col = "pi_rf", k = 8, lon_col = "lon", lat_col = "lat")
centroids <- impute_pi_knn(centroids, value_col = "pi_max", k = 8, lon_col = "lon", lat_col = "lat")
centroids <- impute_pi_knn(centroids, value_col = "pi_glm", k = 8, lon_col = "lon", lat_col = "lat")
centroids <- impute_pi_knn(centroids, value_col = "pi_gam", k = 8, lon_col = "lon", lat_col = "lat")
centroids <- impute_pi_knn(centroids, value_col = "pi_ens", k = 8, lon_col = "lon", lat_col = "lat")


# Add predictions from count component from all models explored
centroids$mu_rf <- predict(RFa, newdata = centroids, type = "response")
centroids$mu_brt <- predict(BRTa, newdata = centroids, n.trees = BRTa$gbm.call$best.trees, type = "response")
centroids$mu_glm <- as.numeric(predict(GLMa, newdata = centroids, type = "response"))
centroids$mu_gam <- as.numeric(predict(GAMa, newdata = centroids, type = "response"))
metrics <- read.csv(file.path(dir_tables,"count_model_kfold_10x5r.csv"))
weights <- metrics$Pearson 
weights <- setNames(weights, metrics$Model) 
weights_t <- weights / sum(weights)
centroids$mu_ens <- as.numeric(
  centroids$mu_rf * weights_t[["RF"]] + 
    centroids$mu_brt * weights_t[["BRT"]] + 
    centroids$mu_glm * weights_t[["GLM"]] + 
    centroids$mu_gam * weights_t[["GAM"]] 
)

# Compute Hurdle predictions for different thresholds 
for (thres in seq(0, 1, 0.01)){
  thres_lbl <- gsub("\\.", "", sprintf("%.2f", thres)) # Formatting
  for (binary_model in c("rf", "brt", "max", "glm", "gam", "ens")){
    pi_col <- paste0("pi_", binary_model)                                                            # Name of pi prediction column
    class_col  <- paste0(binary_model, "_class_t", thres_lbl)                                        # Name of presence class column
    centroids[[class_col]] <- as.integer(centroids[[pi_col]] > thres)                                # Compute presence class column 
    for (count_model in c("rf", "brt", "glm", "gam", "ens")){
      mu_col <- paste0("mu_", count_model)                                                           # Name of mu prediction column 
      hurdle_col <- paste0(binary_model,"_",count_model,"_t", thres_lbl, "_abund")                   # Name of hurdle prediction column
      centroids[[hurdle_col]] <- centroids[[class_col]] * centroids[[pi_col]] * centroids[[mu_col]]  # Compute hurdle abundance
    }
  }
}

# Check
head(centroids)

# Save as .fst file
fst::write_fst(centroids, file.path(dir_pred, "centroids_5x5_qld_with_predictions.fst"), compress = 50)



#
# BUILD A SLIM CENTROID FILE FOR SHINY APP 
#
# Columns needed for Shiny:
# - location/time keys: grid_id, lon, lat, date, year, month, season
# - predictions needed to recompute hurdle: pi_* and mu_*
keep_cols <- c(
  "grid_id", "lon", "lat",
  "date", "year", "month", "season",
  "zip_abund", "zinb_abund",
  paste0("pi_", c("rf","brt","max","glm","gam","ens")),
  paste0("mu_", c("rf","brt","glm","gam","ens"))
)

centroids_shiny <- centroids |> 
  # Drop *all* threshold-derived columns if they exist
  dplyr::select(
    -matches("_class_t\\d+$"),
    -matches("_t\\d+_abund$")
  ) |> 
  # Keep only the minimal set (any_of avoids errors if a col is missing)
  dplyr::select(any_of(keep_cols)) |> 
  # Make sure key columns are sensible types for Shiny filtering
  dplyr::mutate(
    date   = as.Date(date),
    year   = as.integer(as.character(year)),
    month  = as.integer(as.character(month)),
    season = as.character(season)
  )

# Quick sanity check
message("Shiny centroids columns: ", ncol(centroids_shiny))
print(names(centroids_shiny))

# Write file (higher compress => smaller file for Zenodo / sharing)
out_path <- file.path(dir_pred, "centroids_5x5_qld_with_predictions_shiny-app.fst")
fst::write_fst(centroids_shiny, out_path, compress = 100)

message("Wrote: ", out_path)
message("Approx file size (MB): ", round(file.info(out_path)$size / 1024^2, 1))