# ==============================================================================
# MODELLING * Anopheles farauti * ABUNDANCE 
# HURDLE APPROACH: 1st component
#
# MODELLING DISTRIBUTION
# Fitting, Diagnostics, Cross-Validation & Prediction
#
# This script fits various statistical and machine learning models to estimate 
# distribution of An. farauti (binary model)
#
# Manuela, M.
# 24-12-2025
# ==============================================================================


# Setup / Auxiliary functions --------------------------------------------------
source(here::here("code","00_setup.R"))

# Threshold to convert predicitons from continuous to binary
tau <- 0.25


# ------------------------------------------------------------------------------
# ENVIRONMENTAL RASTERS
# ------------------------------------------------------------------------------
asc_files  <- list.files(dir_data, pattern = "\\.asc$", full.names = TRUE)
env_stack  <- stack(asc_files)
names(env_stack) <- tools::file_path_sans_ext(basename(asc_files))

# Predictors based on literature & correlation analysis
wanted_rasters <- c(
  "diurntemp", "seasontemp", "maxtemp", "mintemp", 
  "annrangetemp", "annprecip", "precipwetm", "precipdrym",
  "seasonprecip", "rh3ann", "dp3ann", "mangroves", "trees",
  "elev", "dist_coast", "water_occ", "water_occ_99"
)

env_stack <- subset(env_stack, wanted_rasters)

# Visual check
#plot(env_stack[[1:9]])


# ------------------------------------------------------------------------------
# PRESENCE/ABSENCE DATA
# ------------------------------------------------------------------------------
pa <- read.csv(file.path(dir_data, "ab.csv"))  |> 
  filter(source %in% c("nigel","andrew")) |>  
  filter(region == "QLD") |> 
  dplyr::select(lon, lat, presence) |>
  group_by(lon, lat) |>
  slice_max(presence, with_ties = FALSE) |> 
  ungroup()

# Basic checks
# glimpse(pa)
# head(pa)
# summary(pa)

# Extract environmental values at these locations
pa <- pa[ ,c("lon", "lat", "presence")] |> 
  dplyr::bind_cols(
    as.data.frame(raster::extract(env_stack, pa[, c("lon","lat")]))
  ) |> 
  stats::na.omit()

# Explore value ranges
# summary(pa)


# ------------------------------------------------------------------------------
# ADD NEW PRESENCE POINTS (Bowen & Mackay)
# ------------------------------------------------------------------------------
new_pt1 <- data.frame( lon = c(148.1430), lat = c(-19.9540), presence = c(1) )  # Bowen 
new_pt2 <- data.frame( lon = c(149.1190), lat = c(-21.1040), presence = c(1) )  # Mackay (van den Hurk et al. 1998)
new_pt3 <- data.frame( lon = c(145.6972), lat = c(-16.8221), presence = c(1) )  # Omitted (van den Straat et al. 2019) - outside 1979-2000 period

new_points <- rbind(new_pt1, new_pt2) 

# Extract environmental values at these new locations
new_points <- new_points |> 
  dplyr::bind_cols(
    as.data.frame(raster::extract(env_stack, new_points[, c("lon","lat")]))
  ) |> 
  stats::na.omit()

# Check
# new_points

# Explore value ranges
# summary(new_points)

# NOTE: the difference in, for example, annual range temperature between pa (8.8 - 26.08) and new_points (17.48 - 19.02).


# ------------------------------------------------------------------------------
#                    DATA FOR MODELLING SPECIES DISTRIBUTION
#                        BINARY COMPONENT OF HURDLE MODEL
# ------------------------------------------------------------------------------
data <- rbind(pa, new_points) |> 
  as.data.frame()

# Formatting: near-zero numeric to 0
zero_threshold <- 1e-6
data <- data |> 
  dplyr::mutate(
    dplyr::across(
      where(is.numeric), ~ ifelse(abs(.) < zero_threshold, 0, .)
    )
  )

# MaxEnt formatting 
predictors_maxent <- env_stack
occ_data_maxent <- data |> dplyr::filter(presence == 1) |> dplyr::select(lon, lat)
bg_pts <- as.matrix(read.csv(file.path(dir_data, "bg_points.csv"))) # background points 
env <- raster::brick(env_stack) # for dismo::maxent internals (kept for compatibility)



# ------------------------------------------------------------------------------
#                        FITTING MODELS TO FULL-DATA 
#                        
# ------------------------------------------------------------------------------
set.seed(42)


# RANDOM FOREST 
RF <- randomForest::randomForest(
  as.factor(presence) ~ ., # uses all predictor columns after 'presence', 'lon' and 'lat' 
  data        = data[ ,c("presence", names(env_stack))],
  ntree       = 500,
  importance  = TRUE,
  keep.forest = TRUE
)


# BOOSTED REGRESSION TREES
BRT <- gbm.step(
  data            = data,
  gbm.x           = which(names(data) %in% names(env_stack)),
  gbm.y           = which(names(data) == "presence"),
  family          = "bernoulli",
  tree.complexity = 4,
  learning.rate   = 0.01,
  bag.fraction    = 0.5,
  step.size       = 10,
  plot.main       = FALSE, 
  plot.folds      = FALSE,
  verbose         = FALSE,
  silent          = TRUE
)


# MaxEnt (best configuration chosen previously)
# MAX <- maxent(x = predictors_maxent, p = occ_data_maxent, args = "defaultprevalence=0.1") # Simplified version
# After running a bunch of combinations we chose the best one based on lowest delta.AIC.
# We can supply background points explicitly (these are bias‐corrected points generated earlier)
# We set up the features types corresponding to the "best" maxent model
# results: fc.LQ_rm.1
# NOTE: requires Java / maxent.jar. Fail with a clear message if unavailable.
MAX <- dismo::maxent(
    x = predictors_maxent,
    p = occ_data_maxent,
    a = bg_pts,
    path = file.path(dir_pred, "MaxEnt"),
    args = c(
      "linear=TRUE",
      "quadratic=TRUE",
      "hinge=FALSE",       # disable hinge
      "product=FALSE",     # disable product
      "threshold=FALSE",   # disable threshold (otherwise we could try this one "defaultprevalence=0.1"?)
      "betamultiplier=1",  # tweak regularization multiplier: stronger smoothing
      "randomseed=TRUE"    # reproducible run
      )
)


# GENERALISED LINEAR MODEL (with stepAIC)
GLM <- MASS::stepAIC(
    glm(
      as.integer(presence) ~ . + I(dist_coast^2),   # uses all predictors and the squared term of distance to the coast  
      data   = data[ , c("presence", names(env_stack))],
      family = binomial
      ),
    trace = FALSE
)


# GENERALISED ADDITIVE MODEL (shrinkage via select=TRUE)
pred_vars    <- setdiff(names(data[, c("presence", names(env_stack))]), "presence")
smooth_terms <- paste0("s(", pred_vars, ", k=5)", collapse = " + ")
gam_formula  <- as.formula(paste0("as.integer(", "presence", ") ~ ", smooth_terms))

GAM <- mgcv::gam(
  formula = gam_formula,
  data    = data[, c("presence", names(env_stack))],
  family  = binomial(),
  method  = "GCV.Cp",
  select  = TRUE
)
# By passing select = TRUE to mgcv::gam() we turn on a built-in "shrinkage" or 
# term-selection penalty. 
# Under the hood: extra penalty on each smooth's null space
# With select=TRUE, mgcv augments the usual smoothness penalty with an additional 
# penalty that can drive an entire smooth term's effective degrees of freedom 
# down to (almost) zero if it doesn't improve the model fit.
# No stepwise dropping, but automatic shrinkage. It isn't doing a classic 
# forward/backward stepwise search; instead it's a one-step penalized optim. 
# (via method="GCV.Cp") that simultaneously estimates smoothing parameters and
# applies enough penalty to non-informative terms that they vanish.
# We can look at 
# summary(GAM)
# check the estimated edf (estimated degrees of freedom) for each s(...) term.
# Terms whose edf are estimated very close to zero have effectively been "deselected"
# NOTE:
# Switch to bam() and use its defaults (discrete + fREML + threads) for a huge speed‐up
# GAM <- mgcv::bam(
#   formula = gam_formula,
#   data = data[, c("presence", names(env_stack))],
#   family = binomial,
#   select = TRUE,
#   method = "fREML",                   # faster, more stable smoothing‐parameter estimation
#   discrete = TRUE,                    # build discrete basis to speed up computations
#   nthreads = parallel::detectCores()  # use all available cores
# )




# ------------------------------------------------------------------------------
#                     PREDICTIONS & PERFORMANCE METRICS
#                                 FULL-DATA
# ------------------------------------------------------------------------------
preds_RF  <- predict(RF,  type = "prob")[,2]
preds_BRT <- predict(BRT, n.trees = BRT$gbm.call$best.trees, type = "response")
preds_GLM <- as.numeric(predict(GLM, type = "response"))
preds_GAM <- as.numeric(predict(GAM, type = "response"))

# For MaxEnt
rst <- raster::calc( predict(MAX, env_stack, args="outputformat=logistic"), mean )
preds_MAX <- as.numeric(raster::extract(rst, data[, c("lon","lat")]))

# Combine all in one list
preds_full <- list(
  RF = preds_RF,
  BRT = preds_BRT, 
  MAX = preds_MAX, 
  GLM = preds_GLM, 
  GAM = preds_GAM
)

# Compute full-data metrics
# Note: we could also use presence.absence.accuracy() function 
# to compute a few metrics instead of our extract_metrics() 
metrics_full <- bind_rows(
  lapply(names(preds_full), function(m) {
    df <- extract_metrics(data$presence, preds_full[[m]], thr = tau)
    df$Model  <- m
    df$Method <- "Full"
    df
  })
)

# Ensemble weights (AUC-gain + normalised) -------------------------------------
# Since an AUC of 0.5 is "random-chance" performance, subtracting 0.5 turns each
# raw AUC into its improvement over random. 
# For example:
# If AUC = 0.85, then weight = 0.85 - 0.5 = 0.35 (35% better than random)
# If AUC = 0.55, then weight = 0.05 (barely better than random)
# If AUC = 0.50, then weight = 0.00 (same as random)
weights <- metrics_full$AUC - 0.5                # gain vector 
weights <- setNames(weights, metrics_full$Model) # assign names
weights_t <- weights / sum(weights)         # normalizing to sum to one

# Ensemble prediction (BRT + MAX + GLM) 
preds_ENS <- preds_full[["BRT"]] * weights_t[["BRT"]] +
  preds_full[["MAX"]] * weights_t[["MAX"]] + 
  preds_full[["GLM"]] * weights_t[["GLM"]] 

# Update
preds_full[["ENS"]] <- as.numeric(preds_ENS)

# Performance metrics on full-data including Ensemble
metrics_full <- bind_rows(
  lapply(names(preds_full), function(m) {
    df <- extract_metrics(data$presence, preds_full[[m]], thr = tau)
    df$Model  <- m
    df$Method <- "Full"
    df
  })
)

# Formatting
rownames(metrics_full) <- NULL
metrics_full <- metrics_full[, c("Model", "AUC", "Accuracy", "Sensitivity", "Specificity", "BA", "Kappa", "TSS", "Precision", "F1", "MCC")]

# Check
metrics_full

# Save
write.csv(
  metrics_full,
  file.path(dir_tables, paste0("binary_model_full_", tau, ".csv")),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
#                           RASTER PREDICTIONS
#                               FULL-DATA
# ------------------------------------------------------------------------------

# Convert env_stack to terra raster
env_stack_terra <- terra::rast(env_stack) 

# Australia mainland shapefile for masking
aus      <- terra::vect(file.path(dir_data, "maps", "gadm41_AUS_0.shp"))
mainland <- aus[aus$COUNTRY == "Australia", ]

# Model rasters
rf_raster  <- terra::predict(env_stack_terra, RF, type = "prob", index = 2)
brt_raster <- terra::predict(env_stack_terra, BRT, type = "response", n.trees = BRT$gbm.call$best.trees)
glm_raster <- terra::predict(env_stack_terra, GLM, type = "response")
gam_raster <- terra::predict(env_stack_terra, GAM, type = "response")
max_raster <- rast(predict(MAX, env_stack, args = "outputformat=logistic"))
ens_raster <- brt_raster * weights_t[["BRT"]] +
  max_raster * weights_t[["MAX"]] +
  glm_raster * weights_t[["GLM"]]

# Mask to mainland ! 
rf_raster <- terra::mask(rf_raster, mainland)
brt_raster <- terra::mask(brt_raster, mainland)
glm_raster <- terra::mask(glm_raster, mainland)
gam_raster <- terra::mask(gam_raster, mainland)
max_raster <- terra::mask(max_raster, mainland)
ens_raster <- terra::mask(ens_raster, mainland)

# IDEA
# Using five levels of suitability: 
# (0.0‒0.2)  unsuitability 
# (0.2‒0.4)  low suitability 
# (0.4‒0.6)  medium suitability 
# (0.6‒0.8)  suitability 
# (0.8‒1.0)  high suitability 

# Define colour palette for heatmap style (blue to red through yellow)
heat_diverging <- c(
  "#053061", "#2166AC", "#4393C3", "#92C5DE",
  "#f7f786ff", "#FFFF8C",  "#FFE75E", "#FCD070",
   "#D6604D", "#B2182B", "#67001F"
  )

# Quick visual check (optional)
# plot(rf_raster,  col = heat_diverging, main = "RF")
# plot(brt_raster, col = heat_diverging, main = "BRT")
# plot(max_raster, col = heat_diverging, main = "MaxEnt")
# plot(glm_raster, col = heat_diverging, main = "GLM")
# plot(gam_raster, col = heat_diverging, main = "GAM")
# plot(ens_raster, col = heat_diverging, main = "ENS")

# Save raster predictions 
terra::writeRaster(rf_raster, file.path(dir_pred,  "rf_prediction.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(brt_raster, file.path(dir_pred, "brt_prediction.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(max_raster, file.path(dir_pred, "max_prediction.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(glm_raster, file.path(dir_pred, "glm_prediction.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(gam_raster, file.path(dir_pred, "gam_prediction.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(ens_raster, file.path(dir_pred, "ens_prediction.asc"), NAflag = -9999, overwrite = TRUE)



# ------------------------------------------------------------------------------
#                        REPEATED K-FOLD + UNCERTAINTY 
#                            (fold-level rasters) 
# ------------------------------------------------------------------------------
set.seed(1234)

# Settings
K                    <- 10     # folds
reps                 <- 5      # repetitions
stratify_by_presence <- TRUE   # set FALSE for unstratified folds
n          <- nrow(data)       # number of observations
pred_vars  <- names(env_stack) # name of predictors
response   <- "presence"       # name of response
glm_formula <- formula(GLM)    
# gam_formula already defined above

# Pre-allocation for metrics and OOF predictions
all_repeat_metrics <- vector("list", reps)
oof_RF  <- matrix(0, nrow = n, ncol = reps)
oof_BRT <- matrix(0, nrow = n, ncol = reps)
oof_GLM <- matrix(0, nrow = n, ncol = reps)
oof_GAM <- matrix(0, nrow = n, ncol = reps)
oof_MAX <- matrix(0, nrow = n, ncol = reps) 
oof_ENS <- matrix(0, nrow = n, ncol = reps)

# Directory + helper to save fold-level rasters 
pred_dir <- file.path(dir_pred, "cv_preds")
dir.create(pred_dir, recursive = TRUE, showWarnings = FALSE)

cv_paths <- list(
  RF  = character(),
  BRT = character(),
  GLM = character(),
  GAM = character(),
  MAX = character(),
  ENS = character()
)

# Main CV loop (repeats x folds) -----------------------------------------------
for (rep_i in seq_len(reps)) {
  cat("Rep: ", rep_i, "\n")
  set.seed(1000 + rep_i)
  
  folds <- make_folds_once_binary_models(
    y = data[[response]], 
    k = K, 
    stratified = stratify_by_presence
  )
  
  pr_RF  <- rep(0, n)
  pr_BRT <- rep(0, n)
  pr_GLM <- rep(0, n)
  pr_GAM <- rep(0, n)
  pr_MAX <- rep(0, n)
  
  for (fold_idx in seq_along(folds)) {
    cat("  fold: ", fold_idx,"\n")

    # Train/test indices
    test_idx  <- folds[[fold_idx]]
    train_idx <- setdiff(seq_len(n), test_idx)
    
    # Train/test data
    df_train  <- data[train_idx, ]
    df_test   <- data[test_idx, , drop = FALSE]
    

    # RANDOM FOREST
    RF_k <- randomForest::randomForest(
      as.factor(presence) ~ .,
      data        = df_train[, c("presence", pred_vars)],
      ntree       = 500,
      importance  = TRUE,
      keep.forest = TRUE
    )
    pr_RF[test_idx] <- predict(RF_k, df_test, type = "prob")[,2]
    

    # BOOSTED REGRESSION TREES
    BRT_k <- gbm.step(
      data            = df_train,
      gbm.x           = which(names(df_train) %in% pred_vars),
      gbm.y           = which(names(df_train) == response),
      family          = "bernoulli",
      tree.complexity = 4, 
      learning.rate   = 0.01,
      bag.fraction    = 0.5, 
      step.size       = 10,
      plot.main       = FALSE, 
      plot.folds      = FALSE,
      verbose         = FALSE, 
      silent          = TRUE
    )
    pr_BRT[test_idx] <- predict(BRT_k, df_test, n.trees = BRT_k$gbm.call$best.trees, type = "response")
    

    # GENERALISED LINEAR MODELS
    GLM_k <- glm(glm_formula, data = df_train, family = binomial)
    pr_GLM[test_idx] <- predict(GLM_k, df_test, type = "response")
     

    # GENERALISED ADDITIVE MODELS
    GAM_k <- mgcv::gam(
      formula = gam_formula,
      data    = df_train,
      family  = binomial,
      select  = TRUE,
      method  = "GCV.Cp"
    )
    pr_GAM[test_idx] <- predict(GAM_k, df_test, type = "response")
    

    # MaxEnt (predict on data.frame, not matrix)
    MAX_k <- dismo::maxent(
      x    = predictors_maxent,
      p    = as.matrix(df_train[df_train$presence == 1, c("lon","lat")]),
      a    = bg_pts,
      args = c(
        "linear=TRUE",
        "quadratic=TRUE",
        "hinge=FALSE",
        "product=FALSE",
        "threshold=FALSE",
        "betamultiplier=1",
        "randomseed=TRUE"
      )
    )
    pr_MAX[test_idx] <- predict(MAX_k, df_test[, pred_vars, drop = FALSE], args = "outputformat=logistic")

    
    # Full-raster predictions for this fold
    # RF
    rf_r_k  <- terra::predict(env_stack_terra, RF_k, type = "prob", index = 2)
    brt_r_k <- terra::predict(env_stack_terra, BRT_k, type = "response", n.trees = BRT_k$gbm.call$best.trees)
    glm_r_k <- terra::predict(env_stack_terra, GLM_k, type = "response")
    gam_r_k <- terra::predict(env_stack_terra, GAM_k, type = "response")
    max_r_k <- terra::rast(predict(MAX_k, env_stack, args = "outputformat=logistic"))
    
    # Mask to mainland
    rf_r_k  <- terra::mask(rf_r_k,  mainland)
    brt_r_k <- terra::mask(brt_r_k, mainland)
    glm_r_k <- terra::mask(glm_r_k, mainland)
    gam_r_k <- terra::mask(gam_r_k, mainland)
    max_r_k <- terra::mask(max_r_k, mainland)
    
    # Ensemble raster for this fold
    ens_r_k <- brt_r_k * weights_t[["BRT"]] + max_r_k * weights_t[["MAX"]] + glm_r_k * weights_t[["GLM"]]
    
    # Save paths for SD/uncertainty computation later
    cv_paths$RF  <- c(cv_paths$RF,  save_cv_rast(rf_r_k,  "RF",  rep_i, fold_idx))
    cv_paths$BRT <- c(cv_paths$BRT, save_cv_rast(brt_r_k, "BRT", rep_i, fold_idx))
    cv_paths$GLM <- c(cv_paths$GLM, save_cv_rast(glm_r_k, "GLM", rep_i, fold_idx))
    cv_paths$GAM <- c(cv_paths$GAM, save_cv_rast(gam_r_k, "GAM", rep_i, fold_idx))
    cv_paths$MAX <- c(cv_paths$MAX, save_cv_rast(max_r_k, "MAX", rep_i, fold_idx))
    cv_paths$ENS <- c(cv_paths$ENS, save_cv_rast(ens_r_k, "ENS", rep_i, fold_idx))
    

  } # end folds loop
  
  # Avoid near-zero values 
  eps <- 1e-6
  pr_RF  <- replace(pr_RF,  pr_RF  < eps, 0)
  pr_BRT <- replace(pr_BRT, pr_BRT < eps, 0)
  pr_GLM <- replace(pr_GLM, pr_GLM < eps, 0)
  pr_GAM <- replace(pr_GAM, pr_GAM < eps, 0)
  pr_MAX <- replace(pr_MAX, pr_MAX < eps, 0)

  # Ensemble OOF using full-data weights 
  pr_ENS <- weights_t["BRT"] * pr_BRT + weights_t["MAX"] * pr_MAX + weights_t["GLM"] * pr_GLM
  
  oof_RF[,  rep_i] <- pr_RF
  oof_BRT[, rep_i] <- pr_BRT
  oof_GLM[, rep_i] <- pr_GLM
  oof_GAM[, rep_i] <- pr_GAM
  oof_MAX[, rep_i] <- pr_MAX
  oof_ENS[, rep_i] <- pr_ENS
  
  # Store metrics for this repeat
  pred_list_rep <- list(
    RF = pr_RF, 
    BRT = pr_BRT, 
    MAX = pr_MAX, 
    GLM = pr_GLM, 
    GAM = pr_GAM, 
    ENS = pr_ENS
  )
  metrics_rep <- purrr::imap_dfr(
    pred_list_rep,
    ~{
      m <- extract_metrics(data[[response]], .x, thr = tau)
      m$Model  <- .y
      m$Method <- sprintf("Kfold_%dx (rep %d)", K, rep_i)
      m
    }
  )

  all_repeat_metrics[[rep_i]] <- metrics_rep
}

message("✅ Repeated K-fold complete.")


# ------------------------------------------------------------------------------
# UNCERTAINTY MAPS FROM SAVED CV RASTERS (re-load without re-running CV)
# ------------------------------------------------------------------------------
cv_dir <- file.path(dir_pred, "cv_preds")
cv_paths <- list(
  RF = list.files(cv_dir, pattern = "^RF_.*\\.tif$", full.names = TRUE),
  BRT = list.files(cv_dir, pattern = "^BRT_.*\\.tif$", full.names = TRUE),
  MAX = list.files(cv_dir, pattern = "^MAX_.*\\.tif$", full.names = TRUE),
  GLM = list.files(cv_dir, pattern = "^GLM_.*\\.tif$", full.names = TRUE),
  GAM = list.files(cv_dir, pattern = "^GAM_.*\\.tif$", full.names = TRUE),
  ENS = list.files(cv_dir, pattern = "^ENS_.*\\.tif$", full.names = TRUE)  
)
# Example for RF (repeat block kept as-is, just using cv_dir from project)
rf_stack <- terra::rast(cv_paths$RF)
rf_mean  <- terra::app(rf_stack, fun = "mean", na.rm = TRUE)
rf_sd    <- terra::app(rf_stack, fun = "sd",   na.rm = TRUE)
rf_cv    <- terra::app(rf_stack, fun = "cv",   na.rm = TRUE)
terra::writeRaster(rf_mean, file.path(dir_pred, "rf_cv_mean.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(rf_sd,   file.path(dir_pred, "rf_cv_sd.asc"),   NAflag = -9999, overwrite = TRUE)
terra::writeRaster(rf_cv,   file.path(dir_pred, "rf_cv_cv.asc"),   NAflag = -9999, overwrite = TRUE)
#
# BRT
brt_stack <- terra::rast(cv_paths$BRT)
brt_mean  <- terra::app(brt_stack, fun = "mean", na.rm = TRUE)
brt_sd    <- terra::app(brt_stack, fun = "sd", na.rm = TRUE)
brt_cv    <- terra::app(brt_stack, fun = "cv", na.rm = TRUE)
terra::writeRaster(brt_mean, file.path(dir_pred, "brt_cv_mean.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(brt_sd, file.path(dir_pred, "brt_cv_sd.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(brt_cv, file.path(dir_pred, "brt_cv_cv.asc"), NAflag = -9999, overwrite = TRUE)
# MaxEnt
max_stack <- terra::rast(cv_paths$MAX)
max_mean  <- terra::app(max_stack, fun = "mean", na.rm = TRUE)
max_sd    <- terra::app(max_stack, fun = "sd", na.rm = TRUE)
max_cv    <- terra::app(max_stack, fun = "cv", na.rm = TRUE)
terra::writeRaster(max_mean, file.path(dir_pred, "max_cv_mean.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(max_sd, file.path(dir_pred, "max_cv_sd.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(max_cv, file.path(dir_pred, "max_cv_cv.asc"), NAflag = -9999, overwrite = TRUE)
# GLM
glm_stack <- terra::rast(cv_paths$GLM)
glm_mean  <- terra::app(glm_stack, fun = "mean", na.rm = TRUE)
glm_sd    <- terra::app(glm_stack, fun = "sd", na.rm = TRUE)
glm_cv    <- terra::app(glm_stack, fun = "cv", na.rm = TRUE)
terra::writeRaster(glm_mean, file.path(dir_pred, "glm_cv_mean.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(glm_sd, file.path(dir_pred, "glm_cv_sd.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(glm_cv, file.path(dir_pred, "glm_cv_cv.asc"), NAflag = -9999, overwrite = TRUE)
# GAM
gam_stack <- terra::rast(cv_paths$GAM)
gam_mean  <- terra::app(gam_stack, fun = "mean", na.rm = TRUE)
gam_sd    <- terra::app(gam_stack, fun = "sd", na.rm = TRUE)
gam_cv    <- terra::app(gam_stack, fun = "cv", na.rm = TRUE)
terra::writeRaster(gam_mean, file.path(dir_pred, "gam_cv_mean.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(gam_sd, file.path(dir_pred, "gam_cv_sd.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(gam_cv, file.path(dir_pred, "gam_cv_cv.asc"), NAflag = -9999, overwrite = TRUE)
# Ensemble
ens_stack <- terra::rast(cv_paths$ENS)
ens_mean  <- terra::app(ens_stack, fun = "mean", na.rm = TRUE)
ens_sd    <- terra::app(ens_stack, fun = "sd", na.rm = TRUE)
ens_cv    <- terra::app(ens_stack, fun = "cv", na.rm = TRUE)
terra::writeRaster(ens_mean, file.path(dir_pred, "ens_cv_mean.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(ens_sd, file.path(dir_pred, "ens_cv_sd.asc"), NAflag = -9999, overwrite = TRUE)
terra::writeRaster(ens_cv, file.path(dir_pred, "ens_cv_cv.asc"), NAflag = -9999, overwrite = TRUE)

# Optional quick visual check
par(
  mfrow = c(1, 3),            # 1 row, 3 columns
  mar   = c(1, 1, 1, 1),      # inner margins: bottom, left, top, right
  oma   = c(0, 0, 0, 0)       # outer margins
)
plot(rf_mean,  col = heat_diverging, main = "RF (Mean)")
plot(rf_cv,  col = heat_diverging, main = "RF (CV)")
plot(rf_sd,  col = heat_diverging, main = "RF (SD)")
par(mfrow = c(1, 1))


# ------------------------------------------------------------------------------
# Aggregate K-fold metrics across reps
# ------------------------------------------------------------------------------
metrics_kfold_all <- dplyr::bind_rows(all_repeat_metrics)

metrics_kfold_summary <- metrics_kfold_all |> 
  dplyr::group_by(Model) |> 
  dplyr::summarise(
    AUC_mean = mean(AUC, na.rm = TRUE), 
    AUC_sd = sd(AUC, na.rm = TRUE),
    Accuracy_mean = mean(Accuracy, na.rm = TRUE), 
    Accuracy_sd = sd(Accuracy, na.rm = TRUE),
    Sensitivity_mean = mean(Sensitivity, na.rm = TRUE), 
    Sensitivity_sd = sd(Sensitivity, na.rm = TRUE),
    Specificity_mean = mean(Specificity, na.rm = TRUE), 
    Specificity_sd = sd(Specificity, na.rm = TRUE),
    BA_mean  = mean(BA,  na.rm = TRUE), 
    BA_sd  = sd(BA,  na.rm = TRUE),
    Kappa_mean = mean(Kappa, na.rm = TRUE), 
    Kappa_sd = sd(Kappa, na.rm = TRUE),
    TSS_mean = mean(TSS, na.rm = TRUE),
     TSS_sd = sd(TSS, na.rm = TRUE),
    Precision_mean = mean(Precision, na.rm = TRUE), 
    Precision_sd = sd(Precision, na.rm = TRUE),
    F1_mean = mean(F1, na.rm = TRUE), 
    F1_sd = sd(F1, na.rm = TRUE),
    MCC_mean = mean(MCC, na.rm = TRUE), 
    MCC_sd = sd(MCC, na.rm = TRUE),
    .groups  = "drop"
  )

# Check
#metrics_kfold_summary 

# Save CSV 
write.csv(
  metrics_kfold_summary , 
  file.path(dir_tables, sprintf("binary_model_kfold_summary_%dx%dr_%.2f.csv", K, reps, tau)), 
  row.names = FALSE
)

# Simple mean table to export (means across reps)
DIGITS <- 2
all_metric_names <- c("AUC", "Accuracy", "Sensitivity", "Specificity", "BA", "Kappa", "TSS", "Precision", "F1", "MCC")
pretty_model <- function(x) dplyr::recode(x, MAX = "MaxEnt", .default = x)

kfold_means_all <- metrics_kfold_all |> 
  dplyr::group_by(Model) |> 
  dplyr::summarise(
    dplyr::across(dplyr::all_of(all_metric_names), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |> 
  dplyr::mutate(Model = pretty_model(Model)) |> 
  dplyr::arrange(factor(Model, levels = c("RF","BRT","MaxEnt","GLM","GAM","ENS"))) |> 
  dplyr::mutate(dplyr::across(dplyr::all_of(all_metric_names), ~ round(.x, DIGITS)))

# Check
#kfold_means_all

# Save CSV 
write.csv(
  kfold_means_all, 
  file.path(dir_tables, sprintf("binary_model_kfold_%dx%dr_%.2f.csv", K, reps, tau)), 
  row.names = FALSE
)
