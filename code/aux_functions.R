# ------------------------------------------------------------------------------
#                            02_binary_models.R 
#                            auxiliary functions
# ------------------------------------------------------------------------------

extract_metrics <- function(obs, prob, thr) {

  # Coerce to clean types
  #obs  <- as.integer(obs)
  #prob <- as.numeric(prob)

  # WARNING (end user): quiet = TRUE only silences pROC's repeated console
  # messages "Setting levels: control = 0, case = 1" and
  # "Setting direction: controls < cases". pROC's auto-detection is unchanged,
  # so AUC values are identical -- we just hide the per-call chatter (this runs
  # once per model x fold x repeat). This assumes `obs` is coded 0 = absence,
  # 1 = presence and `prob` = P(presence). If you reuse extract_metrics() with a
  # different coding, the now-hidden messages won't warn you and the AUC could be
  # inverted -- drop quiet = TRUE (or pass explicit levels/direction) to check.
  roc_obj <- pROC::roc(obs, prob, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  cls     <- factor(ifelse(prob > thr, 1, 0), levels=c(0,1))
  cm      <- caret::confusionMatrix(cls, factor(obs), positive="1")
  
  # Extract confusion matrix counts for MCC calculation
  tp <- as.numeric(cm$table[2,2])
  tn <- as.numeric(cm$table[1,1])
  fp <- as.numeric(cm$table[2,1])
  fn <- as.numeric(cm$table[1,2])
  
  # Compute MCC (check if denominator is zero)
  mcc_num <- tp * tn - fp * fn
  mcc_den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- mcc_num / mcc_den
  mcc <- ifelse(mcc_den == 0, NA, (tp * tn - fp * fn) / mcc_den)
  
  ndigits <- 2
  with(as.list(cm$byClass), {
    data.frame(
      AUC         = round(auc_val, ndigits),
      Accuracy    = round(cm$overall["Accuracy"], ndigits),
      Sensitivity = round(Sensitivity, ndigits), # True positive rate (TPR), recall
      Specificity = round(Specificity, ndigits), # True negative rate (TNR)
      BA          = round((Sensitivity+Specificity)/2, ndigits), 
      Kappa       = round(cm$overall["Kappa"], ndigits),
      TSS         = round(Sensitivity+Specificity-1, ndigits),
      Precision   = round(`Pos Pred Value`, ndigits),
      F1          = round(2*Precision*Sensitivity / (Precision + Sensitivity), ndigits),
      MCC         = round(mcc, ndigits) # Matthews correlation coefficient
      
      # Reported name	  Also known as...
      #             Accuracy: Classification accuracy
      #  No Information Rate: Null accuracy, baseline accuracy (accuracy of always predicting the majority class)
      #                Kappa: Cohen's κ (measures agreement between predicted and observed classifications, adjusted for chance agreement; ranges from -1 to 1 - 0 means chance, 1 perfect agreement)
      #          Sensitivity: True positive rate (TPR), recall
      #          Specificity: True negative rate (TNR)
      #       Pos Pred Value:	Positive predictive value (PPV), precision
      #       Neg Pred Value:	Negative predictive value (NPV)
      #           Prevalence:	Class prior (proportion of positives in the data)
      #       Detection Rate:	True positives / total observations (sometimes called "TPR × prevalence")
      # Detection Prevalence:	Predicted prevalence (proportion of cases the model calls "positive")
      #    Balanced Accuracy:	(Sensitivity + Specificity) / 2, also "average accuracy"
      #
      # Mcnemar's Test P-Value:	Test of symmetry in the off‐diagonals of the confusion matrix
      
    )
  })
}


# Build one set of folds (optionally stratified)
make_folds_once_binary_models <- function(y, k, stratified = TRUE) {
  if (stratified) {
    caret::createFolds(y = y, k = k, list = TRUE, returnTrain = FALSE)
  } else {
    idx <- sample(seq_along(y))
    split(idx, cut(seq_along(idx), breaks = k, labels = FALSE))
  }
}

# helper to save fold-level rasters
save_cv_rast <- function(r, model, rep_i, fold_i) {
  fp <- file.path(pred_dir,
                  sprintf("%s_rep%02d_fold%02d.tif", model, rep_i, fold_i))
  terra::writeRaster(r, fp, NAflag = -9999, overwrite = TRUE)
  fp
}

# ------------------------------------------------------------------------------
#                            03_count_models.R 
#                           auxiliary functions
# ------------------------------------------------------------------------------

get_metrics <- function(obs, pred) {

  #obs  <- as.numeric(obs)
  #pred <- as.numeric(pred)

  data.frame(
    RMSE = RMSE(pred, obs),
    MAE = MAE(pred, obs),
    Pearson = suppressWarnings(stats::cor(obs, pred, method = "pearson")),
    Spearman = suppressWarnings(stats::cor(obs, pred, method = "spearman"))
  )
}

# Proportional K-folds by combination of factors (no count bins)
make_folds_count_models <- function(df, by, k = 10, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  strata <- interaction(df[, by], drop = TRUE, lex.order = TRUE)
  caret::createFolds(strata, k = k, list = TRUE, returnTrain = FALSE)  # returns test indices
}

# Helper for uncertainty in predictions: bootstrapping Figure 4
align_factor_levels <- function(train_df, pred_df, facs = c("year","season","month")) {
  for (f in facs) if (f %in% names(train_df)) {
    if (is.factor(train_df[[f]])) train_df[[f]] <- droplevels(train_df[[f]])
    if (f %in% names(pred_df)) {
      if (!is.factor(pred_df[[f]])) pred_df[[f]] <- factor(pred_df[[f]])
      pred_df[[f]] <- factor(pred_df[[f]], levels = levels(train_df[[f]]))
    }
  }
  list(train = train_df, pred = pred_df)
}




# ------------------------------------------------------------------------------
#                         predictions at centroids 
#                           auxiliary functions
# ------------------------------------------------------------------------------

impute_pi_knn <- function(df, value_col, k = 8,
                          lon_col = "lon", lat_col = "lat",
                          out_col = NULL, replace = FALSE) {
  
  # Impute NA values of a column with the mean of k nearest neighbors (by lon/lat)
  # - df: data.frame with lon/lat columns and the value column
  # - value_col: name of the column to impute (character)
  # - k: number of neighbors to average (default 8)
  # - lon_col, lat_col: coordinate column names
  # - out_col: optional new column name; if NULL, overwrite value_col
  # - replace: if TRUE, overwrite value_col (ignored if out_col is provided)
  # Needs RANN package!
  
  stopifnot(value_col %in% names(df),
            lon_col %in% names(df),
            lat_col %in% names(df))
  v <- df[[value_col]]
  good <- !is.na(v)
  bad  <- which(!good)

  # nothing to do
  if (length(bad) == 0L || sum(good) == 0L) {
    if (!length(bad)) message(sprintf("No NA in '%s' to impute.", value_col))
    return(df)
  }

  k_use <- min(k, sum(good))
  coords <- as.matrix(df[, c(lon_col, lat_col)])

  # k-NN from NA points (query) to available points (data)
  nn <- RANN::nn2(
    data  = coords[good, , drop = FALSE],
    query = coords[bad,  , drop = FALSE],
    k     = k_use
  )

  # Map neighbor indices back to the full data indices
  neigh_idx_global <- matrix(which(good)[nn$nn.idx], nrow = length(bad), ncol = k_use)

  # Mean of neighbor values
  imputed_vals <- rowMeans(matrix(v[neigh_idx_global], nrow = length(bad), ncol = k_use), na.rm = TRUE)

  # Write results
  res <- v
  res[bad] <- imputed_vals

  if (!is.null(out_col)) {
    df[[out_col]] <- res
  } else if (isTRUE(replace)) {
    df[[value_col]] <- res
  } else {
    # default: overwrite the original
    df[[value_col]] <- res
  }
  df
}

estimate_theta <- function(df_counts, fallback = 10) {
  #
  # This function estimates the Negative-binomial theta parameter
  #
  th <- tryCatch(MASS::glm.nb(count ~ 1, data = df_counts)$theta, error = function(e) NA_real_)
  if (is.na(th)) fallback else th
}


choose_grid_by_place <- function(chosen_place,
                                 centroids,
                                 places_qld,
                                 crs_metric = 3577,   # Australian Albers (metres)
                                 verbose = TRUE) {
  # Function to help with a figure

  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))

  # Unique grid centroids & sf objects
  centroids_cells <- dplyr::distinct(centroids, grid_id, lon, lat)
  centroids_sf    <- sf::st_as_sf(centroids_cells, coords = c("lon","lat"), crs = 4326)

  # Places as sf (dedupe names)
  places_sf <- sf::st_as_sf(places_qld, coords = c("lon","lat"), crs = 4326) |>
    dplyr::distinct(name, .keep_all = TRUE)

  # Find the place row
  place_sf <- places_sf |>
    dplyr::filter(grepl(chosen_place, name, ignore.case = TRUE)) |>
    dplyr::slice(1)

  if (nrow(place_sf) == 0)
    stop(sprintf("No place matched '%s' in places_qld", chosen_place), call. = FALSE)

  # Nearest grid cell in a metric CRS
  place_m     <- sf::st_transform(place_sf, crs_metric)
  centroids_m <- sf::st_transform(centroids_sf, crs_metric)

  idx <- sf::st_nearest_feature(place_m, centroids_m)

  chosen_grid <- centroids_cells$grid_id[idx]
  dist_km <- as.numeric(sf::st_distance(place_m, centroids_m[idx, ])) / 1000

  if (isTRUE(verbose)) message(sprintf("Chosen place: %s  → grid_id: %s (%.1f km away)", place_sf$name[1], chosen_grid, dist_km))

  # Return a small list (easy to use)
  return(
    list(
      grid_id     = as.integer(chosen_grid),
      place_name  = place_sf$name[1],
      place_lon   = sf::st_coordinates(place_sf)[1,1],
      place_lat   = sf::st_coordinates(place_sf)[1,2],
      distance_km = as.numeric(dist_km)
    )
  )
}


# ------------------------------------------------------------------------------
#                     Modelling dataset (shared by 02 & 03)
# ------------------------------------------------------------------------------

build_ab_data <- function(path = file.path(dir_data, "ab.csv")) {
  # Build the abundance modelling frame from the analysis-ready ab.csv:
  # keep Andrew's QLD trap surveys, select modelling columns, drop NAs, coerce
  # factors + date, and zero-out near-zero numeric noise. Deterministic and cheap
  # (no model fitting), so any script (e.g. 03_zi_models.R) can rebuild `ab_data`
  # on its own WITHOUT re-running 02's cross-validation.
  # Requires dir_data + tidyverse (loaded via 00_setup.R).
  zero_threshold <- 1e-6
  read.csv(path)  |>
    dplyr::filter(
      source == "andrew",           # Only observations from Andrew's surveys
      region == "QLD",              # Only observations from QLD surveys
      method == "T"                 # Only observations from trap surveys
    )  |>
    dplyr::select(
      ID,                           # unique identifier
      lon,                          # longitude (site collection)
      lat,                          # latitude (site collection)
      presence,                     # response in distribution models (0 absent, 1 present)
      count,                        # number of mozzies caught per trap
      year,                         # year of site collection
      month,                        # month of site collection
      day,                          # day of site collection
      date,                         # date (YYYY-MM-DD) of site collection
      week,                         # week of site collection
      season,                       # season (dry/wet) of site collection
      season2,                      # season (early/late dry/wet) of site collection
      method,                       # method of collection
      region,                       # region where site collection took place
      source,                       # data source (Nigel/Andrew's surveys)
      ID_type,
      habitat,                      # habitat type (high altitude/brackish/freshwater)
      site,                         # site location (Andrew's dataset)
      site2,                        # labels for site location
      ppa21,                        # accumulated precip. over 21 days prior to trap collection
      tmaxm21,                      # mean max temperature over 21 days prior
      tminm21,                      # mean min temperature over 21 days prior
      rhm21,                        # mean relative humidity over 21 days prior
      water_occ,                    # surface water occurrence frequency (1984-2021)
      water_occ_99,                 # distance to 99% water-occurrence location
      mang_rf_5km,                  # mangrove + rainforest cover within 5 km
      dist_coast,                   # distance from site to coastline (km)
      elev,                         # elevation
      ppa21_z,                      # standardised (divided by max) versions of the above
      tmaxm21_z,
      tminm21_z,
      rhm21_z,
      elev_z,
      mang_rf_5km_z,
      water_occ_z,
      water_occ_99_z
    )  |>
    tidyr::drop_na()  |>
    dplyr::mutate(dplyr::across(c(year, season, month, week), as.factor))  |>
    dplyr::distinct()  |>
    dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y"))  |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(abs(.) < zero_threshold, 0, .)))
}


message("✅ Auxiliary functions loaded.")