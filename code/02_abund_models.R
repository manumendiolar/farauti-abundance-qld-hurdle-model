# ==============================================================================
# Anopheles farauti RELATIVE ABUNDANCE 
# HURDLE APPROACH: 2nd component
#
# MODELLING POSITIVE COUNTS 
# Fitting, Diagnostics, Cross-Validation & Prediction
#
# This script fits various statistical and machine learning models to estimate 
# counts of An. farauti (positive counts or all depending on framework)
#
# Manuela, M.
# 17-10-2025
# ==============================================================================

    
# Auxiliary functions / setup --------------------------------------------------
source(file.path("code", "00_setup.R"))


# Data -------------------------------------------------------------------------
ab_data <- read.csv(file.path(dir_data, "ab.csv"))  |> 
  filter(source == "andrew")  |>  # Only observations from Andrew's surveys 
  filter(region == "QLD")  |>     # Only observations from QLD surveys
  filter(method == "T")  |>       # Only observations from trap surveys
  dplyr::select(
    ID,                           # unique identifier 
    lon,                          # longitude (site collection)
    lat,                          # latitude (site collection)
    presence,                     # response variable in distribution models (0: absent, 1: present)
    count,                        # number of mozzies caught per trap
    year,                         # year of site collection
    month,                        # month of site collection
    day,                          # day of site collection
    date,                         # date (YYYY-MM-DD) of site collection
    week,                         # week of site collection
    season,                       # season (dry/wet) of site collection
    season2,                      # season (early/late dry and early/late dry) of site collection  
    method,                       # method of collection  
    region,                       # region where site collection took place  
    source,                       # data source (Nigel/Andrew's surveys)
    ID_type, 
    habitat,                      # habitat type (high altitude/brackish/freshwater), specifically for Andrew's dataset
    site,                         # site location, specifically for Andrew's dataset
    site2,                        # labels for site location
    ppa21,                        # accumulated precipitation over the 21 days prior trap collection
    tmaxm21,                      # mean maximum temperature over the 21 days prior trap collection
    tminm21,                      # mean minimum temperature over the 21 days prior trap collection
    rhm21,                        # mean relative humidity over the 21 days prior trap collection
    water_occ,                    # frequency with which water was present on the surface from March 1984 to December 2021 (also called surface water occurrence)
    water_occ_99,                 # derived (distance to a location with 99% frequency which water was present on the surface from March 1984 to December 2021
    mang_rf_5km,                  # derived mangrove and rainforest cover over a 5 km radius from site collection
    dist_coast,                   # derived distance from the site collection to the coastline (km)
    elev,                         # elevation
    ppa21_z,                      # standardised (each divided by their max value) version of the above mentioned variable
    tmaxm21_z, 
    tminm21_z, 
    rhm21_z, 
    elev_z, 
    mang_rf_5km_z, 
    water_occ_z, 
    water_occ_99_z
    )  |>
  drop_na()  |>
  mutate(
    across(c(year, season, month, week), as.factor))  |>
  distinct()


# Check
head(ab_data)

# Ensure the 'date' column is Date type
ab_data$date <- as.Date(ab_data$date, format = "%d/%m/%Y")

# Check
head(ab_data)
glimpse(ab_data)

# OPTIONAL
# Remove records with ID 700 and 734 from the dataset
# ab_data <- ab_data  |> filter(!ID %in% c(700, 734))  # this is just to see outcome without these...

# Formatting: "near-zero values" replace with zero
zero_threshold <- 1e-6
ab_data <- ab_data  |> dplyr::mutate(across(where(is.numeric), ~ ifelse(abs(.) < zero_threshold, 0, .)))



#-------------------------------------------------------------------------------
#                    DATA FOR MODELLING SPECIES COUNTS
#                      ONLY POSITIVE (i.e., COUNT > 0)
# ------------------------------------------------------------------------------
ab_data_pos <- ab_data  |> dplyr::filter(count > 0)

# Define vector of predictor names
predictors <- c("year", "season", "month", 
                "ppa21", "tmaxm21", "tminm21", "rhm21", 
                "elev", "mang_rf_5km", "water_occ", "water_occ_99")




# ------------------------------------------------------------------------------
#                        FITTING MODELS TO FULL-DATA 
#                        
# ------------------------------------------------------------------------------
set.seed(42)

# RANDOM FOREST
RFa <- randomForest(
  count ~ year + season + month + ppa21 + tmaxm21 + tminm21 + rhm21 + elev + mang_rf_5km + water_occ + water_occ_99, 
  data = ab_data_pos, 
  ntree = 1500,
  mtry = 4, #3
  nodesize = 5, #1
  importance = TRUE,
  keep.forest = TRUE
)


# BOOSTED REGRESSION TREES
BRTa <- dismo::gbm.step(
  data = ab_data_pos, 
  gbm.x = which(names(ab_data_pos) %in% predictors),
  gbm.y = which(names(ab_data_pos) == "count"),
  family = "poisson",
  tree.complexity = 8, 
  learning.rate = 0.001, 
  bag.fraction = 0.64, 
  plot.main = FALSE, 
  plot.folds = FALSE,
  verbose = FALSE, 
  silent = TRUE
)
  

# GENERALISED LINEAR MODEL (stepAIC)
GLMa <- MASS::stepAIC(
  glmmTMB(
    count ~ year + season + month + ppa21 + tmaxm21 + tminm21 + rhm21 + elev + mang_rf_5km + water_occ + water_occ_99,
    data = ab_data_pos,
    family = truncated_poisson(link = "log"),
    na.action = na.exclude
  ),
  scope = list(
    lower = ~ year + month,
    upper = ~ year + season + month + ppa21 + tmaxm21 + tminm21 + rhm21 + elev + mang_rf_5km + water_occ + water_occ_99
    ),
  direction = "both",
  trace     = FALSE
)


# GENERALISED ADDITIVE MODEL (shrinkage via select=TRUE)
GAMa <- mgcv::gam(
  formula = count ~ year + season + month + s(ppa21, k=5) + s(tmaxm21, k=5) + s(tminm21, k=5) + s(rhm21, k=5) + s(elev, k=5) + s(mang_rf_5km, k=5) + s(water_occ, k=5) + s(water_occ_99, k=5),
  data    = ab_data_pos,
  family  = nb(),
  select  = TRUE,
  method  = "GCV.Cp"
)
# Using a different algorithm for faster and more stable smoothing‐parameter estimation
# GAMa <- mgcv::bam(
#   count ~ year + season + month +
#     s(ppa21, k = 5) + 
#     s(tmaxm21, k = 5) + 
#     s(tminm21, k = 5) + 
#     s(rhm21, k = 5) +
#     s(elev, k=5) +
#     s(mang_rf_5km, k = 5) + 
#     s(water_occ, k = 5) +
#     s(water_occ_99, k = 5),
#   data = ab_data_pos,
#   family = nb(),
#   na.action = na.exclude,
#   select = TRUE,
#   method = "fREML",                   # faster, more stable smoothing‐parameter estimation
#   discrete = TRUE,                    # build discrete basis to speed up computations
#   nthreads = parallel::detectCores()  # use all your cores
# )




# ------------------------------------------------------------------------------
#                     Predictions at sampled locations 
#                       Performance metrics full data
# ------------------------------------------------------------------------------
preds_RFa  <- as.numeric(predict(RFa, type = "response"))
#preds_BRTa <- as.numeric(predict(BRTa, newdata = ab_data_pos[, c("count", predictors)], n.trees = best_trees, type = "response")) 
preds_BRTa <- as.numeric(predict(BRTa, n.trees = BRTa$gbm.call$best.trees, type = "response"))
preds_GLMa <- as.numeric(predict(GLMa, type = "response"))
preds_GAMa <- as.numeric(predict(GAMa, type = "response"))

# Combine all in one list
preds_full <- list(
  RFa = preds_RFa,
  BRTa = preds_BRTa, 
  GLMa = preds_GLMa, 
  GAMa = preds_GAMa
)


# Compute metrics
metrics_full <- bind_rows(
  lapply(names(preds_full), function(m) {
    df <- get_metrics(ab_data_pos$count, preds_full[[m]])
    df$Model  <- m
    df$Method <- "Full"
    df
  })
)

# ENSEMBLE MODEL (compute weights & prediction)
weights <- metrics_full$Pearson 
weights <- setNames(weights, metrics_full$Model) # assign names
weights_t <- weights / sum(weights)         # normalizing to sum to one
preds_ens <- preds_full[["RFa"]] * weights_t[["RFa"]] + 
  preds_full[["BRTa"]] * weights_t[["BRTa"]] +
  preds_full[["GLMa"]] * weights_t[["GLMa"]] +
  preds_full[["GAMa"]] * weights_t[["GAMa"]]

# Update
preds_full[["ENSa"]] <- as.numeric(preds_ens)

# Update metrics
metrics_full <- bind_rows(
  lapply(names(preds_full), function(m) {
    df <- get_metrics(ab_data_pos$count, preds_full[[m]])
    df$Model  <- m
    df$Method <- "Full"
    df
  })
)

# Rename model levels
metrics_full <- metrics_full  |>
  mutate(
    Model = recode(
      Model,
      RFa  = "RF",
      BRTa = "BRT",
      GLMa = "GLM",
      GAMa = "GAM",
      ENSa = "ENS",
    )
  )

# Check table
rownames(metrics_full) <- NULL
metrics_full

# Re-order
metrics_full <- metrics_full  |> 
  dplyr::select(Model, RMSE, MAE, Pearson, Spearman, Method)

# Check
metrics_full

# Save
write.csv(
  metrics_full[ ,1:5],
  file.path(dir_tables, "count_model_full.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
#                        Repeated K-fold CV 
#                   (with optional stratification) 
# ------------------------------------------------------------------------------
set.seed(123)

# Settings
K        <- 10
reps     <- 5
STRATIFY <- TRUE                # stratify by count quantiles
npos     <- nrow(ab_data_pos)    # Number of obs (count > 0)
glm_formula <- formula(GLMa)     
gam_formula <- formula(GAMa) 


# Storage for OOF predictions
oof_RFa  <- matrix(0, nrow = npos, ncol = reps)
oof_BRTa <- matrix(0, nrow = npos, ncol = reps)
oof_GLMa <- matrix(0, nrow = npos, ncol = reps)
oof_GAMa <- matrix(0, nrow = npos, ncol = reps)
oof_ENSa <- matrix(0, nrow = npos, ncol = reps)

metrics_reps <- vector("list", reps)

# Repeated K-fold loop
for (rep_i in seq_len(reps)) {
  cat("REP: ", rep_i, "------------------------\n")
  set.seed(1000 + rep_i)
   
  folds <- caret::createFolds(
    interaction(ab_data_pos[, c("year","season","month")], drop = TRUE, lex.order = TRUE),
    k = K, list = TRUE, returnTrain = FALSE
  )
  
  pr_RFa  <- rep(0, npos)
  pr_BRTa <- rep(0, npos)
  pr_GLMa <- rep(0, npos)
  pr_GAMa <- rep(0, npos)
  
  cat("fold ", "\n")
  for (k_i in seq_along(folds)) {
    
    cat(k_i, " ")
    te_idx <- folds[[k_i]]
    tr_idx <- setdiff(seq_len(npos), te_idx)
    
    df_train <- droplevels(ab_data_pos[tr_idx, ])
    df_test <- ab_data_pos[te_idx, , drop = FALSE]
    table(df_train$season); table(df_test$season)
    table(df_train$year); table(df_test$year)
    table(df_train$month); table(df_test$month)
    #al <- align_factor_levels(df_train, df_test)
    #df_train <- al$train
    #df_test <- al$test
    
   
    # RANDOM FOREST
    RFa_k <- randomForest(
      count ~ year + season + month + ppa21 + tmaxm21 + tminm21 + rhm21 + elev + mang_rf_5km + water_occ,
      data = df_train, 
      ntree = 1500, 
      mtry = 4,
      nodesize = 5,
      importance = FALSE, 
      keep.forest = TRUE
      )
    pr_RFa[te_idx] <- as.numeric(predict(RFa_k, df_test, type="response"))

    # BOOSTED REGRESSION TREES 
    BRTa_k <- dismo::gbm.step(
      data = df_train, 
      gbm.x = which(names(df_train) %in% predictors),
      gbm.y = which(names(df_train) == "count"),
      family = "poisson",     
      tree.complexity = 3,
      learning.rate = 0.01,
      bag.fraction = 0.60,
      plot.main = FALSE,
      plot.folds = FALSE,
      verbose = FALSE,
      silent = TRUE,
      # new
      n.trees = 4000, 
      interaction.depth = 3,
      shrinkage = 0.001,         
      n.minobsinnode = 5   
    )
    
    if (is.null(BRTa_k)) {
      cat("(using gbm::gbm) ")
      gbm_k <- gbm::gbm(
        count ~ ., data = df_train[, c("count", predictors)],
        distribution = "poisson",
        n.trees = 4000, 
        interaction.depth = 3,
        shrinkage = 0.01, 
        bag.fraction = 0.6,  # OOB only works if bag.fraction < 1
        n.minobsinnode = 5,
        cv.folds          = 5,     # <-- turn on internal K-fold CV
        keep.data = FALSE, 
        verbose = FALSE
      )
      # early stopping using OOB; use plot.it = FALSE in the loop for speed
      #best_trees <- suppressMessages(gbm::gbm.perf(gbm_k, method = "OOB", plot.it = FALSE))
      best_trees <- gbm::gbm.perf(gbm_k, method = "cv", plot.it = FALSE)
      pr_BRTa[te_idx] <- as.numeric(predict(gbm_k, newdata = df_test, n.trees = best_trees, type = "response"))
    } else {
      pr_BRTa[te_idx] <- as.numeric(predict(BRTa_k, df_test, n.trees=BRTa_k$gbm.call$best.trees, type="response"))
    }
   
    # GENERALISED LINEAR MODEL (truncated Poisson) 
    fit_glm <- glm(glm_formula, data = df_train, family = poisson(link = "log"))  # Poisson (non-truncated) first
    
    # Truncated Poisson using glm coefficients as starts
    GLMa_k <- glmmTMB(
      formula = glm_formula,
      data    = df_train,
      family  = truncated_poisson(link = "log"),
      start   = list(beta = unname(stats::coef(fit_glm)))  # <-- flat 'beta', not 'cond'
    )
    
    pr_GLMa[te_idx] <- as.numeric(predict(GLMa_k, df_test, type="response"))
    
    # GENERALISED ADDITIVE MODEL (NB)
    GAMa_k <- mgcv::gam(
      formula = gam_formula,
      data = df_train,
      family = nb(),
      select = TRUE, 
      method = "GCV.Cp"
      )
    pr_GAMa[te_idx] <- as.numeric(predict(GAMa_k, df_test, type="response"))
    
  } 
  cat("\n")
  
  # Ensemble OOF for this repeat (weights from full fit)
  pr_ENSa <- weights_t["RFa"]*pr_RFa + 
    weights_t["BRTa"]*pr_BRTa +
    weights_t["GLMa"]*pr_GLMa +
    weights_t["GAMa"]*pr_GAMa
  
  # stash OOF
  oof_RFa[,rep_i]  <- pr_RFa
  oof_BRTa[,rep_i] <- pr_BRTa
  oof_GLMa[,rep_i] <- pr_GLMa
  oof_GAMa[,rep_i] <- pr_GAMa
  oof_ENSa[,rep_i] <- pr_ENSa
  
  # metrics this repeat
  pred_list_rep <- list(
    RF = pr_RFa, 
    BRT = pr_BRTa,
    GLM = pr_GLMa, 
    GAM = pr_GAMa, 
    ENS = pr_ENSa
    )
  
  m_rep <- purrr::imap_dfr(pred_list_rep, ~{
    #m <- get_metrics(ab_data_pos$count, .x) # original scale
    m <- get_metrics(log1p(ab_data_pos$count), log1p(.x)) # log scale
    m$Model <- .y
    m$Method <- sprintf("Kfold_%dx (rep %d)", K, rep_i)
    m
  })
  
  metrics_reps[[rep_i]] <- m_rep

} 

# Get metrics per-repeat 
metrics_kfold_all_reps <- dplyr::bind_rows(metrics_reps)
metrics_kfold_all_reps 


# Aggregate K-fold metrics across repeats
metrics_kfold_summary <- metrics_kfold_all_reps  |>
  group_by(Model)  |>
  summarise(
    RMSE = mean(RMSE, na.rm=TRUE),
    MAE  = mean(MAE,  na.rm=TRUE),
    Pearson  = mean(Pearson,  na.rm=TRUE),
    Spearman = mean(Spearman, na.rm=TRUE),
    .groups="drop"
  )  |>
  mutate(Model = factor(Model, levels=c("RF","BRT","GLM","GAM","ENS")))  |>
  arrange(Model)

# Check
metrics_kfold_summary

# Save CSVs
write.csv(
  metrics_kfold_summary,
  file.path(dir_tables, sprintf("count_model_kfold_%dx%dr_log.csv", K, reps)),
  row.names = FALSE
)


