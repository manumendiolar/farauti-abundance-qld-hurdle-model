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
# 24-12-2025
# ==============================================================================

    
# Auxiliary functions / setup --------------------------------------------------
source(here::here("code","00_setup.R"))


# Data -------------------------------------------------------------------------
# Build the shared abundance modelling frame. The wrangling lives in
# build_ab_data() (aux_functions.R) so 03_zi_models.R can rebuild `ab_data` on
# its own -- cheaply, without re-running this script's cross-validation.
ab_data <- build_ab_data()



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

# Re-order
metrics_full <- metrics_full  |> 
  dplyr::select(Model, RMSE, MAE, Pearson, Spearman, Method)

# Check
metrics_full

# Save
# write.csv(
#   metrics_full[ ,1:5],
#   file.path(dir_tables, "count_model_full.csv"),
#   row.names = FALSE
# )


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

# Progress bar + timer over all repeats x folds (reps x K fold-fits)
cv_start <- Sys.time()
pb <- utils::txtProgressBar(min = 0, max = reps * K, style = 3)

# Repeated K-fold loop
for (rep_i in seq_len(reps)) {
  set.seed(1000 + rep_i)
   
  folds <- caret::createFolds(
    interaction(ab_data_pos[, c("year","season","month")], drop = TRUE, lex.order = TRUE),
    k = K, list = TRUE, returnTrain = FALSE
  )
  
  pr_RFa  <- rep(0, npos)
  pr_BRTa <- rep(0, npos)
  pr_GLMa <- rep(0, npos)
  pr_GAMa <- rep(0, npos)
  
  for (k_i in seq_along(folds)) {
    utils::setTxtProgressBar(pb, (rep_i - 1) * K + k_i)
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
      count ~ year + season + month + ppa21 + tmaxm21 + tminm21 + rhm21 + elev + mang_rf_5km + water_occ + water_occ_99,
      data = df_train, 
      ntree = 1500, 
      mtry = 4,
      nodesize = 5,
      importance = FALSE, 
      keep.forest = TRUE
      )
    pr_RFa[te_idx] <- as.numeric(predict(RFa_k, df_test, type="response"))

    # BOOSTED REGRESSION TREES 
    # Keep gbm.step settings simple/stable; use gbm fallback if needed.
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
      # capture.output() swallows gbm's internal "CV: 1..5" fold chatter (printed via
      # cat when cv.folds > 0). Console output only -- the model is unchanged.
      invisible(capture.output(
      gbm_k <- gbm::gbm(
        count ~ ., data = df_train[, c("count", predictors)],
        distribution = "poisson",
        n.trees = 4000, 
        interaction.depth = 3,
        shrinkage = 0.01, 
        bag.fraction = 0.6,  # OOB only works if bag.fraction < 1
        n.minobsinnode = 5,
        cv.folds          = 5,     # <-- turn on internal K-fold CV
        n.cores           = 1,     # run gbm's internal CV sequentially: parallel PSOCK
                                   # workers don't inherit the renv library path, so they
                                   # fail with "no package called 'gbm'". n.cores=1 avoids
                                   # spawning workers (same result, just not parallelised).
        keep.data = FALSE,
        verbose = FALSE
      )
      ))
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
    m <- get_metrics(ab_data_pos$count, .x)  # original (raw) count scale = manuscript Table 3
    #m <- get_metrics(log1p(ab_data_pos$count), log1p(.x))  # log1p scale (display only; cf. paper_fig_S07.R)
    m$Model <- .y
    m$Method <- sprintf("Kfold_%dx (rep %d)", K, rep_i)
    m
  })
  
  metrics_reps[[rep_i]] <- m_rep

} 

close(pb)
cv_mins <- round(as.numeric(difftime(Sys.time(), cv_start, units = "mins")), 1)
message(sprintf("✅ Repeated K-fold complete (%.1f min).", cv_mins))

# Get metrics per-repeat
metrics_kfold_all_reps <- dplyr::bind_rows(metrics_reps)
#metrics_kfold_all_reps 


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

# Save 
# Read by paper_tbl_03.R (Table 3) and predictions_at_centroids.R (ENS weights).
write.csv(
  metrics_kfold_summary,
  file.path(dir_tables, sprintf("count_model_kfold_%dx%dr.csv", K, reps)),
  row.names = FALSE
)


