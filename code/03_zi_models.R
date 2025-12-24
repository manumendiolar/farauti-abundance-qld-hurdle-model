# ==============================================================================
# Anopheles farauti RELATIVE ABUNDANCE 
# ZERO INFLATED APPROACH: ZIP and ZINB
#
# Fitting, Diagnostics, Cross-Validation & Prediction
#
# This script fits two zero-inflated models to estimate relative abundance of
# An. farauti 
#
# Manuela, M.
# 24-12-2025
# ==============================================================================

# NOTE: Run 02_abund_models.R first (creates ab_data, get_metrics, etc.)

# Auxiliary functions / setup --------------------------------------------------
source(here::here("code","00_setup.R"))


# ------------------------------------------------------------------------------
#                        FITTING MODELS TO FULL-DATA 
#                        
# ------------------------------------------------------------------------------
set.seed(42)

zi_formula_count <- count ~ year + season + month + 
  ppa21_z + tmaxm21_z + tminm21_z + rhm21_z + 
  elev_z + mang_rf_5km_z + 
  water_occ_z + water_occ_99_z

zi_formula_zero  <- ~ year + season + month   # simpler zero model helps convergence (or ~1)


# ZERO-INFLATED POISSON

# With glmmTMB package
ZIPa <- glmmTMB(
  formula = zi_formula_count,
  ziformula = zi_formula_zero, 
  family = poisson, # with this package the zero-inflation part is modeled as a Bernoulli always
  data = ab_data, 
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)
# With pscl package
#ZIPa <- pscl::zeroinfl(
# count ~ year + season + month + ppa21_z + tmaxm21_z + tminm21_z + rhm21_z + elev_z + mang_rf_5km_z + 
#   water_occ_z + water_occ_99_z | year + season + month,
#  data = ab_data, 
#  dist = "pois", 
#  link = "logit",
#  control = zeroinfl.control(maxit = 2000, EM = TRUE),
#  na.action = na.exclude, 
#  model = FALSE, 
#  x = FALSE, 
#  y = FALSE
#)


# ZERO-INFLATED NEGATIVE BINOMIAL

# With glmmTMB package
ZINBa <- glmmTMB(
  formula = zi_formula_count,
  ziformula = zi_formula_zero, 
  family = nbinom2, 
  data = ab_data, 
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)
# With pscl package
#ZINBa <- pscl::zeroinfl(
#  count ~ year + season + month + ppa21_z + tmaxm21_z + tminm21_z + rhm21_z + elev_z + mang_rf_5km_z + 
#    water_occ_z + water_occ_99_z | year + season + month,
#  data = ab_data,
#  dist = "negbin",
#  link = "logit",
#  control = zeroinfl.control(maxit = 2000, EM = TRUE),
#  na.action = na.exclude,
#  model = FALSE, 
#  x = FALSE, 
#  y = FALSE
#)



# Predictions (response scale)
preds_ZIPa  <- as.numeric(predict(ZIPa,  type = "response"))
preds_ZINBa <- as.numeric(predict(ZINBa, type = "response"))

# Metrics (log1p scale)
metrics_full_zi <- dplyr::bind_rows(
  get_metrics(log1p(ab_data$count), log1p(preds_ZIPa)) |>
  dplyr::mutate(Model="ZIP",  Method="Full"),
  get_metrics(log1p(ab_data$count), log1p(preds_ZINBa)) |> 
  dplyr::mutate(Model="ZINB", Method="Full")
) |> 
  dplyr::select(Model, RMSE, MAE, Pearson, Spearman, Method)

# Metrics without CV
metrics_full_zi

# Save
write.csv(
  metrics_full_zi[ ,1:5],
  file.path(dir_tables, "zi_model_full.csv"),
  row.names = FALSE
)



# ------------------------------------------------------------------------------
#                        Repeated K-fold CV 
#                   (with optional stratification) 
# ------------------------------------------------------------------------------
set.seed(123)

# Setup
K    <- 10          # folds
REPS <- 5           # repeats
fac_key <- interaction(ab_data[, c("year","season","month")], drop=TRUE, lex.order=TRUE)
zp_key  <- ifelse(ab_data$count == 0, "Z", "P")
strata  <- interaction(fac_key, zp_key, drop=TRUE, lex.order=TRUE)
nall <- nrow(ab_data)
oof_ZIP   <- matrix(NA_real_, nrow = nall, ncol = REPS)
oof_ZINB  <- matrix(NA_real_, nrow = nall, ncol = REPS)
metrics_reps_zi <- vector("list", REPS)

for (rep_i in seq_len(REPS)) {
  cat("REP: ", rep_i, "------------------------\n")
  set.seed(2000 + rep_i)
  folds <- caret::createFolds(strata, k = K, list = TRUE, returnTrain = FALSE)
  
  pr_zip  <- rep(NA_real_, nall)
  pr_zinb <- rep(NA_real_, nall)
    
  cat("fold ", "\n")
  for (k_i in seq_along(folds)) {
    cat(k_i, " ")
    te_idx <- folds[[k_i]]
    tr_idx <- setdiff(seq_len(nall), te_idx)
    
    df_train <- droplevels(ab_data[tr_idx, ])
    df_test  <- ab_data[te_idx, , drop = FALSE]
    
    # ZIP
    ZIP_fit <- glmmTMB(
      formula = zi_formula_count,
      ziformula = zi_formula_zero,
      family = poisson, 
      data = df_train, 
      control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
    )
    pr_zip[te_idx] <- as.numeric(predict(ZIP_fit, df_test, type = "response"))

    # ZINB
    ZINB_fit <- glmmTMB(
      formula = zi_formula_count,
      ziformula = zi_formula_zero,
      family = nbinom2, 
      data = df_train, 
      control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
    )
    pr_zinb[te_idx] <- as.numeric(predict(ZINB_fit, df_test, type = "response"))
  } 
  cat("\n")
  oof_ZIP[ , rep_i] <- pr_zip
  oof_ZINB[ , rep_i] <- pr_zinb

  # metrics this repeat
  pred_list_rep <- list(
    ZIP = pr_zip, 
    ZINB = pr_zinb
    )
  
  m_rep <- purrr::imap_dfr(pred_list_rep, ~{

    #m <- get_metrics(ab_data$count, .x) # original scale
    m <- get_metrics(log1p(ab_data$count), log1p(.x)) # log scale
    m$Model <- .y
    m$Method <- sprintf("Kfold_%dx (rep %d)", K, rep_i)
    m
  })
  
  metrics_reps_zi[[rep_i]] <- m_rep

} 

# Get metrics per-repeat 
metrics_kfold_zi_reps <- dplyr::bind_rows(metrics_reps_zi)
metrics_kfold_zi_reps 


# Aggregate K-fold metrics across repeats
metrics_kfold_summary <- metrics_kfold_zi_reps |> 
  dplyr::group_by(Model) |> 
  dplyr::summarise(
    RMSE = mean(RMSE, na.rm=TRUE),
    MAE  = mean(MAE,  na.rm=TRUE),
    Pearson  = mean(Pearson,  na.rm=TRUE),
    Spearman = mean(Spearman, na.rm=TRUE),
    .groups="drop"
  ) |> 
  dplyr::mutate(Model = factor(Model, levels=c("ZIP","ZINB"))) |> 
  dplyr::arrange(Model)

# Check
metrics_kfold_summary

# Save
write.csv(
  metrics_kfold_summary,
  file.path(dir_tables, sprintf("zi_model_kfold_%dx%dr_log.csv", K, REPS)),
  row.names = FALSE
)


