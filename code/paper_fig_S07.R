# ==============================================================================
# FIGURE SM 07
# 
# Relationship between observed and predicted trap counts of An. farauti data 
# in longitudinal abundance surveys using different models from the 10-fold 
# cross-validation. RF: Random Forest, BRT: Boosted Regression Trees, 
# GLM: Generalised Linear Model (truncated Poisson), 
# GAM: Generalised Additive Model (truncated negative binomial), 
# ENS: Ensemble prediction weighting different models based on Pearson 
# correlation coefficient. ZINB: Zero-inflated negative binomial. 
# Note that abundance was log-transformed for better visualisation.
#
# Panels: RF, BRT, GLM, GAM, ENS (positive-count models) + ZINB (zero-inflated).
#
# REQUIREMENTS:
#   - 03_count_models.R has been run (defines:
#       ab_data, ab_data_pos,
#       oof_RFa, oof_BRTa, oof_GLMa, oof_GAMa, oof_ENSa)
#   - 04_zi_models.R has been run (defines:
#       oof_ZINB and uses the same ab_data object)
#
# Output:
#   - cv_scatter_pred_vs_obs_logscale_ggplot.png in dir_plots
# ==============================================================================

source(file.path("code", "00_setup.R"))

## First run 02_abund_models.R and 03_zi_models.R !


## ---------------------------------------------------------------------------
## 1. Sanity checks
## ---------------------------------------------------------------------------

needed_objects <- c(
  "ab_data", "ab_data_pos",
  "oof_RFa", "oof_BRTa", "oof_GLMa", "oof_GAMa", "oof_ENSa",
  "oof_ZINB"
)

missing <- setdiff(needed_objects, ls())
if (length(missing) > 0) {
  stop(
    "The following objects are missing. ",
    "Make sure 03_count_models.R and 04_zi_models.R have been run:\n  ",
    paste(missing, collapse = ", ")
  )
}

## ---------------------------------------------------------------------------
## 2. Collapse out-of-fold predictions across repeats
## ---------------------------------------------------------------------------

npos <- nrow(ab_data_pos)

stopifnot(
  nrow(oof_RFa)  == npos,
  nrow(oof_BRTa) == npos,
  nrow(oof_GLMa) == npos,
  nrow(oof_GAMa) == npos,
  nrow(oof_ENSa) == npos
)

pred_RF_cv   <- rowMeans(oof_RFa,  na.rm = TRUE)
pred_BRT_cv  <- rowMeans(oof_BRTa, na.rm = TRUE)
pred_GLM_cv  <- rowMeans(oof_GLMa, na.rm = TRUE)
pred_GAM_cv  <- rowMeans(oof_GAMa, na.rm = TRUE)
pred_ENS_cv  <- rowMeans(oof_ENSa, na.rm = TRUE)

obs_pos <- ab_data_pos$count

# ZINB predictions (all counts, then subset to positive)
nall <- nrow(ab_data)
stopifnot(nrow(oof_ZINB) == nall)

pred_ZINB_cv_all <- rowMeans(oof_ZINB, na.rm = TRUE)

idx_pos_all <- which(ab_data$count > 0)
stopifnot(length(idx_pos_all) == npos)
stopifnot(all.equal(obs_pos, ab_data$count[idx_pos_all]))

pred_ZINB_cv_pos <- pred_ZINB_cv_all[idx_pos_all]

## ---------------------------------------------------------------------------
## 3. Build long data frame (with log1p values)
## ---------------------------------------------------------------------------

df_models_pos <- bind_rows(
  data.frame(Model = "RF",   Pred = pred_RF_cv,  Obs = obs_pos),
  data.frame(Model = "BRT",  Pred = pred_BRT_cv, Obs = obs_pos),
  data.frame(Model = "GLM",  Pred = pred_GLM_cv, Obs = obs_pos),
  data.frame(Model = "GAM",  Pred = pred_GAM_cv, Obs = obs_pos),
  data.frame(Model = "ENS",  Pred = pred_ENS_cv, Obs = obs_pos)
)

df_zinb <- data.frame(
  Model = "ZINB",
  Pred  = pred_ZINB_cv_pos,
  Obs   = obs_pos
)

df_plot <- bind_rows(df_models_pos, df_zinb) |>
  mutate(
    Model   = factor(Model, levels = c("RF", "BRT", "GLM", "GAM", "ENS", "ZINB")),
    logPred = log1p(Pred),
    logObs  = log1p(Obs)
  )
head(df_plot)
pretty_vals <- c(1, 2, 5, 10, 20, 50)

## ---------------------------------------------------------------------------
## 4. ggplot: multi-panel scatter, log1p scale (like your base-R version)
## ---------------------------------------------------------------------------

p <- ggplot(df_plot, aes(x = logPred, y = logObs)) +
  geom_point(shape = 16, size = 2.25, alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", col = "grey60") +
  #scale_x_continuous(breaks = log1p(pretty_vals), labels = pretty_vals) +
  #scale_y_continuous(breaks = log1p(pretty_vals), labels = pretty_vals) +
  facet_wrap(~ Model, nrow = 3) + # , scales = "free_x"
  labs(
    x = "Predicted log trap counts",
    y = "Observed log trap counts"
  ) +
  theme_bw() +
  theme(
    panel.grid       = element_blank(),
    strip.text       = element_text(face = "bold", size = 12),
    axis.text        = element_text(size = 11),
    axis.title       = element_text(size = 12),
    #axis.title.x     = element_text(size = 12),
    #axis.title.y     = element_text(size = 12),
    panel.spacing    = unit(1, "lines"),
    plot.margin      = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
  )

# Check
print(p)


# Save
ggsave(
  file.path(dir_plots, "abund_models_cv_scatter_pred_vs_obs_logscale.png"), 
  p, 
  width = 6.5, 
  height = 8, 
  dpi = 300
)