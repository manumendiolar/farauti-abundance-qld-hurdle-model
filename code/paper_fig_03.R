# ------------------------------------------------------------------------------
# FIGURE 3
#
# Time series of observed and predicted An. farauti trap counts at the five 
# longitudinal sampling sites from August 1995 to September 1997. Predictions 
# are obtained from the hurdle model, with a BRT used for the distribution 
# component (suitability) and a Random Forest for abundance conditional on 
# presence. Trap counts at each site and sampling date were computed according 
# to Equation (2), following the workflow in Algorithm 1. Black solid lines and 
# points show observed values (summed across traps at each site and date). Blue 
# dashed lines and square points show the bootstrap mean prediction, and shaded 
# bands indicate 95\% bootstrap percentile intervals based on 1{,}000 resampled 
# refits of the hurdle model.
# ------------------------------------------------------------------------------


# Auxiliary functions / setup --------------------------------------------------
source(file.path("code", "00_setup.R"))

# Assumes 01_dist_models.R has been executed.
# Assumes 02_abund_models.R has been executed.


# ==============================================================
# Bootstrap bands for brt_rf_t025:
# E* = 1(pi_BRT > tau) * pi_BRT * mu_RF
#   - Refit RF on bootstrapped positives each replicate
#   - Predict mu on ALL rows, combine with fixed pi_BRT & "ecological" threshold (tau)
#   - Aggregate to site-date; summarise across replicates
# ==============================================================


# Load rasters from previous step (01_distribution_models.R)
rf_raster <- raster(file.path(dir_pred, "rf_prediction.asc"))  
brt_raster <- raster(file.path(dir_pred, "brt_prediction.asc"))
max_raster <- raster(file.path(dir_pred, "max_prediction.asc"))
glm_raster <- raster(file.path(dir_pred, "glm_prediction.asc"))
gam_raster <- raster(file.path(dir_pred, "gam_prediction.asc"))
ens_raster <- raster(file.path(dir_pred, "ens_prediction.asc"))

# Add predicted probability of presence info (extract by lon/lat
ab_data$rf_prediction <- raster::extract(rf_raster, ab_data[, c("lon","lat")])
ab_data$brt_prediction <- raster::extract(brt_raster, ab_data[, c("lon","lat")])
ab_data$max_prediction <- raster::extract(max_raster, ab_data[, c("lon","lat")])
ab_data$glm_prediction <- raster::extract(glm_raster, ab_data[, c("lon","lat")])
ab_data$gam_prediction <- raster::extract(gam_raster, ab_data[, c("lon","lat")])
ab_data$ens_prediction <- raster::extract(ens_raster, ab_data[, c("lon","lat")])


# Compute abundance for each model
ab_data$rf_abundance <- predict(RFa, ab_data, type = "response")
ab_data$brt_abundance <- predict(BRTa, ab_data, type = "response")
ab_data$glm_abundance <- predict(GLMa, ab_data, type = "response")
ab_data$gam_abundance <- predict(GAMa, ab_data, type = "response")
ab_data$ens_abundance <- weights_t["RFa"] * ab_data$rf_abundance +
  weights_t["BRTa"] * ab_data$brt_abundance +
  weights_t["GLMa"] * ab_data$glm_abundance +
  weights_t["GAMa"] * ab_data$gam_abundance


# Check
head(ab_data)


# Settings 
set.seed(24601)
B     <- 1000        # bootstrap replicates (adjust as you like)
tau   <- 0.25         # threshold for presence_class
dist_model <- "brt"
abund_model <- "rf"

# We keep pi fixed (from full-fit GLM raster extraction)
pi <- ab_data$brt_prediction
pres_mask_fixed <- as.integer(pi > tau)
n_all <- nrow(ab_data)

# Matrices to hold bootstrap predictions (per-row)
E_star_mat <- matrix(NA_real_, nrow = n_all, ncol = B)

# ----------- progress bar ----------------------------------------
use_cli <- requireNamespace("cli", quietly = TRUE) && interactive()

if (use_cli) {
  pb_id <- cli::cli_progress_bar("Bootstraps", total = B)
} else {
  step <- max(1L, floor(B / 20L))  # fallback: print every ~5%
  cat("Bootstraps: ")
}
# -----------------------------------------------------------------
for (b in seq_len(B)) {
  
  # Sample positives with replacement and fit RF
  idx_boot <- sample(seq_len(nrow(ab_data_pos)), replace = TRUE)
  tr_pos   <- ab_data_pos[idx_boot, , drop = FALSE]
  
  # Align factor levels between train positives and full prediction frame
  al <- align_factor_levels(tr_pos, ab_data, facs = c("year","season","month"))
  tr_pos_b <- al$train
  pred_df  <- al$pred
  
  # Fit RF (truncated counts are handled implicitly by training on positives only)
  RFa_b <- randomForest(
    count ~ year + season + month + ppa21 + tmaxm21 + tminm21 + rhm21 + elev + mang_rf_5km + water_occ + water_occ_99,
    data = tr_pos_b,
    ntree = 1500,
    mtry = 4,
    nodesize = 5,
    importance = FALSE,
    keep.forest = TRUE
  )
  
  # likelihood function: given a value of r, gives the likelihood of the observed
  # count training data given a truncated Poisson process with uncertain lambda, 
  # with distribution lambda ~ Gamma(size = r, scale = predicted / r)
  # such that the mean of lambda is the out-of-bag random forest predicted values
  # This gives a truncated negative binomial distribution with size = r, mu = mu_b
  L = function(r) sum(dnbinom(tr_pos_b$count, size = r, mu = RFa_b$predicted, log = TRUE) - 
                        log(1 - dnbinom(0, size = r, mu = RFa_b$predicted))) # normalise for truncated distribution
  # now work out the maximum-likelihood value for r
  O = optimise(L, c(0,10), maximum = TRUE)
  r = O$maximum
  
  
  # Example plot of predicted vs count
  # with 95% CIs for observations
  
  # par(lwd = 2, cex = 1.5)
  # plot(RFa_b$predicted, tr_pos_b$count, log = 'xy', pch = 19,
  #      xlim = c(1, 200), ylim = c(0.1, 200),
  #      xlab = "Predicted", ylab = "Count")
  # abline(c(0,1))
  # for (i in 1:nrow(tr_pos_b)) {
  #   lambda = pmax(1e-2, # so it shows up on log plot
  #                 qnbinom(c(0.025, 0.975), size = r, mu = RFa_b$predicted[i]))
  #   arrows(x0 = RFa_b$predicted[i], y0 = lambda[1], y1 = lambda[2], 
  #          angle = 90, code = 3)
  # }
  
  # Calculate how much of the data is within the 95% CIs as sanity check
  #a1 = qnbinom(0.025, size = r, mu = RFa_b$predicted)
  #a2 = qnbinom(0.975, size = r, mu = RFa_b$predicted)
  #mean(tr_pos_b$count > a1 & tr_pos_b$count < a2) # including endpoints
  #mean(tr_pos_b$count > a1 & tr_pos_b$count < a2) # excluding
  
  # Predict μ on ALL rows (including zeros)
  mu_b <- as.numeric(predict(RFa_b, pred_df, type = "response"))
  # Apply gamma-distributed model error to predicted values
  # with r estimated from training data above
  mu_b = rgamma(length(pred_df$count), shape = r, scale = mu_b/r)
  
  # Combine presence-absence and abundance parts of hurdle
  E_star_mat[, b] <- (runif(length(pred_df$count)) < pi) * mu_b 
  
  # Combine with fixed π_GLM and threshold rule: E*_b
  #E_star_mat[, b] <- pres_mask_fixed * pi * mu_b
  # --- progress update that works in Positron ---
  if (use_cli) {
    cli::cli_progress_update(id = pb_id, inc = 1)
  } else if (b %% step == 0L || b == B) {
    cat(".")
    flush.console()
  }
}
if (use_cli) cli::cli_progress_done(id = pb_id) else cat(" done\n")



# ------------------------ Summaries ------------------------------

# Row metadata 
row_info <- ab_data |>
  dplyr::mutate(row_id = dplyr::row_number()) %>%
  dplyr::select(row_id, date, site, site2, count)

# (A) Per-row mean & 95% bands
row_summ <- as.data.frame(E_star_mat) |> 
  dplyr::mutate(row_id = seq_len(n_all)) |> 
  tidyr::pivot_longer(-row_id, names_to = "rep", values_to = "E_star") |> 
  dplyr::group_by(row_id) |> 
  dplyr::summarise(
    E_mean  = mean(E_star, na.rm = TRUE),
    E_lower = quantile(E_star, 0.025, na.rm = TRUE),
    E_upper = quantile(E_star, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::left_join(row_info, by = "row_id")


# (B) Proper site–date bands = quantiles of the replicate SUMS
E_long <- as.data.frame(E_star_mat) |> 
  dplyr::mutate(row_id = seq_len(n_all)) |> 
  tidyr::pivot_longer(-row_id, names_to = "rep", values_to = "E_star") |> 
  dplyr::mutate(rep = as.integer(gsub("^V","", rep))) |> 
  dplyr::left_join(row_info, by = "row_id")

# Sum across rows PER replicate, then take quantiles across replicates
pred_site_date <- E_long %>%
  dplyr::group_by(site2, date, rep) %>%
  dplyr::summarise(pred = sum(E_star, na.rm = TRUE), .groups = "drop") |> 
  dplyr::group_by(site2, date) |> 
  dplyr::summarise(
    pred_mean = mean(pred, na.rm = TRUE),
    lower     = quantile(pred, 0.025, na.rm = TRUE),
    upper     = quantile(pred, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# Observed totals by site–date to overlay
obs_site_date <- ab_data |> 
  dplyr::group_by(site2, date) |> 
  dplyr::summarise(observed = sum(count, na.rm = TRUE), .groups = "drop")

# Optional: pretty panel names
site_map <- c(
  K = "Kuranda",
  B = "Black Mountain Road",
  C = "Cairns",
  R = "Russell River",
  M = "Ninds Creek",
  E = "Eubenangee Swamp"
)
site_levels <- unname(site_map)

df_plot <- obs_site_date |> 
  dplyr::left_join(pred_site_date, by = c("site2","date")) |> 
  dplyr::mutate(
    Panel = factor(dplyr::recode(site2, !!!site_map), levels = site_levels)
  ) |> 
  dplyr::arrange(Panel, date)

## Save the table that backs the figure
# readr::write_csv(
#   df_plot %>% dplyr::select(site2, date, observed, pred_mean, lower, upper),
#   file.path(dir_tables, sprintf("bootstrap_brt_rf_t025_bands_by_site_date_B%d.csv", B))
# )


# Build a long table for lines/points (keep the ribbon data separate)
df_long <- df_plot |> 
  dplyr::filter(Panel != "Eubenangee Swamp") |> 
  dplyr::select(Panel, date, observed, pred_mean, lower, upper) |> 
  tidyr::pivot_longer(c(observed, pred_mean),
                      names_to = "series", values_to = "value") |> 
  dplyr::mutate(
    series = factor(series,
                    levels = c("observed","pred_mean"),
                    labels = c("Observed","Predicted"))
  )

# Multi-panel plot (3x2) with legend & square markers for Predicted
p <- ggplot() +
  geom_ribbon(
    data = unique(df_long[, c("Panel","date","lower","upper")]),
    aes(x = date, ymin = lower, ymax = upper),
    inherit.aes = FALSE, 
    fill = "blue2", 
    alpha = 0.075
  ) +
  geom_line(
    data = df_long,
    aes(x = date, y = value, color = series, linetype = series),
    linewidth = 0.25
  ) +
  geom_point(
    data = df_long,
    aes(x = date, y = value, color = series, shape = series),
    size = 1.0
  ) +
  facet_wrap2(~ Panel, ncol = 2, scales = "free_y") +
  scale_x_date(
    date_breaks = "2 months", 
    date_labels = "%b-%y"
  ) +
  scale_color_manual(
    values = c("Observed" = "black", "Predicted" = "blue2"), 
    name = ""
  ) +
  scale_linetype_manual(
    values = c("Observed" = 1, "Predicted" = 2), 
    name = ""
  ) +
  scale_shape_manual(
    values = c("Observed" = 16, "Predicted" = 15),
    name = ""
  ) +
  labs(
    x = "Date", y = "Total trap counts"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.title.x  = element_text(size = 14, margin = ggplot2::margin(t = 6)),
    axis.title.y  = element_text(size = 14, margin = ggplot2::margin(r = 6)),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y   = element_text(size = 10),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    strip.text        = element_text(size = 12.5, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position   = "bottom"
  ) +
  facetted_pos_scales(
    y = list(
      Panel == "Cairns"            ~ scale_y_continuous(limits = c(0, 500)),
      Panel == "Kuranda"           ~ scale_y_continuous(limits = c(0, 150)),
      Panel == "Black Mountain Road" ~ scale_y_continuous(limits = c(0, 150)),
      Panel == "Russell River"     ~ scale_y_continuous(limits = c(0, 150)),
      Panel == "Ninds Creek"       ~ scale_y_continuous(limits = c(0, 150))
    )
  )

# Check
print(p)

# Savets_hurdle_glm_rf_thres0_reps_1000
ggplot2::ggsave(
  filename = file.path(dir_plots, paste0("ts_hurdle_",dist_model,"_",abund_model, "_tau_", tau, "_reps_", B,"_NickB.png")),
  plot = p, 
  width = 9.5, 
  height = 9.75, 
  dpi = 300, 
  bg = "white"
)
