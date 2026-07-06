# ==============================================================================
#
# TRAP-EFFICIENCY UNCERTAINTY PROPAGATION
#
# Goal:
#   The trap-count to abundance conversion divides predicted counts by a single
#   efficiency per season. That efficiency is estimated from three independent
#   season-year mark-release-recapture experiments (Chow et al. 2025), so it has
#   a mean and a confidence interval, not a single fixed value.
#
#   We summarise each season's efficiency as a mean + CI across the three
#   replicates, then propagate the lower, central and upper efficiency through
#   the conversion to obtain lower, central and upper abundance layers.
#
# Two things to report:
#   1. ABSOLUTE magnitude. Carrying the efficiency CI through gives an abundance
#      band. This is the trap-efficiency uncertainty the reviewer asked for.
#   2. RELATIVE pattern. The conversion divides every cell in a season by the
#      same number, so within a season it cannot change the rank order of cells.
#      The hotspot ranking is therefore invariant by construction; only the
#      absolute numbers move.
#
# Precondition:
#   `centroids` holds the hurdle trap-count prediction in column
#   "brt_rf_t025_abund" (despite the name it is a trap count), plus season (D/W).
# ==============================================================================

source(here::here("code", "00_setup.R"))

# ---- Load predictions --------------------------------------------------------
centroids <- fst::read_fst(
  file.path(dir_pred, "centroids_5x5_qld_with_predictions.fst")
) |>
  dplyr::mutate(
    date   = as.Date(date),
    season = as.factor(season)
  )

dist_model  <- "brt"
abund_model <- "rf"
tau         <- 0.25
col_name    <- paste0(dist_model, "_", abund_model, "_t", sprintf("%03d", tau * 100), "_abund")

centroids <- centroids |>
  dplyr::select(
    grid_id, lon, lat, date, season,
    trapcounts = dplyr::all_of(col_name)
  )

summary(centroids$trapcounts)

# ------------------------------------------------------------------------------
#                  Efficiency mean + CI across the three replicates
#
# Per season-year recapture rates (%) from Chow et al. (2025), their Table 2.
# From Chow et al. (2025): "Estimating the dispersal of the malaria vector 
# Anopheles farauti through a natural ecosystem in north Queensland, Australia 
# using mark release and recapture experiments"
#
# 
# Year-Season  |  Marked & Rel.  | Tot. Collected  | Marked Collected  | N 
# ______________________________________________________________________________
# 2015-wet     |  3346           | 16675           |  39               | 1430629                    
# 2015-dry     |  1110           | 17425           |  14               | 1381553
# 2016-wet     |  2374           | 17051           |  19               | 2130478    
# 2016-dry     |  5422           | 16997           | 119               |  774434
# 2017-wet     |   752           |  4287           |  13               |  247986
# 2017-dry     |  6885           |  2599           | 105               |  170420   
#
#
# Trap efficiency as % 
# 2015-wet 1.165571
# 2015-dry 1.261261
# 2016-wet 0.800337
# 2016-dry 2.194762
# 2017-wet 1.728723
# 2017-dry 1.525054
# ------------------------------------------------------------------------------
wet_obs <- c(1.165571, 0.800337, 1.728723) / 100   # 2015, 2016, 2017 wet
dry_obs <- c(1.261261, 2.194762, 1.525054) / 100   # 2015, 2016, 2017 dry

# Mean + CI across the replicates.
#   level     = 0.90 follows Roslyn's suggestion and keeps the arithmetic mean.
#   log_scale = TRUE is the conservative alternative (95%, geometric mean,
#               guarantees positive efficiency). Switch both to use it.
eff_ci <- function(x, level = 0.90, log_scale = FALSE) {
  n <- length(x)
  t <- qt(1 - (1 - level) / 2, df = n - 1)
  if (log_scale) {
    m  <- mean(log(x)); se <- sd(log(x)) / sqrt(n)
    c(lower = exp(m - t * se), mean = exp(m), upper = exp(m + t * se))
  } else {
    m  <- mean(x); se <- sd(x) / sqrt(n)
    c(lower = m - t * se, mean = m, upper = m + t * se)
  }
}

e_wet <- eff_ci(wet_obs)
e_dry <- eff_ci(dry_obs)

# Expected with the 90% raw default:
#   e_wet  lower 0.00443  mean 0.01232  upper 0.02020   (0.44% / 1.23% / 2.02%)
#   e_dry  lower 0.00849  mean 0.01660  upper 0.02472   (0.85% / 1.66% / 2.47%)
print(round(rbind(wet = e_wet, dry = e_dry) * 100, 3))

# ------------------------------------------------------------------------------
#                          Three abundance layers
#
# abundance = trapcounts / efficiency, applied per season.
# Direction matters and is the easy thing to get backwards:
#   - LOW  abundance comes from HIGH efficiency (the upper efficiency bound)
#   - HIGH abundance comes from LOW  efficiency (the lower efficiency bound)
# ------------------------------------------------------------------------------
to_abundance <- function(dat, eff_wet, eff_dry) {
  dat |>
    dplyr::mutate(
      abundance = dplyr::if_else(season == "W",
                                 trapcounts / eff_wet,
                                 trapcounts / eff_dry)
    )
}

abund_central <- to_abundance(centroids, e_wet["mean"],  e_dry["mean"])
abund_low     <- to_abundance(centroids, e_wet["upper"], e_dry["upper"])  # high eff
abund_high    <- to_abundance(centroids, e_wet["lower"], e_dry["lower"])  # low  eff

# ------------------------------------------------------------------------------
#         Period-averaged abundance per cell (the heatmap quantity)
# ------------------------------------------------------------------------------
cell_mean <- function(dat) {
  dat |>
    dplyr::group_by(grid_id, lon, lat) |>
    dplyr::summarise(mean_all = mean(abundance, na.rm = TRUE), .groups = "drop")
}

cell_central <- cell_mean(abund_central) |> dplyr::rename(central = mean_all)
cell_low     <- cell_mean(abund_low)     |> dplyr::rename(low     = mean_all)
cell_high    <- cell_mean(abund_high)    |> dplyr::rename(high    = mean_all)

cells <- cell_central |>
  dplyr::inner_join(cell_low,  by = c("grid_id", "lon", "lat")) |>
  dplyr::inner_join(cell_high, by = c("grid_id", "lon", "lat"))

# ------------------------------------------------------------------------------
#   1. ABSOLUTE band: how far the abundance numbers move under the efficiency CI
# ------------------------------------------------------------------------------
band <- cells |>
  dplyr::summarise(
    ratio_low_median  = median(low  / central, na.rm = TRUE),
    ratio_high_median = median(high / central, na.rm = TRUE)
  )
cat("\nAbsolute abundance band (median over cells, relative to central):\n")
print(round(band, 3))
# Expected (90% raw): ratio_low ~0.61, ratio_high ~2.78  -> roughly x0.6 to x2.8

# ------------------------------------------------------------------------------
#   2. RELATIVE pattern: does the hotspot ranking survive the efficiency CI?
#      Within season the answer is 1 by construction; we confirm it, and we
#      check the period-averaged map (which mixes both seasons).
# ------------------------------------------------------------------------------
spear <- function(a, b) suppressWarnings(
  cor(a, b, method = "spearman", use = "complete.obs")
)

ranking <- tibble::tibble(
  comparison = c("low vs central", "high vs central"),
  rho        = c(spear(cells$low, cells$central), spear(cells$high, cells$central))
)
cat("\nPeriod-averaged hotspot ranking vs central (Spearman):\n")
print(round(ranking, 4))

# Confirm the within-season invariance (should be exactly 1.0000 each season)
within_season <- abund_central |>
  dplyr::select(grid_id, date, season, ab_c = abundance) |>
  dplyr::inner_join(
    abund_low |> dplyr::select(grid_id, date, ab_low = abundance),
    by = c("grid_id", "date")
  ) |>
  dplyr::group_by(season) |>
  dplyr::summarise(rho = spear(ab_c, ab_low), .groups = "drop")
cat("\nWithin-season ranking (should be 1 by construction):\n")
print(round(within_season, 4))

# period-averaged ranking
ranking |> dplyr::mutate(rho = round(rho, 4)) |> print()

# within-season ranking
within_season |> dplyr::mutate(rho = round(rho, 4)) |> print()

# ------------------------------------------------------------------------------
#   Tidy summary table for the supplement
# ------------------------------------------------------------------------------
summary_tbl <- tibble::tibble(
  layer        = c("lower (high efficiency)", "central (mean efficiency)", "upper (low efficiency)"),
  eff_wet_pct  = round(c(e_wet["upper"], e_wet["mean"], e_wet["lower"]) * 100, 3),
  eff_dry_pct  = round(c(e_dry["upper"], e_dry["mean"], e_dry["lower"]) * 100, 3),
  median_ratio_vs_central = round(c(band$ratio_low_median, 1, band$ratio_high_median), 3)
)
# period-averaged ranking
ranking |> dplyr::mutate(rho = round(rho, 4)) |> print()

# within-season ranking
within_season |> dplyr::mutate(rho = round(rho, 4)) |> print()

cat("\n================ TRAP-EFFICIENCY PROPAGATION SUMMARY ================\n")
print(summary_tbl)

readr::write_csv(summary_tbl, file.path(dir_pred, "trap_efficiency_ci_summary.csv"))

# Per-cell central/low/high (for optional bound maps in your usual plotting code)
readr::write_csv(cells, file.path(dir_pred, "abundance_central_low_high.csv"))