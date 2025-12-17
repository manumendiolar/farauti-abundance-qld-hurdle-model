############################################################
## GAMa (abundance, positive counts) diagnostics – PNG
## A: test-stat-based importance (summary(GAMa), %)
## B–E: main effects for top 4 numeric predictors
## Y-axis = Expected count (given presence, GAMa)
## With 95% confidence bands
############################################################

## First run 02_abund_models.R !

## Open device
png(file.path(dir_plots, "abund_model_diagnostics_GAMa.png"),
    width  = 2000,
    height = 3000,
    res    = 300)


## VARIABLE IMPORTANCE FROM summary(GAMa) ----------------------------
sg <- summary(GAMa)

## Parametric terms (e.g. year, season, month)
p_tab <- sg$p.table
if (!is.null(p_tab)) {
  # Drop intercept
  p_tab <- p_tab[rownames(p_tab) != "(Intercept)", , drop = FALSE]

  # Column name may be "t value" or "z value" depending on family / fit
  stat_col <- intersect(colnames(p_tab), c("z value", "t value"))[1]
  stat_p   <- (p_tab[, stat_col])^2   # square of test statistic as importance

  vi_param <- data.frame(
    variable = rownames(p_tab),
    stat     = as.numeric(stat_p),
    row.names = NULL
  )
} else {
  vi_param <- data.frame(variable = character(0), stat = numeric(0))
}

## Smooth terms s(x)
s_tab <- sg$s.table
if (!is.null(s_tab)) {
  col_stat <- if ("Chi.sq" %in% colnames(s_tab)) "Chi.sq" else "F"
  stat_s   <- s_tab[, col_stat]
  var_s    <- gsub("^s\\((.*)\\)$", "\\1", rownames(s_tab))

  vi_smooth <- data.frame(
    variable = var_s,
    stat     = as.numeric(stat_s),
    row.names = NULL
  )
} else {
  vi_smooth <- data.frame(variable = character(0), stat = numeric(0))
}

## Combine parametric + smooth, rescale to %
vi_gama <- rbind(vi_param, vi_smooth)
vi_gama <- vi_gama[!is.na(vi_gama$stat), , drop = FALSE]
vi_gama <- vi_gama[order(vi_gama$stat, decreasing = TRUE), ]
vi_gama$rel_inf <- 100 * vi_gama$stat / sum(vi_gama$stat)


## LAYOUT ------------------------------------------------------------
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.8, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer margins for shared y label


## PANEL A – IMPORTANCE BARPLOT -------------------------------------
par(mar = c(5, 10, 4, 2))

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(vi_gama))

barplot(
  height    = rev(vi_gama$rel_inf),
  names.arg = rev(vi_gama$variable),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(vi_gama$rel_inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)

## PANELS B–E – MAIN EFFECTS WITH 95% CI (EXPECTED COUNT) -----------
par(mar = c(4.5, 3, 3.5, 1))

# numeric predictors only (and not the response)
numeric_vars <- names(ab_data_pos)[sapply(ab_data_pos, is.numeric)]
numeric_vars <- setdiff(numeric_vars, "count")

vi_gama_num <- vi_gama[vi_gama$variable %in% numeric_vars, , drop = FALSE]
vi_gama_num <- vi_gama_num[order(vi_gama_num$rel_inf, decreasing = TRUE), ]

n_panels <- min(4, nrow(vi_gama_num))
top_vars <- vi_gama_num$variable[1:n_panels]
top_inf  <- vi_gama_num$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

## Build a "typical" covariate profile (others held constant) ----------

base_prof <- ab_data_pos[1, , drop = FALSE]
for (nm in names(base_prof)) {
  if (nm == "count") next
  if (is.numeric(ab_data_pos[[nm]])) {
    base_prof[[nm]] <- mean(ab_data_pos[[nm]], na.rm = TRUE)
  } else if (is.factor(ab_data_pos[[nm]]) || is.character(ab_data_pos[[nm]])) {
    tab <- table(ab_data_pos[[nm]])
    base_prof[[nm]] <- names(tab)[which.max(tab)]
  }
}

## Helper: PD + 95% CI on expected-count scale -------------------------
compute_pd_gama_ci <- function(var_name, grid.size = 50) {
  x_vals <- seq(
    min(ab_data_pos[[var_name]], na.rm = TRUE),
    max(ab_data_pos[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  newdat <- base_prof[rep(1, grid.size), , drop = FALSE]
  newdat[[var_name]] <- x_vals

  # Predict on link scale with SE
  pr   <- predict(GAMa, newdata = newdat, type = "link", se.fit = TRUE)
  eta  <- pr$fit
  se   <- pr$se.fit

  # 95% CI on link scale
  eta_lo <- eta - 1.96 * se
  eta_hi <- eta + 1.96 * se

  # Back-transform to expected count
  mu_hat <- exp(eta)
  mu_lo  <- exp(eta_lo)
  mu_hi  <- exp(eta_hi)

  data.frame(
    x    = x_vals,
    mu   = mu_hat,
    mu_lo = mu_lo,
    mu_hi = mu_hi
  )
}

plot_pd_panel_gama_ci <- function(var_name, rel_inf, panel_letter) {
  pd   <- compute_pd_gama_ci(var_name, grid.size = 60)
  x    <- pd$x
  mu   <- pd$mu
  mu_lo <- pd$mu_lo
  mu_hi <- pd$mu_hi

  # Empty plot
  plot(
    x, mu,
    type = "n",
    xlab = paste0(var_name, " (", round(rel_inf, 1), "%)"),
    ylab = "",
    main = paste0(panel_letter, "."),
    # leave ylim flexible for counts, or set manually if you prefer
    # ylim = c(0, max(mu_hi) * 1.05),
    cex.axis = 1.2,
    cex.lab  = 1.4,
    cex.main = 1.5
  )

  # Shaded CI band
  polygon(
    x = c(x, rev(x)),
    y = c(mu_lo, rev(mu_hi)),
    border = NA,
    col = adjustcolor("grey80", alpha.f = 0.7)
  )

  # Fitted line
  lines(x, mu, lwd = 1.5)

  # Rug of observed data
  rug(ab_data_pos[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_gama_ci(top_vars[i], top_inf[i], panel_letters[i])
}

# Shared y-axis label
mtext("Predicted trap counts",
      side  = 2,
      outer = TRUE,
      line  = 1.2,
      cex   = 1,
      adj   = 0.25)


## Close device
dev.off()


# Quick check
#library(mgcv)
# Assuming 'model' is your fitted GAM object and 'x1', 'x2' are smooth terms
#plot(GAMa, select = 1) # Plots the first smooth term
#plot(model, select = 2) # Plots the second smooth term
# Or for all:
#plot(GAMa) # Plots all smooths, often in separate panels
