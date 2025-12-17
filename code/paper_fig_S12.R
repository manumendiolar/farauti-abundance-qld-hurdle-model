############################################################
## GAM (distribution) diagnostics – high-res PNG
## A: test-stat-based importance (summary(GAM), %)
## B–E: main effects for top 4 numeric predictors
## Y-axis = Probability of presence (GAM)
## With 95% confidence bands
############################################################

## First run 01_dist_models.R !

# Open device
png(file.path(dir_plots, "dist_model_diagnostics_GAM.png"),
    width  = 2000,
    height = 3000,
    res    = 300)


# Compute variable importance
sg <- summary(GAM)

# Parametric terms (e.g. year, season, month)
p_tab <- sg$p.table
if (!is.null(p_tab)) {
  p_tab <- p_tab[rownames(p_tab) != "(Intercept)", , drop = FALSE]
  stat_p <- (p_tab[, "z value"])^2        # t^2 as importance
  vi_param <- data.frame(
    variable = rownames(p_tab),
    stat     = as.numeric(stat_p),
    row.names = NULL
  )
} else {
  vi_param <- data.frame(variable = character(0), stat = numeric(0))
}

# Smooth terms s(x)
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

vi_gam <- rbind(vi_param, vi_smooth)

# clean & rescale
vi_gam <- vi_gam[!is.na(vi_gam$stat), , drop = FALSE]
vi_gam <- vi_gam[order(vi_gam$stat, decreasing = TRUE), ]
vi_gam$rel_inf <- 100 * vi_gam$stat / sum(vi_gam$stat)


# Layout ------------------------------------------------------------
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer margins for shared y label


# PANEL A – IMPORTANCE BARPLOT -------------------------------------
par(mar = c(5, 10, 4, 2))

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(vi_gam))

barplot(
  height    = rev(vi_gam$rel_inf),
  names.arg = rev(vi_gam$variable),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(vi_gam$rel_inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)


## PANELS B–E – MAIN EFFECTS WITH 95% CI -----------------------------
par(mar = c(4.5, 3, 3.5, 1))

# numeric predictors only (and not the response)
numeric_vars <- names(data)[sapply(data, is.numeric)]
numeric_vars <- setdiff(numeric_vars, "presence")

vi_gam_num <- vi_gam[vi_gam$variable %in% numeric_vars, , drop = FALSE]
vi_gam_num <- vi_gam_num[order(vi_gam_num$rel_inf, decreasing = TRUE), ]

n_panels <- min(4, nrow(vi_gam_num))
top_vars <- vi_gam_num$variable[1:n_panels]
top_inf  <- vi_gam_num$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

## Build a "typical" covariate profile (others held constant) ----------

base_prof <- data[1, , drop = FALSE]
for (nm in names(base_prof)) {
  if (nm == "presence") next
  if (is.numeric(data[[nm]])) {
    base_prof[[nm]] <- mean(data[[nm]], na.rm = TRUE)
  } else if (is.factor(data[[nm]]) || is.character(data[[nm]])) {
    tab <- table(data[[nm]])
    base_prof[[nm]] <- names(tab)[which.max(tab)]
  }
}

## Helper: PD + 95% CI on probability scale -----------------------------
compute_pd_gam_ci <- function(var_name, grid.size = 50) {
  x_vals <- seq(
    min(data[[var_name]], na.rm = TRUE),
    max(data[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  newdat <- base_prof[rep(1, grid.size), , drop = FALSE]
  newdat[[var_name]] <- x_vals

  pr <- predict(GAM, newdata = newdat, type = "link", se.fit = TRUE)
  eta  <- pr$fit
  se   <- pr$se.fit

  eta_lo <- eta - 1.96 * se
  eta_hi <- eta + 1.96 * se

  p_hat <- plogis(eta)
  p_lo  <- plogis(eta_lo)
  p_hi  <- plogis(eta_hi)

  data.frame(
    x    = x_vals,
    p    = p_hat,
    p_lo = p_lo,
    p_hi = p_hi
  )
}

plot_pd_panel_gam_ci <- function(var_name, rel_inf, panel_letter) {
  pd <- compute_pd_gam_ci(var_name, grid.size = 60)
  x    <- pd$x
  p    <- pd$p
  p_lo <- pd$p_lo
  p_hi <- pd$p_hi

  # empty plot
  plot(
    x, p,
    type = "n",
    xlab = paste0(var_name, " (", round(rel_inf, 1), "%)"),
    ylab = "",
    main = paste0(panel_letter, "."),
    ylim = c(0, 1),
    cex.axis = 1.2,
    cex.lab  = 1.4,
    cex.main = 1.5
  )

  # shaded CI band
  polygon(
    x = c(x, rev(x)),
    y = c(p_lo, rev(p_hi)),
    border = NA,
    col = adjustcolor("grey80", alpha.f = 0.7)
  )

  # fitted line
  lines(x, p, lwd = 1.5)

  # rug of observed data
  rug(data[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_gam_ci(top_vars[i], top_inf[i], panel_letters[i])
}

# shared y-axis label
mtext("Suitability",
      side  = 2,
      outer = TRUE,
      line  = 1.2,
      cex   = 1,
      adj   = 0.28)


## Close device
dev.off()
