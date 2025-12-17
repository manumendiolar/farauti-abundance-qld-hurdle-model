############################################################
## GLM (distribution) diagnostics – high-res PNG
## A: deviance-based importance (drop1 LRT, %)
## B–E: main effects for top 4 numeric predictors
## Y-axis = Probability of presence (GLM)
## With 95% confidence bands instead of loess smooth
############################################################

## First run 01_dist_models.R !

# Open device
png(file.path(dir_plots, "dist_model_diagnostics_GLM.png"),
    width  = 2000,
    height = 3000,
    res    = 300)

# Compute variable importance via drop1
dd <- drop1(GLM, test = "Chisq")   # likelihood ratio tests
dd <- dd[rownames(dd) != "<none>", , drop = FALSE]

vi_glm <- data.frame(
  variable = rownames(dd),
  stat     = dd$LRT,
  row.names = NULL
)

# remove any NA stats
vi_glm <- vi_glm[!is.na(vi_glm$stat), , drop = FALSE]

# rescale to %
vi_glm <- vi_glm[order(vi_glm$stat, decreasing = TRUE), ]
vi_glm$rel_inf <- 100 * vi_glm$stat / sum(vi_glm$stat)


# Layout
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer margins (for shared y label)


# PANEL A – IMPORTANCE BARPLOT -------------------------------------
par(mar = c(5, 10, 4, 2))

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(vi_glm))

barplot(
  height    = rev(vi_glm$rel_inf),
  names.arg = rev(vi_glm$variable),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(vi_glm$rel_inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)

# PANELS B–E – MAIN EFFECTS WITH 95% CI -----------------------------
par(mar = c(4.5, 3, 3.5, 1))

# numeric predictors only (and not the response)
numeric_vars <- names(data)[sapply(data, is.numeric)]
numeric_vars <- setdiff(numeric_vars, "presence")

vi_glm_num <- vi_glm[vi_glm$variable %in% numeric_vars, , drop = FALSE]
vi_glm_num <- vi_glm_num[order(vi_glm_num$rel_inf, decreasing = TRUE), ]

n_panels <- min(4, nrow(vi_glm_num))
top_vars <- vi_glm_num$variable[1:n_panels]
top_inf  <- vi_glm_num$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

## Build a "typical" covariate profile for other predictors -------------
# We'll hold all other variables at typical values (mean for numeric,
# most common level for factors). This makes the CI computation clean
# because we're predicting for one representative profile per x value.
base_prof <- data[1, , drop = FALSE]  # template row
for (nm in names(base_prof)) {
  if (nm == "presence") next
  if (is.numeric(data[[nm]])) {
    base_prof[[nm]] <- mean(data[[nm]], na.rm = TRUE)
  } else if (is.factor(data[[nm]]) || is.character(data[[nm]])) {
    tab <- table(data[[nm]])
    base_prof[[nm]] <- names(tab)[which.max(tab)]
  }
}

## Helper: compute PD + 95% CI on probability scale ---------------------
compute_pd_glm_ci <- function(var_name, grid.size = 50) {
  x_vals <- seq(
    min(data[[var_name]], na.rm = TRUE),
    max(data[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  # build newdata where only var_name varies, others fixed at base_prof
  newdat <- base_prof[rep(1, grid.size), , drop = FALSE]
  newdat[[var_name]] <- x_vals

  # predict on link scale with standard errors
  pr <- predict(GLM, newdata = newdat, type = "link", se.fit = TRUE)
  eta  <- pr$fit
  se   <- pr$se.fit

  # 95% CI on link scale
  eta_lo <- eta - 1.96 * se
  eta_hi <- eta + 1.96 * se

  # transform to probability scale
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

plot_pd_panel_glm_ci <- function(var_name, rel_inf, panel_letter) {
  pd <- compute_pd_glm_ci(var_name, grid.size = 60)
  x    <- pd$x
  p    <- pd$p
  p_lo <- pd$p_lo
  p_hi <- pd$p_hi

  # empty plot first
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

  # confidence band (shaded)
  polygon(
    x = c(x, rev(x)),
    y = c(p_lo, rev(p_hi)),
    border = NA,
    col = adjustcolor("grey80", alpha.f = 0.5)
  )

  # main effect line
  lines(x, p, lwd = 1.5)

  # rug of observed data
  rug(data[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_glm_ci(top_vars[i], top_inf[i], panel_letters[i])
}

# shared y-axis label
mtext("Suitability",
      side  = 2,
      outer = TRUE,
      line  = 1.2,
      cex   = 1,
      adj   = 0.28)


# Close device
dev.off()
