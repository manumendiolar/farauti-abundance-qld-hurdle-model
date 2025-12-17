############################################################
## RF diagnostics figure – high-res PNG
## A: variable importance (most → least, top → bottom)
## B–E: partial dependence for top 4 predictors
## Single shared y-label for B–E
## Y-axis = predicted probability of presence (0–1)
############################################################

# Setup / Auxiliary functions 
source(here::here("code","00_setup.R"))

# First run 01_dist_models.R !


# Open device 
png(file.path(dir_plots, "dist_model_diagnostics_RF.png"),
    width  = 2000,
    height = 3000,
    res    = 300)

# Compute variable importance
rf_imp <- randomForest::importance(RF, type = 1)

# Save as data.frame
rf_vi <- data.frame(
  variable = rownames(rf_imp),
  rel_inf  = rf_imp[, 1],
  row.names = NULL
)

# Keep only variables that are actually columns in `data`
rf_vi <- rf_vi[rf_vi$variable %in% names(data), , drop = FALSE]

# Order by decreasing importance and convert to %
rf_vi <- rf_vi[order(rf_vi$rel_inf, decreasing = TRUE), ]
rf_vi$rel_inf <- 100 * rf_vi$rel_inf / sum(rf_vi$rel_inf)

# Layout: 5 panels
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer: bottom, left, top, right



# PANEL A – VARIABLE IMPORTANCE ------------------------------------
par(mar = c(5, 10, 4, 2))

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(rf_vi))

barplot(
  height    = rev(rf_vi$rel_inf),
  names.arg = rev(rf_vi$variable),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(rf_vi$rel_inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)

# PANELS B--E – PARTIAL DEPENDENCE -----------------------------------
par(mar = c(4.5, 3, 3.5, 1))

n_panels <- min(4, nrow(rf_vi))
top_vars <- rf_vi$variable[1:n_panels]
top_inf  <- rf_vi$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

# figure out positive class if RF is classification
prob_cols <- colnames(predict(RF, data, type = "prob"))
pos_class <- prob_cols[2]   # change to e.g. "presence" if needed

# function to compute PD on probability scale for one variable
compute_pd_rf <- function(var_name, grid.size = 50) {
  x_vals <- seq(
    min(data[[var_name]], na.rm = TRUE),
    max(data[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  y_vals <- numeric(length(x_vals))

  for (i in seq_along(x_vals)) {
    newdat <- data
    newdat[[var_name]] <- x_vals[i]

    if (RF$type == "classification") {
      p <- predict(RF, newdat, type = "prob")[, pos_class]
    } else {  # regression 0/1
      p <- predict(RF, newdat, type = "response")
    }

    y_vals[i] <- mean(p, na.rm = TRUE)
  }

  data.frame(x = x_vals, y = y_vals)
}

plot_pd_panel_rf <- function(var_name, rel_inf, panel_letter) {
  pd <- compute_pd_rf(var_name, grid.size = 50)
  x <- pd$x
  y <- pd$y

  plot(
    x, y,
    type = "l",
    xlab = paste0(var_name, " (", round(rel_inf, 1), "%)"),
    ylab = "",
    main = paste0(panel_letter, "."),
    #ylim = c(0, 1),
    cex.axis = 1.2,
    cex.lab  = 1.4,
    cex.main = 1.5
  )
  # Smoothed PD
  fit <- loess(y ~ x)
  lines(x, predict(fit), lty = 2, col = "red2")

  # Rug
  rug(data[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_rf(top_vars[i], top_inf[i], panel_letters[i])
}

# Shared y-axis label
mtext("Suitability",
      side  = 2,
      outer = TRUE,
      line  = 1.2,
      cex   = 1,
      adj   = 0.28)

# Close device
dev.off()
