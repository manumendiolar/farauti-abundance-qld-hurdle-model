############################################################
## RFa diagnostics – high-res PNG
## A: variable importance (most → least, top → bottom)
## B–E: partial dependence for top 4 predictors
## Response = predicted *counts* from RFa
############################################################

## First run 02_abund_models.R !

## Open device
png(file.path(dir_plots, "abund_model_diagnostics_RFa.png"),
    width  = 2000,
    height = 3000,
    res    = 300)

## Compute variable importance

# RF object for counts
#  - change RFa / data_abund if your objects have different names
rf_imp <- randomForest::importance(RFa, type = 1)

# training data used to fit RFa
dat <- ab_data_pos

rf_vi <- data.frame(
  variable = rownames(rf_imp),
  rel_inf  = rf_imp[, 1],
  row.names = NULL
)

# Keep only variables present in the data
rf_vi <- rf_vi[rf_vi$variable %in% names(dat), , drop = FALSE]

# Keep only numeric predictors for partial dependence
num_vars <- sapply(dat, is.numeric)
rf_vi <- rf_vi[rf_vi$variable %in% names(dat)[num_vars], , drop = FALSE]

# Order by decreasing importance and convert to %
rf_vi <- rf_vi[order(rf_vi$rel_inf, decreasing = TRUE), ]
rf_vi$rel_inf <- 100 * rf_vi$rel_inf / sum(rf_vi$rel_inf)


## LAYOUT: 5 PANELS --------------------------------------------------
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer margins: bottom, left, top, right


## PANEL A – VARIABLE IMPORTANCE ------------------------------------
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

## PANELS B–E – PARTIAL DEPENDENCE (PREDICTED COUNTS) ----------------
par(mar = c(4.5, 3, 3.5, 1))

n_panels      <- min(4, nrow(rf_vi))
top_vars      <- rf_vi$variable[1:n_panels]
top_inf       <- rf_vi$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

# function to compute PD on count scale for one variable
compute_pd_rfa <- function(var_name, grid.size = 50) {
  if (!is.numeric(dat[[var_name]])) {
    stop(paste("Variable", var_name, "is not numeric."))
  }

  x_vals <- seq(
    min(dat[[var_name]], na.rm = TRUE),
    max(dat[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  y_vals <- numeric(length(x_vals))

  for (i in seq_along(x_vals)) {
    newdat <- dat
    newdat[[var_name]] <- x_vals[i]

    p <- predict(RFa, newdat, type = "response")
    y_vals[i] <- mean(p, na.rm = TRUE)
  }

  data.frame(x = x_vals, y = y_vals)
}

plot_pd_panel_rfa <- function(var_name, rel_inf, panel_letter) {
  pd <- compute_pd_rfa(var_name, grid.size = 50)
  x  <- pd$x
  y  <- pd$y

  # nice y-limit with some headroom
  ylim_max <- max(y, na.rm = TRUE) * 1.05

  plot(
    x, y,
    type = "l",
    xlab = paste0(var_name, " (", round(rel_inf, 1), "%)"),
    ylab = "",
    main = paste0(panel_letter, "."),
    #ylim = c(0, ylim_max),
    cex.axis = 1.2,
    cex.lab  = 1.4,
    cex.main = 1.5
  )

  # Smoothed PD
  fit <- loess(y ~ x)
  lines(x, predict(fit), lty = 2, col = "red1")

  # Rug
  rug(dat[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_rfa(top_vars[i], top_inf[i], panel_letters[i])
}

# Shared y-axis label
mtext("Predicted trap counts",
      side  = 2,
      outer = TRUE,
      line  = 1.2,
      cex   = 1.1,
      adj   = 0.25)

## Close device
dev.off()
