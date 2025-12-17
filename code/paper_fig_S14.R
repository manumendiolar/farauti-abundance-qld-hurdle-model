############################################################
## BRTa diagnostics figure – high-res PNG
## A: variable importance (most → least, top → bottom)
## B–E: partial dependence for top 4 predictors
## Response = predicted mean trap count (regression)
############################################################

## First run 02_abund_models.R !

## Open device
png(file.path(dir_plots, "abund_model_diagnostics_BRTa.png"),
    width  = 2000,
    height = 3000,
    res    = 300)

## Make sure your abundance data frame is called `data_abund`
## (change this line if you use a different name)
data_abund <- ab_data_pos  # no-op; just a reminder

## VARIABLE IMPORTANCE
# summary.gbm returns a data frame with var + rel.inf
brt_imp_raw <- summary(BRTa, plotit = FALSE)

brt_vi <- data.frame(
  variable = brt_imp_raw$var,
  rel_inf  = brt_imp_raw$rel.inf,
  row.names = NULL
)

# Keep only variables that exist in the data and are numeric
numeric_vars <- names(data_abund)[sapply(data_abund, is.numeric)]
brt_vi <- brt_vi[brt_vi$variable %in% numeric_vars, , drop = FALSE]

# Order by decreasing importance and convert to %
brt_vi <- brt_vi[order(brt_vi$rel_inf, decreasing = TRUE), ]
brt_vi$rel_inf <- 100 * brt_vi$rel_inf / sum(brt_vi$rel_inf)


## LAYOUT: 5 PANELS -----------------------------
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer: bottom, left, top, right


## PANEL A – VARIABLE IMPORTANCE ----------------
par(mar = c(5, 10, 4, 2))

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(brt_vi))

barplot(
  height    = rev(brt_vi$rel_inf),
  names.arg = rev(brt_vi$variable),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(brt_vi$rel_inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)

## PANELS B–E – PARTIAL DEPENDENCE ---------------
par(mar = c(4.5, 3, 3.5, 1))

n_panels <- min(4, nrow(brt_vi))
top_vars <- brt_vi$variable[1:n_panels]
top_inf  <- brt_vi$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

# Function to compute PD on expected-count scale for one variable
compute_pd_brta <- function(var_name, grid.size = 50) {
  x_vals <- seq(
    min(data_abund[[var_name]], na.rm = TRUE),
    max(data_abund[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  y_vals <- numeric(length(x_vals))

  for (i in seq_along(x_vals)) {
    newdat <- data_abund
    newdat[[var_name]] <- x_vals[i]

    # BRTa is a regression model; type = "response" gives mean count
    mu <- predict(BRTa, newdata = newdat,
                  n.trees = BRTa$n.trees,
                  type    = "response")
    y_vals[i] <- mean(mu, na.rm = TRUE)
  }

  data.frame(x = x_vals, y = y_vals)
}

plot_pd_panel_brta <- function(var_name, rel_inf, panel_letter) {
  pd <- compute_pd_brta(var_name, grid.size = 50)
  x <- pd$x
  y <- pd$y

  plot(
    x, y,
    type = "l",
    xlab = paste0(var_name, " (", round(rel_inf, 1), "%)"),
    ylab = "",
    main = paste0(panel_letter, "."),
    cex.axis = 1.2,
    cex.lab  = 1.4,
    cex.main = 1.5
  )

  # Smoothed PD
  fit <- loess(y ~ x)
  lines(x, predict(fit), lty = 2, col = "red2")

  # Rug
  rug(data_abund[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_brta(top_vars[i], top_inf[i], panel_letters[i])
}

# Shared y-axis label
mtext("Predicted trap counts",
      side  = 2,
      outer = TRUE,
      line  = 1.2,
      cex   = 1,
      adj   = 0.25)

##  Close device
dev.off()
