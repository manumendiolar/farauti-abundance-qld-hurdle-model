############################################################
## BRT diagnostics figure – high-res PNG
## A: variable importance (most → least, top → bottom)
## B–G: partial dependence for top 6 predictors
## Single shared y-label for B–G
############################################################

# Setup / Auxiliary functions 
source(here::here("code","00_setup.R"))

# First run 01_dist_models.R !

# Open device 
png(file.path(dir_plots, "dist_model_diagnostics_BRT.png"),
    width  = 2000,
    height = 3000,
    res    = 300)

# Compute variable importance
brt_sum <- summary(BRT,
                   n.trees = BRT$n.trees,
                   plotit  = FALSE)
# Order by decreasing influence
brt_sum <- brt_sum[order(brt_sum$rel.inf, decreasing = TRUE), ]
brt_sum$rel.inf <- 100 * brt_sum$rel.inf / sum(brt_sum$rel.inf)


# Layout: 5 panels
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))  # first row taller than the others

# Outer margins so we can put a shared y label
par(oma = c(0, 4, 0, 0))  # outer: bottom, left, top, right

# PANEL A – VARIABLE IMPORTANCE ------------------------------------
par(mar = c(5, 10, 4, 2))  # inner: bottom, left, top, right

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(brt_sum))

# Reverse order so most important is at the TOP
barplot(
  height    = rev(brt_sum$rel.inf),
  names.arg = rev(brt_sum$var),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),   # darkest at the top (most important)
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(brt_sum$rel.inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)

# PANELS B–E – PARTIAL DEPENDENCE ----------------------------------
par(mar = c(4.5, 3, 3.5, 1))  # a bit smaller left margin (shared label)

top_vars <- brt_sum$var[1:4]
top_inf  <- brt_sum$rel.inf[1:4]
panel_letters <- c("B", "C", "D", "E")

plot_pd_panel <- function(var_name, rel_inf, panel_letter) {
  pd <- plot(
    BRT,
    i.var       = var_name,
    n.trees     = BRT$n.trees,
    type        = "response",
    return.grid = TRUE
  )

  x <- pd[[var_name]]
  y <- pd$y

  # Raw PD (no individual y-label)
  plot(
    x, y,
    type = "l",
    xlab = paste0(var_name, " (", round(rel_inf, 1), "%)"),
    ylab = "",                  # no label here
    main = paste0(panel_letter, "."),
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
  plot_pd_panel(top_vars[i], top_inf[i], panel_letters[i])
}

# Shared y-axis label for all PD panels
mtext("Suitability",
      side  = 2,       # left side
      outer = TRUE,
      line  = 1.2,
      cex   = 1,
      adj   = 0.28)


# Close device
dev.off()