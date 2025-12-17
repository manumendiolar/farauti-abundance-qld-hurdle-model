############################################################
## MaxEnt diagnostics figure – high-res PNG
## A: variable contribution (most → least, top → bottom)
## B–E: response curves for top 4 predictors
## Y-axis = MaxEnt logistic output (0–1)
############################################################

## First run 01_dist_models.R !

# Use both presence and background env data
env_data <- rbind(MAX@presence, MAX@absence)

# Open device
png(file.path(dir_plots, "dist_model_diagnostics_MAX.png"),
    width  = 2000,
    height = 3000,
    res    = 300)

# Compute variable importance (% contribution)
res <- MAX@results   
# rows with ".contribution" in the name
idx_contrib <- grepl("contribution", rownames(res), ignore.case = TRUE)
var_names <- sub(".contribution", "", rownames(res)[idx_contrib])
contrib   <- res[idx_contrib, 1]
me_vi <- data.frame(
  variable = var_names,
  rel_inf  = as.numeric(contrib),
  row.names = NULL
)
# keep only vars that are columns in env_data
me_vi <- me_vi[me_vi$variable %in% names(env_data), , drop = FALSE]
# order & rescale to 100%
me_vi <- me_vi[order(me_vi$rel_inf, decreasing = TRUE), ]
me_vi$rel_inf <- 100 * me_vi$rel_inf / sum(me_vi$rel_inf)


# Layout: 5 panels --------------------------------------------------
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer margins: bottom, left, top, right


# PANEL A – VARIABLE CONTRIBUTION ----------------------------------
par(mar = c(5, 10, 4, 2))

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(me_vi))

barplot(
  height    = rev(me_vi$rel_inf),
  names.arg = rev(me_vi$variable),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(me_vi$rel_inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)

# PANELS B–E – RESPONSE CURVES -------------------
par(mar = c(4.5, 3, 3.5, 1))

n_panels <- min(4, nrow(me_vi))
top_vars <- me_vi$variable[1:n_panels]
top_inf  <- me_vi$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

# helper: partial dependence on logistic scale for one variable
compute_pd_maxent <- function(maxent_obj, env_df, var_name, grid.size = 50) {
  x_vals <- seq(
    min(env_df[[var_name]], na.rm = TRUE),
    max(env_df[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  y_vals <- numeric(length(x_vals))

  for (i in seq_along(x_vals)) {
    newdat <- env_df
    newdat[[var_name]] <- x_vals[i]

    # logistic output; args ensures we get logistic, not e.g. cloglog
    p <- predict(maxent_obj, newdat,
                 args = c("outputformat=logistic"),
                 type = "response")

    y_vals[i] <- mean(p, na.rm = TRUE)
  }

  data.frame(x = x_vals, y = y_vals)
}

plot_pd_panel_maxent <- function(var_name, rel_inf, panel_letter) {
  pd <- compute_pd_maxent(MAX, env_data, var_name, grid.size = 50)
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
  # rug showing data distribution
  rug(env_data[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_maxent(top_vars[i], top_inf[i], panel_letters[i])
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
