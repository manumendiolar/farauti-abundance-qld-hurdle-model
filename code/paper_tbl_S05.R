# ------------------------------------------------------------------------------
# TABLE S05 (SUPP MAT.)
# Trap-efficiency estimates and propagated abundance sensitivity. Season-year
# recapture proportions (%) were computed from the mark-release-recapture
# experiments of Chow et al. (2025). Each season's efficiency is summarised as a
# mean with a 90% confidence interval (90% CI) across the three replicates. The
# abundance band gives the multiplicative change in absolute abundance implied
# by the interval bounds. The within-season rank order of grid cells is
# invariant to this rescaling by construction, and the period-averaged hotspots
# ranking was essentially unchanged.
#
# This mirrors the efficiency mean/CI computation in
# trap_efficiency_sensitivity_analysis.R, but lays the values out by season for
# the supplement. Writes a LaTeX table.
# ------------------------------------------------------------------------------


# Optional: writes a LaTeX table.
outfile <- file.path("outputs", "tables", "trap_efficiency_table.tex")

# Per season-year recapture rates (%) from Chow et al. (2025), their Table 2.
# Columns are the 2015, 2016, 2017 replicates within each season.
wet_obs <- c(`2015` = 1.165571, `2016` = 0.800337, `2017` = 1.728723)
dry_obs <- c(`2015` = 1.261261, `2016` = 2.194762, `2017` = 1.525054)

# Mean + 90% CI across the three replicates (arithmetic mean, raw scale),
# matching trap_efficiency_sensitivity_analysis.R.
eff_ci <- function(x, level = 0.90) {
  n  <- length(x)
  t  <- qt(1 - (1 - level) / 2, df = n - 1)
  m  <- mean(x)
  se <- sd(x) / sqrt(n)
  c(lower = m - t * se, mean = m, upper = m + t * se)
}

e_wet <- eff_ci(wet_obs)
e_dry <- eff_ci(dry_obs)

# Abundance band = central efficiency / CI bounds. Abundance = counts / efficiency,
# so the upper efficiency bound gives the lower abundance and vice versa.
band <- function(e) c(low = e[["mean"]] / e[["upper"]], high = e[["mean"]] / e[["lower"]])
b_wet <- band(e_wet)
b_dry <- band(e_dry)

# format numbers to 2 decimals
fmt <- function(x) formatC(x, format = "f", digits = 2)
rng <- function(lo, hi) sprintf("%s--%s", fmt(lo), fmt(hi))

# build LaTeX rows (one per season)
row_season <- function(name, obs, e, b)
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
          name,
          fmt(obs[["2015"]]), fmt(obs[["2016"]]), fmt(obs[["2017"]]),
          fmt(e[["mean"]]),
          rng(e[["lower"]], e[["upper"]]),
          rng(b[["low"]],   b[["high"]]))

rows <- c(
  row_season("Wet", wet_obs, e_wet, b_wet),
  row_season("Dry", dry_obs, e_dry, b_dry)
)

latex <- paste0(
"\\begin{table}[htbp]\n",
"    \\centering\n",
"    \\caption{Trap-efficiency estimates and propagated abundance sensitivity. Season-year recapture proportions (\\%) were computed from the mark-release-recapture experiments of \\citet{chow_2025estimating}. Each season's efficiency is summarised as a mean with a 90\\% confidence interval (90\\% CI) across the three replicates. The abundance band gives the multiplicative change in absolute abundance implied by the interval bounds. The within-season rank order of grid cells is invariant to this rescaling by construction, and the period-averaged hotspots ranking was essentially unchanged.}\n",
"    \\label{tab:trap_efficiency}\n",
"    \\begin{threeparttable}\n",
"    \\rowcolors{1}{white}{white}\n",
"    \\begin{tabularx}{0.95\\textwidth}{l*{6}{>{\\centering\\arraybackslash}X}}\n",
"        \\toprule\n",
"        \\textbf{Season} & \\textbf{2015} & \\textbf{2016} & \\textbf{2017} & \\textbf{Mean} & \\textbf{90\\% CI} & \\textbf{Abund. band} \\\\\n",
"        \\midrule\n",
"        ", paste(rows, collapse = "\n        "), "\n",
"        \\bottomrule\n",
"    \\end{tabularx}\n",
"    \\end{threeparttable}\n",
"\\end{table}\n"
)

# Print to console
cat(latex)

# Write file
writeLines(latex, outfile)
