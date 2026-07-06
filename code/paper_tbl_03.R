# ------------------------------------------------------------------------------
# TABLE 3
# Predictive performance of abundance models for positive \anff\ counts.
# Values are means from repeated stratified 10-fold cross-validation
# (10 folds, 5 repetitions). Reported metrics include Root Mean Squared Error
# (RMSE), Mean Absolute Error (MAE), and correlation coefficients (Pearson and
# Spearman) between observed and predicted values. Best value per metric in bold.
# ------------------------------------------------------------------------------


# Reads outputs/tables/count_model_kfold_10x5r.csv -- the raw count-scale CV
# metrics reported in the manuscript (Table 3), written by 02_abund_models.R.
# Optional: writes a LaTeX table.

infile  <- file.path("outputs", "tables", "count_model_kfold_10x5r.csv")
outfile <- file.path("outputs", "tables", "count_model_kfold_10x5r.tex")

df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE)

# (optional) order rows in a preferred sequence if present
ord <- c("RF","BRT","GLM","GAM","ENS")
df$Model <- factor(df$Model, levels = ord)
df <- df[order(df$Model), , drop = FALSE]

# Full model names for display (match the manuscript)
model_names <- c(
  RF  = "Random Forest",
  BRT = "Boosted Regression Trees",
  GLM = "Generalised Linear Model",
  GAM = "Generalised Additive Model",
  ENS = "Ensemble"
)

# format numbers to 2 decimals
fmt <- function(x) formatC(x, format = "f", digits = 2)

# Build display table: spell out model names and bold the best value per metric.
# Direction matters: RMSE/MAE are lower-is-better, Pearson/Spearman higher-is-
# better. Compare on the rounded 2-dp value so display ties both bold.
metric_dir <- c(RMSE = "min", MAE = "min", Pearson = "max", Spearman = "max")
disp <- df
for (mc in names(metric_dir)) {
  vals   <- round(as.numeric(df[[mc]]), 2)
  target <- if (metric_dir[[mc]] == "min") min(vals, na.rm = TRUE) else max(vals, na.rm = TRUE)
  cell   <- fmt(as.numeric(df[[mc]]))
  hit    <- !is.na(vals) & vals == target
  cell[hit] <- paste0("\\textbf{", cell[hit], "}")
  disp[[mc]] <- cell
}
disp$Model <- ifelse(is.na(model_names[as.character(df$Model)]),
                     as.character(df$Model), model_names[as.character(df$Model)])

# build LaTeX rows
rows <- apply(disp, 1, function(r)
  sprintf("%s & %s & %s & %s & %s \\\\",
          r["Model"], r["RMSE"], r["MAE"], r["Pearson"], r["Spearman"]))

latex <- paste0(
"\\begin{table}[H]\n",
"    \\centering\n",
"    \\caption{Predictive performance of abundance models for positive \\anff\\ counts. Values are means from repeated stratified 10--fold cross-validation (10 folds, 5 repetitions). Reported metrics include Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), and correlation coefficients (Pearson and Spearman) between observed and predicted values. Best value per metric in bold.}\n",
"    \\label{tab:kfold_repeated_4_metrics_binary_models}\n",
"    \\begin{threeparttable}\n",
"    \\rowcolors{1}{}{white}\n",
"    \\begin{tabularx}{0.75\\textwidth}{l*{5}{>{\\centering\\arraybackslash}X}}\n",
"        \\toprule\n",
"        \\textbf{Model} & \\textbf{RMSE} & \\textbf{MAE} & \\textbf{Pearson} & \\textbf{Spearman} \\\\\n",
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
