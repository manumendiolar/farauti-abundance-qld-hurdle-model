# ------------------------------------------------------------------------------
# TABLE 2
# Predictive performance of the six distribution models for \anff\ suitability.
# Values are means from repeated stratified 10-fold cross-validation
# (10 folds, 5 repetitions). Suitability predictions were converted to binary
# presence-absence classifications using a threshold of 0.25 (tau = 0.25).
# Reported metrics include AUC (Area Under the Curve), Balanced Accuracy (BA),
# TSS (True Skill Statistic) and Matthews Correlation Coefficient (MCC).
# Best value per metric in bold.
# ------------------------------------------------------------------------------


# Reads outputs/tables/binary_model_kfold_10x5r.csv 
# Optional: writes a LaTeX table.

tau <- 0.25
infile  <- file.path("outputs", "tables", sprintf("binary_model_kfold_10x5r_%.2f.csv",tau)) 
outfile <- file.path("outputs", "tables", sprintf("binary_model_kfold_10x5r_%.2f.tex", tau))

df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE)

# keep only the columns we need (and in the right order)
keep <- c("Model", "AUC", "BA", "TSS", "MCC")
df <- df[keep]

# (optional) order rows in a preferred sequence if present
ord <- c("RF","BRT","MaxEnt","GLM","GAM","ENS")
df$Model <- factor(df$Model, levels = ord)
df <- df[order(df$Model), , drop = FALSE]

# Full model names for display (match the manuscript)
model_names <- c(
  RF     = "Random Forest",
  BRT    = "Boosted Regression Trees",
  MaxEnt = "MaxEnt",
  GLM    = "Generalised Linear Model",
  GAM    = "Generalised Additive Model",
  ENS    = "Ensemble"
)

# format numbers to 2 decimals
fmt <- function(x) formatC(x, format = "f", digits = 2)

# Build display table: spell out model names and bold the best (max) value per
# metric, comparing on the rounded 2-dp value so display ties (e.g. BA) both bold.
metric_cols <- c("AUC", "BA", "TSS", "MCC")
disp <- df
for (mc in metric_cols) {
  vals <- round(as.numeric(df[[mc]]), 2)
  is_best <- !is.na(vals) & vals == max(vals, na.rm = TRUE)
  cell <- fmt(as.numeric(df[[mc]]))
  cell[is_best] <- paste0("\\textbf{", cell[is_best], "}")
  disp[[mc]] <- cell
}
disp$Model <- ifelse(is.na(model_names[as.character(df$Model)]),
                     as.character(df$Model),
                     model_names[as.character(df$Model)])

# build LaTeX rows
rows <- apply(disp, 1, function(r)
  sprintf("%s & %s & %s & %s & %s \\\\",
          r["Model"], r["AUC"], r["BA"], r["TSS"], r["MCC"]))

latex <- paste0(
"\\begin{table}[H]\n",
"    \\centering\n",
"    \\caption{Predictive performance of the six distribution models for \\anff\\ suitability. Values are means from repeated stratified 10--fold cross-validation (10 folds, 5 repetitions). Suitability predictions were converted to binary presence--absence classifications using a threshold of ", sprintf("%.2f", tau), " ($\\tau=", sprintf("%.2f", tau), "$). Reported metrics include AUC (Area Under the Curve), Balanced Accuracy (BA), TSS (True Skill Statistic) and Matthews Correlation Coefficient (MCC). Best value per metric in bold.}\n",
"    \\label{tab:kfold_repeated_4_metrics_binary_models}\n",
"    \\begin{threeparttable}\n",
"    \\rowcolors{1}{}{white}\n",
"    \\begin{tabularx}{0.75\\textwidth}{l*{5}{>{\\centering\\arraybackslash}X}}\n",
"        \\toprule\n",
"        \\textbf{Model} & \\textbf{AUC} & \\textbf{BA} & \\textbf{TSS} & \\textbf{MCC} \\\\\n",
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

