# ------------------------------------------------------------------------------
# TABLE S04
# Cross-validation performance of the zero-inflated abundance models fitted to
# all \anff\  counts (10 folds, 5 repetitions). Metrics include Root Mean Squared
# Error (RMSE), Mean Absolute Error (MAE) and correlation coefficients (Pearson
# and Spearman) between observed and predicted values.
# ------------------------------------------------------------------------------


# Reads outputs/tables/zi_model_kfold_10x5r.csv -- the raw count-scale CV metrics
# written by 03_zi_models.R (assumed to match the manuscript, Table S4, as for
# Table 3; confirm against the paper).
# Optional: writes a LaTeX table.

infile  <- file.path("outputs", "tables", "zi_model_kfold_10x5r.csv")
outfile <- file.path("outputs", "tables", "zi_model_kfold_10x5r.tex")

df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE)

# (optional) order rows in a preferred sequence if present
ord <- c("ZIP","ZINB")
df$Model <- factor(df$Model, levels = ord)
df <- df[order(df$Model), , drop = FALSE]

# Full model names for display (match the manuscript)
model_names <- c(
  ZIP  = "Zero Inflated Poisson",
  ZINB = "Zero Inflated Negative Binomial"
)

# format numbers to 2 decimals
fmt <- function(x) formatC(x, format = "f", digits = 2)

# Build display table: spell out model names. No bolding here -- the manuscript
# Table S4 (only two models) does not bold a "best" value per metric.
disp <- df
for (mc in c("RMSE", "MAE", "Pearson", "Spearman")) disp[[mc]] <- fmt(as.numeric(df[[mc]]))
disp$Model <- ifelse(is.na(model_names[as.character(df$Model)]),
                     as.character(df$Model), model_names[as.character(df$Model)])

# build LaTeX rows
rows <- apply(disp, 1, function(r)
  sprintf("%s & %s & %s & %s & %s \\\\",
          r["Model"], r["RMSE"], r["MAE"], r["Pearson"], r["Spearman"]))

latex <- paste0(
"\\begin{table}[H]\n",
"    \\centering\n",
"    \\caption{Cross-validation performance of the zero-inflated abundance models fitted to all \\anff\\ counts (10 folds, 5 repetitions). Metrics include Root Mean Squared Error (RMSE), Mean Absolute Error (MAE) and correlation coefficients (Pearson and Spearman) between observed and predicted values.}\n",
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
