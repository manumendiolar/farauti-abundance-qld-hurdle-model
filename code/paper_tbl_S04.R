# ------------------------------------------------------------------------------
# TABLE 4 
# Repeated stratified 10-fold cross-validation (means across 5 repetitions) 
# performance metrics for abundance models fitted to all \anff\  counts. 
# Metrics include Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), 
# and correlation coefficients (Pearson and Spearman) between observed and 
# predicted values. Models: Zero Inflated Poisson (ZIP) and Zero Inflated 
# Negative Binomial (ZINB).
# ------------------------------------------------------------------------------


# Reads outputs/tables/zi_model_kfold_10x5r.csv 
# Optional: writes a LaTeX table.

infile  <- file.path("outputs", "tables", "zi_model_kfold_10x5r.csv")
outfile <- file.path("outputs", "tables", "zi_model_kfold_table.tex")

df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE)

# (optional) order rows in a preferred sequence if present
ord <- c("ZIP","ZINB")
df$Model <- factor(df$Model, levels = ord)
df <- df[order(df$Model), , drop = FALSE]

# format numbers to 2 decimals
fmt <- function(x) formatC(x, format = "f", digits = 2)

# build LaTeX rows
rows <- apply(df, 1, function(r)
  sprintf("%s & %s & %s & %s & %s \\\\",
          as.character(r["Model"]),
          fmt(as.numeric(r["RMSE"])),
          fmt(as.numeric(r["MAE"])),
          fmt(as.numeric(r["Pearson"])),
          fmt(as.numeric(r["Spearman"]))))

latex <- paste0(
"\\begin{table}[H]\n",
"    \\centering\n",
"    \\caption{Repeated stratified 10--fold cross-validation (means across 5 repetitions) performance metrics for abundance models fitted to all \\anff\\ counts. Metrics include Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), and correlation coefficients (Pearson and Spearman) between observed and predicted values. Models: Zero Inflated Poisson (ZIP) and Zero Inflated Negative Binomial (ZINB).}\n",
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