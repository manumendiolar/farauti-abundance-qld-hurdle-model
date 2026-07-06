# ------------------------------------------------------------------
# TABLE: MEAN PERFORMANCE METRICS FOR DISTRIBUTION MODELS (REPEATED K-FOLDS)
# ------------------------------------------------------------------

tau <- 0.25
infile  <- file.path("outputs", "tables",
                     sprintf("binary_model_kfold_summary_10x5r_%.2f.csv", tau))
outfile <- file.path("outputs", "tables",
                     sprintf("binary_model_kfold_10x5r_%.2f_full.tex", tau))

df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE)

# Optional: order rows in a preferred sequence if present
ord <- c("RF", "BRT", "MAX", "GLM", "GAM", "ENS")
df$Model <- factor(df$Model, levels = ord)
df <- df[order(df$Model), , drop = FALSE]

# Keep only Model and *_mean columns (in desired order)
df <- df[, c("Model",
             "AUC_mean",
             "Accuracy_mean",
             "Sensitivity_mean",
             "Specificity_mean",
             "BA_mean",
             "Kappa_mean",
             "TSS_mean",
             "Precision_mean",
             "F1_mean",
             "MCC_mean")]

# Rename columns to drop "_mean"
colnames(df) <- c("Model",
                  "AUC",
                  "Accuracy",
                  "Sensitivity",
                  "Specificity",
                  "BA",
                  "Kappa",
                  "TSS",
                  "Precision",
                  "F1",
                  "MCC")

# Full model names for display (match the manuscript). The CSV labels MaxEnt "MAX".
model_names <- c(
  RF   = "Random Forest",
  BRT  = "Boosted Regression Trees",
  MAX  = "MaxEnt",
  GLM  = "Generalised Linear Model",
  GAM  = "Generalised Additive Model",
  ENS  = "Ensemble"
)

# simple formatter
fmt <- function(x) formatC(x, format = "f", digits = 2)

# Build display table: spell out model names and bold the best value per metric.
# All ten metrics here are higher-is-better. Compare on the rounded 2-dp value so
# display ties both bold.
metric_cols <- c("AUC","Accuracy","Sensitivity","Specificity","BA",
                 "Kappa","TSS","Precision","F1","MCC")
disp <- df
for (mc in metric_cols) {
  vals   <- round(as.numeric(df[[mc]]), 2)
  cell   <- fmt(as.numeric(df[[mc]]))
  hit    <- !is.na(vals) & vals == max(vals, na.rm = TRUE)
  cell[hit] <- paste0("\\textbf{", cell[hit], "}")
  disp[[mc]] <- cell
}
disp$Model <- ifelse(is.na(model_names[as.character(df$Model)]),
                     as.character(df$Model), model_names[as.character(df$Model)])

# build LaTeX rows: Model + mean for each metric
rows <- apply(disp, 1, function(r) {
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
    r["Model"], r["AUC"], r["Accuracy"], r["Sensitivity"], r["Specificity"],
    r["BA"], r["Kappa"], r["TSS"], r["Precision"], r["F1"], r["MCC"]
  )
})

latex <- paste0(
"\\begin{landscape}\n",
"\\begin{table}[ht]\n",
"    \\centering\n",
"    \\caption{Cross-validation performance of the six distribution models for \\anff\\ suitability, reporting the full metric set (10 folds, 5 repetitions). Suitability was converted to binary predictions using a threshold of $\\tau = 0.25$. Reported metrics include AUC, Accuracy (Acc), Sensitivity (Sens), Specificity (Spec), Balanced Accuracy (BA), Cohen's Kappa (Kappa), True Skill Statistic (TSS), Precision (Prec), F1, and Matthews Correlation Coefficient (MCC). Best value per metric in bold.}\n",
"    \\label{tab:dist_kfold_means}\n",
"    \\small\n",
"    \\setlength{\\tabcolsep}{3pt}\n",
"    \\renewcommand{\\arraystretch}{1.05}\n",
"    \\begin{threeparttable}\n",
"    \\rowcolors{1}{}{white}\n",
"    \\begin{tabularx}{\\linewidth}{l*{10}{>{\\centering\\arraybackslash}X}}\n",
"    \\toprule\n",
"    \\textbf{Model} & \\textbf{AUC} & \\textbf{Acc} & \\textbf{Sens} & \\textbf{Spec} & \\textbf{BA} & \\textbf{Kappa} & \\textbf{TSS} & \\textbf{Prec} & \\textbf{F1} & \\textbf{MCC}\\\\\n",
"    \\midrule\n",
"    ", paste(rows, collapse = "\n    "), "\n",
"    \\bottomrule\n",
"    \\end{tabularx}\n",
"    \\end{threeparttable}\n",
"\\end{table}\n",
"\\end{landscape}\n"
)

# Print to console (for checking)
cat(latex)

# Write file 
writeLines(latex, outfile)
