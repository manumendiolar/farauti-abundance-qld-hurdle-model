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

# simple formatter
fmt <- function(x) formatC(x, format = "f", digits = 2)

# build LaTeX rows: Model + mean for each metric
rows <- apply(df, 1, function(r) {
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
    as.character(r["Model"]),
    fmt(as.numeric(r["AUC"])),
    fmt(as.numeric(r["Accuracy"])),
    fmt(as.numeric(r["Sensitivity"])),
    fmt(as.numeric(r["Specificity"])),
    fmt(as.numeric(r["BA"])),
    fmt(as.numeric(r["Kappa"])),
    fmt(as.numeric(r["TSS"])),
    fmt(as.numeric(r["Precision"])),
    fmt(as.numeric(r["F1"])),
    fmt(as.numeric(r["MCC"]))
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
