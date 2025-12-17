# Build LaTeX table for distribution-model thresholds from:
#   binary_model_optimal_thresholds_R.csv
# Output:
#   dist_thresholds.tex  (ready to \input{} in LaTeX)

library(dplyr)
library(readr)
library(stringr)
library(knitr)
library(kableExtra)

# ---- paths ----
infile  <- file.path(dir_tables, "binary_model_optimal_thresholds_R.csv")
outfile <- file.path(dir_tables, "dist_thresholds.tex")

# ---- read + clean ----
thr <- read_csv(infile, show_col_types = FALSE)

# PresenceAbsence output usually has method in first column; make that explicit
names(thr)[1] <- "Method"

# Standardise column names to match your manuscript header
# (adjust these if your CSV uses different names)
thr <- thr %>%
  rename(
    RF     = RF_pred,
    BRT    = BRT_pred,
    MaxEnt = MAX_pred,
    GLM    = GLM_pred,
    GAM    = GAM_pred,
    ENS    = ENS_pred
  ) %>%
  # ensure method labels match your table exactly
  mutate(
    Method = str_replace_all(Method, fixed("PredPrev=Obs"), "PredPrev=Obs"),
    Method = str_replace_all(Method, fixed("MinROCdist"),   "MinROCdist"),
    Method = str_replace_all(Method, fixed("MaxSens+Spec"), "MaxSens+Spec")
  ) %>%
  select(Method, RF, BRT, MaxEnt, GLM, GAM, ENS) %>%
  mutate(across(-Method, ~ round(as.numeric(.x), 2)))

# Optional: enforce your preferred method order
method_order <- c(
  "Default","Sens=Spec","MaxSens+Spec","MaxKappa","MaxPCC",
  "PredPrev=Obs","ObsPrev","MeanProb","MinROCdist","ReqSens","ReqSpec","Cost"
)
thr <- thr %>%
  mutate(Method = factor(Method, levels = method_order)) %>%
  arrange(Method) %>%
  mutate(Method = as.character(Method))

# ---- make LaTeX table ----
tab_tex <- kbl(
  thr,
  format = "latex",
  booktabs = TRUE,
  align = c("l", rep("c", 6)),
  col.names = c("Method","RF","BRT","MaxEnt","GLM","GAM","ENS"),
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE
  ) %>%
  add_header_above(c(" " = 1, " " = 6)) %>% # keeps header spacing clean
  row_spec(0, bold = TRUE)

# Wrap with threeparttable + tabularx exactly like your template
# (kable creates a tabular; we replace it with tabularx)
tab_tex <- as.character(tab_tex)

# Replace tabular environment with tabularx (0.95\textwidth)
tab_tex <- tab_tex |>
  str_replace("\\\\begin\\{tabular\\}\\{[lcr]+\\}",
              "\\\\begin{tabularx}{0.95\\\\textwidth}{l*{6}{>{\\\\centering\\\\arraybackslash}X}}") |>
  str_replace("\\\\end\\{tabular\\}", "\\\\end{tabularx}")

# Add caption/label + threeparttable + notes
tab_tex <- paste0(
"\\begin{table}[ht]\n",
"    \\centering\n",
"    \\caption{Data-driven thresholds for converting predicted suitability to binary presence-absence for each distribution model considered in this study. Values were obtained using the \\texttt{optimal.thresholds()} function in the \\texttt{PresenceAbsence} R package. For each optimisation method, the table reports the threshold on the 0--1 suitability scale that optimises that method for each model.}\n",
"    \\label{tab:dist_thresholds}\n",
"    \\begin{threeparttable}\n",
"    \\rowcolors{1}{}{white}\n",
tab_tex, "\n",
"    \\begin{tablenotes}\n",
"    \\footnotesize\n",
"    \\item \\textbf{Default}: fixed threshold of 0.5.\n",
"    \\item \\textbf{Sens=Spec}: threshold where sensitivity equals specificity.\n",
"    \\item \\textbf{MaxSens+Spec}: threshold that maximises the sum of sensitivity and specificity.\n",
"    \\item \\textbf{MaxKappa}: threshold that maximises Cohen's Kappa.\n",
"    \\item \\textbf{MaxPCC}: threshold that maximises the total accuracy.\n",
"    \\item \\textbf{PredPrev=Obs}: threshold where predicted prevalence equals observed prevalence.\n",
"    \\item \\textbf{ObsPrev}: threshold set to the observed prevalence.\n",
"    \\item \\textbf{MeanProb}: threshold set to the mean predicted suitability.\n",
"    \\item \\textbf{MinROCdist}: threshold that minimises distance to the upper-left corner of the ROC curve.\n",
"    \\item \\textbf{ReqSens}: highest threshold that attains the required sensitivity (here 0.85).\n",
"    \\item \\textbf{ReqSpec}: lowest threshold that attains the required specificity (here 0.85).\n",
"    \\item \\textbf{Cost}: minimises misclassification cost assuming equal costs of false positives and false negatives.\n",
"    \\end{tablenotes}\n",
"    \\end{threeparttable}\n",
"\\end{table}\n"
)

writeLines(tab_tex, outfile)
message("Wrote: ", outfile)
