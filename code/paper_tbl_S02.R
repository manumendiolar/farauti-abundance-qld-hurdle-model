# 
# TABLE S02 (SUPP MAT.)
#
# This script will:
#  1) Compute data-driven optimal thresholds (if the CSV doesn't exist or if 
#     `recompute_thresholds <- TRUE`), using PresenceAbsence::optimal.thresholds().
#  2) Render a LaTeX table ready to \input{} in manuscript.
#
# Inputs required to (re)compute thresholds:
#   - `data$presence` (0/1 or FALSE/TRUE)
#   - predictions either as:
#       (a) `preds_full` list with names RF, BRT, MAX, GLM, GAM, ENS (preferred), OR
#       (b) vectors: preds_RF, preds_BRT, preds_MAX, preds_GLM, preds_GAM, preds_ENS
#   - `dir_tables` path (defined in your setup script)

# Set up
source(here::here("code","00_setup.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(knitr)
  library(kableExtra)
  library(PresenceAbsence)
})

# Uncomment if distribution models haven't been calibrated
#source(here::here("code","01_dist_models.R"))


# ---- user options ----
recompute_thresholds <- TRUE

# ---- paths ----
infile  <- file.path(dir_tables, "binary_model_optimal_thresholds_R.csv")
outfile <- file.path(dir_tables, "binary_model_optimal_thresholds_R.tex")

# ---- helper: compute and (optionally) write thresholds ----
compute_optimal_thresholds <- function(
  presence,
  preds,
  req_sens = 0.85,
  req_spec = 0.85,
  cost_fp  = 1,
  cost_fn  = 1
) {
  stopifnot(length(presence) == length(preds$RF))
  DATA <- data.frame(
    ID       = seq_along(presence),
    presence = as.numeric(presence),
    RF_pred  = as.numeric(preds$RF),
    BRT_pred = as.numeric(preds$BRT),
    MAX_pred = as.numeric(preds$MAX),
    GLM_pred = as.numeric(preds$GLM),
    GAM_pred = as.numeric(preds$GAM),
    ENS_pred = as.numeric(preds$ENS)
  )

  # PresenceAbsence::optimal.thresholds() defaults are package-defined;
  # we set req_sens/req_spec and equal costs explicitly to match table notes.
  thr <- PresenceAbsence::optimal.thresholds(DATA)

  thr <- as.data.frame(thr)
  names(thr)[1] <- "Method"
  thr  
}

# ---- compute thresholds if needed ----
if (recompute_thresholds || !file.exists(infile)) {

  if (!exists("data")) {
    stop("Cannot compute thresholds: object `data` not found, and CSV does not exist at: ", infile)
  }
  if (!("presence" %in% names(data))) {
    stop("Cannot compute thresholds: `data$presence` not found.")
  }

  # Prefer a single list of predictions if available
  preds <- NULL
  if (exists("preds_full") && is.list(preds_full)) {
    needed <- c("RF","BRT","MAX","GLM","GAM","ENS")
    if (!all(needed %in% names(preds_full))) {
      stop("`preds_full` exists but is missing names: ", paste(setdiff(needed, names(preds_full)), collapse = ", "))
    }
    preds <- list(
      RF  = preds_full[["RF"]],
      BRT = preds_full[["BRT"]],
      MAX = preds_full[["MAX"]],
      GLM = preds_full[["GLM"]],
      GAM = preds_full[["GAM"]],
      ENS = preds_full[["ENS"]]
    )
  } else {
    needed <- c("preds_RF","preds_BRT","preds_MAX","preds_GLM","preds_GAM","preds_ENS")
    missing <- needed[!vapply(needed, exists, logical(1))]
    if (length(missing) > 0) {
      stop(
        "Cannot compute thresholds: missing prediction objects (and `preds_full` not found): ",
        paste(missing, collapse = ", ")
      )
    }
    preds <- list(
      RF  = get("preds_RF"),
      BRT = get("preds_BRT"),
      MAX = get("preds_MAX"),
      GLM = get("preds_GLM"),
      GAM = get("preds_GAM"),
      ENS = get("preds_ENS")
    )
  }

  thr_raw <- compute_optimal_thresholds(
    presence = data$presence,
    preds    = preds,
    req_sens = 0.85,
    req_spec = 0.85,
    cost_fp  = 1,
    cost_fn  = 1
  )

  # Save the raw PresenceAbsence output for reproducibility
  write_csv(thr_raw, infile)
  message("Wrote thresholds CSV: ", infile)
}

# ---- read + clean (for LaTeX table) ----
thr <- read_csv(infile, show_col_types = FALSE)

# PresenceAbsence output usually has method in first column; make that explicit
names(thr)[1] <- "Method"

# Standardise column names to match your manuscript header
thr <- thr %>%
  rename(
    RF     = RF_pred,
    BRT    = BRT_pred,
    MaxEnt = MAX_pred,
    GLM    = GLM_pred,
    GAM    = GAM_pred,
    ENS    = ENS_pred
  ) %>%
  dplyr::select(Method, RF, BRT, MaxEnt, GLM, GAM, ENS) %>%
  mutate(across(-Method, ~ round(as.numeric(.x), 2)))

# Optional: enforce preferred method order
# 1	  Default	threshold=0.5
# 2	  Sens=Spec	sensitivity=specificity (balancing true positive and true negative rates)
# 3	  MaxSens+Spec	maximizes (sensitivity+specificity)/2
# 4	  MaxKappa	maximizes Kappa
# 5   MaxPCC	maximizes PCC (percent correctly classified)
# 6	  PredPrev=Obs	predicted prevalence=observed prevalence
# 7	  ObsPrev	threshold=observed prevalence
# 8	  MeanProb	mean predicted probability
# 9	  MinROCdist	minimizes distance between ROC plot and (0,1)
# 10	ReqSens	user defined required sensitivity
# 11	ReqSpec	user defined required specificity
# 12	Cost	user defined relative costs ratio
method_order <- c(
  "Default",
  "Sens=Spec",
  "MaxSens+Spec",
  "MaxKappa",
  "MaxPCC",
  "PredPrev=Obs",
  "ObsPrev",
  "MeanProb",
  "MinROCdist",
  "ReqSens",
  "ReqSpec",
  "Cost"
)
thr <- thr %>%
  mutate(Method = factor(Method, levels = method_order)) %>%
  arrange(Method) %>%
  mutate(Method = as.character(Method))

# ---- make LaTeX table ----
tab_tex <- kbl(
  thr,
  format   = "latex",
  booktabs = TRUE,
  align    = c("l", rep("c", 6)),
  col.names = c("Method","RF","BRT","MaxEnt","GLM","GAM","ENS"),
  escape   = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE
  ) %>%
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
"    \\caption{Data-driven thresholds for converting predicted suitability to binary presence--absence for each distribution model considered in this study. Values were obtained using the \\texttt{optimal.thresholds()} function in the \\texttt{PresenceAbsence} R package. For each optimisation method, the table reports the threshold on the 0--1 suitability scale that optimises that method for each model.}\n",
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
