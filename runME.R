# ==============================================================================
# runME.R  —  master driver for the reproducible core pipeline
#
# Manuscript: "A hurdle modelling approach for estimating mosquito abundance:
#              an Anopheles farauti case study"
#
# This script runs the CORE analysis end to end, in the correct order, in a
# single R session:
#
#   setup -> distribution (01) -> abundance (02) -> zero-inflated (03)
#         -> trap efficiency -> predictions -> tables -> figures
#
# It does NOT run code/prepare_data/ (building the environmental covariates from
# external sources). That stage is optional and documented separately in
# code/prepare_data/README.md; its outputs are already provided in the data
# release.
#
# ------------------------------------------------------------------------------
# BEFORE YOU RUN
#   1. Start R with the working directory set to the repository root
#      (open the project in RStudio/Positron, or `setwd("path/to/repo")`).
#   2. First time only, restore the pinned package versions:
#        install.packages("renv"); renv::restore()
#   3. Make sure the data inputs are present in data/ and outputs/ (see README;
#      the analysis data and the large prediction .fst are in the CSIRO Data Portal
#      https://data.csiro.au/collection/csiro:75052).
#
# HOW TO RUN
#   From an R session at the repo root:   source("runME.R")
#   From a terminal at the repo root:     Rscript runME.R
#
# NOTE ON RUNTIME
#   A full run refits every model with repeated (5x) stratified 10-fold
#   cross-validation and a 1,000-iteration bootstrap (Figure 3). Expect this to
#   take a long time (tens of minutes to hours depending on the machine). Use the
#   `run_stages` selector below to run only part of the pipeline.
# ==============================================================================


# ---- Configuration -----------------------------------------------------------

# Stages to run, in order. Comment out any you want to skip. Note the
# dependencies: `tables` and `figures` read outputs produced by the model
# stages, and `predictions`/`figures` (Fig 4) need the model objects/outputs
# from 01-03, so run the earlier stages at least once first.
run_stages <- c(
  "distribution",     # 01_dist_models.R
  "abundance",        # 02_abund_models.R
  "zero_inflated",    # 03_zi_models.R
  "trap_efficiency",  # trap_efficiency_sensitivity_analysis.R
  "predictions",      # predictions_at_centroids.R   (see KNOWN ISSUE below)
  "tables",           # paper_tbl_*.R
  "figures"           # paper_fig_*.R
)

# Stop at the first error (TRUE), or report it and continue with later stages
# (FALSE)? For a clean reproduction run keep this TRUE.
stop_on_error <- TRUE

# NOTE (predictions / ensemble weights): predictions_at_centroids.R reads
# outputs/tables/count_model_kfold_10x5r.csv (raw-scale CV metrics) for its
# ensemble (ENS) weights. That file is produced by the `abundance` stage
# (02_abund_models.R), so run the model stages at least once before `predictions`.
# The ENS option is used only by the Shiny app; the manuscript's Figures 3 and 4
# use the BRT x RF hurdle and do not depend on it.


# ---- Locate the project root & load setup ------------------------------------
# Bootstrapped without here::here() so this works before packages are attached.
if (!file.exists(file.path("code", "00_setup.R"))) {
  stop(
    "runME.R must be run from the repository root (the folder containing 'code/').\n",
    "Current working directory: ", getwd(), "\n",
    "Fix: open the project folder, or run setwd('path/to/repo') first.",
    call. = FALSE
  )
}
source(file.path("code", "00_setup.R"))  # packages, paths (dir_*), seed, aux fns


# ---- Pre-flight: required input files ----------------------------------------
required_inputs <- c(
  file.path(dir_data, "ab.csv"),
  file.path(dir_data, "bg_points.csv"),
  file.path(dir_data, "centroids_5x5_qld.fst"),
  file.path(dir_data, "maps", "gadm41_AUS_0.shp")
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "Missing required input file(s):\n  ",
    paste(missing_inputs, collapse = "\n  "), "\n\n",
    "These are provided in the CSIRO Data Access Portal release, not on GitHub.\n",
    "See README.md and data/README.md.",
    call. = FALSE
  )
}
if (length(list.files(dir_data, pattern = "\\.asc$")) == 0) {
  warning("No .asc environmental rasters found in ", dir_data,
          " - the distribution model and Figure 2 will fail without them.",
          call. = FALSE)
}


# ---- Stage definitions -------------------------------------------------------
# Each entry maps a stage name to the script(s) it runs, in order. Comments give
# the manuscript figure/table each script produces.
stage_scripts <- list(
  distribution    = "01_dist_models.R",
  abundance       = "02_abund_models.R",
  zero_inflated   = "03_zi_models.R",
  trap_efficiency = "trap_efficiency_sensitivity_analysis.R",  # Table S5 inputs
  predictions     = "predictions_at_centroids.R",              # centroids .fst
  tables = c(
    "paper_tbl_02.R",    # Table 2  (distribution CV metrics)
    "paper_tbl_03.R",    # Table 3  (abundance CV metrics)
    "paper_tbl_S02.R",   # Table S2 (thresholds)
    "paper_tbl_S03.R",   # Table S3 (additional distribution metrics)
    "paper_tbl_S04.R",   # Table S4 (zero-inflated models)
    "paper_tbl_S05.R"    # Table S5 (trap efficiency)
  ),
  figures = c(
    "paper_fig_01.R",    # Figure 1  (survey locations map)  [canonical; not _new]
    "paper_fig_02.R",    # Figure 2  (+ Figures S2-S6: per-model distribution/uncertainty)
    "paper_fig_03.R",    # Figure 3  (observed vs predicted time series; bootstrap)
    "paper_fig_04.R",    # Figure 4  (abundance heatmap; needs predictions .fst)
    "paper_fig_S01.R",   # Figure S1 (predictor correlation matrix)
    "paper_fig_S07.R",   # Figure S7 (observed vs predicted scatter)
    "paper_fig_S08.R",   # Figure S8  \
    "paper_fig_S09.R",   # Figure S9   |
    "paper_fig_S10.R",   # Figure S10  |
    "paper_fig_S11.R",   # Figure S11  |  variable importance &
    "paper_fig_S12.R",   # Figure S12  |  partial dependence
    "paper_fig_S13.R",   # Figure S13  |
    "paper_fig_S14.R",   # Figure S14  |
    "paper_fig_S15.R",   # Figure S15  |
    "paper_fig_S16.R"    # Figure S16 /
  )
)


# ---- Runner ------------------------------------------------------------------
run_step <- function(script) {
  path <- file.path(dir_code, script)
  if (!file.exists(path)) {
    stop("Script not found: ", path, call. = FALSE)
  }
  message("\n>>> ", script, " ...")
  t0 <- Sys.time()
  ok <- TRUE
  tryCatch(
    source(path, local = FALSE),   # local = FALSE: keep objects in the global env
    error = function(e) {
      ok <<- FALSE
      msg <- paste0("FAILED: ", script, " -- ", conditionMessage(e))
      if (stop_on_error) stop(msg, call. = FALSE) else message(msg)
    }
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)
  message(sprintf("<<< %s %s (%.2f min)", script, if (ok) "done" else "FAILED", elapsed))
  invisible(ok)
}


# ---- Run ---------------------------------------------------------------------
message("========================================================")
message("runME.R  -  core pipeline")
message("Root: ", project_root)
message("Stages: ", paste(run_stages, collapse = ", "))
message("========================================================")

run_start <- Sys.time()
results <- list()
for (stage in run_stages) {
  if (is.null(stage_scripts[[stage]])) {
    warning("Unknown stage skipped: ", stage, call. = FALSE)
    next
  }
  message("\n---------- STAGE: ", stage, " ----------")
  for (script in stage_scripts[[stage]]) {
    results[[script]] <- run_step(script)
  }
}

# ---- Summary -----------------------------------------------------------------
total_min <- round(as.numeric(difftime(Sys.time(), run_start, units = "mins")), 2)
message("\n========================================================")
message(sprintf("Pipeline finished in %.2f min", total_min))
failed <- names(results)[!vapply(results, isTRUE, logical(1))]
if (length(failed) == 0) {
  message("All scripts completed successfully.")
} else {
  message("Scripts that FAILED: ", paste(failed, collapse = ", "))
}
message("Outputs written under: ", file.path("outputs", "{plots,tables,predictions}"))
message("========================================================")
