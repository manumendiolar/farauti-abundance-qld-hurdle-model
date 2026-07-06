# ------------------------------------------------------------------------------
# Project setup 
# ------------------------------------------------------------------------------
# This script:
# - loads required packages (preferably via renv::restore() beforehand)
# - defines common project paths
# - sets reproducibility + parallel defaults
# - sources auxiliary helpers if present

suppressPackageStartupMessages({
  library(here)
})

# ---- Packages ----------------------------------------------------------------
# NOTE:
# If you're using renv (recommended), run renv::restore() once after cloning.
# We avoid auto-installing by default to keep runs reproducible.

required_packages <- c(
  # Core
  "tidyverse",
  "rlang",
  "scales",

  # Spatial
  "terra",
  "raster",
  "sf",
  "ggspatial",
  "rnaturalearth",
  "rnaturalearthdata",

  # Modelling
  "dismo",
  "ENMTools",
  "randomForest",
  "gbm",
  "mgcv",
  "glmmTMB",
  "pscl",

  # Evaluation/diagnostics
  "pROC",
  "caret",
  "DHARMa",
  "effects",
  "ggeffects",
  "pdp",

  # Utilities
  "fs",
  "progress",
  "fst",

  # Correlation/extra stats/plots
  "usdm",
  "MASS",
  "ggcorrplot",
  "RColorBrewer",
  "fields",

  # Reporting/plots
  "xtable",
  "png",
  "grid",
  "cowplot",
  "viridis",
  "glue",
  "ggrepel",
  "patchwork",
  "ggh4x"
)

missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  msg <- paste0(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "), "\n\n",
    "Only scripts that use these will fail; others will still run.\n",
    "Recommended fix:\n",
    "  install.packages('renv')\n",
    "  renv::restore()\n\n",
    "If you are not using renv, install the missing packages manually."
  )
  warning(msg, call. = FALSE)
}

# Load only the packages that are actually installed, so a missing modelling
# package does not block a script that never uses it.
available_pkgs <- setdiff(required_packages, missing_pkgs)
suppressPackageStartupMessages({
  invisible(lapply(available_pkgs, library, character.only = TRUE))
})

# ---- Paths -------------------------------------------------------------------
project_root <- here::here()

dir_data   <- file.path(project_root, "data")
dir_plots  <- file.path(project_root, "outputs", "plots")
dir_tables <- file.path(project_root, "outputs", "tables")
dir_pred   <- file.path(project_root, "outputs", "predictions")
dir_code   <- file.path(project_root, "code")

# Ensure output dirs exist
dir.create(dir_plots,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_pred,   recursive = TRUE, showWarnings = FALSE)

# ---- Reproducibility & parallel ----------------------------------------------
set.seed(123)
options(mc.cores = max(1, parallel::detectCores() - 1))

# ---- Aux functions ------------------------------------------------------------
aux_file <- file.path(dir_code, "aux_functions.R")
if (file.exists(aux_file)) source(aux_file)

message("✅ Setup complete. Root: ", project_root)