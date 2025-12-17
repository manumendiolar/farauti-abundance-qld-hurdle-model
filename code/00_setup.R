# ------------------------------------------------------------------------------
# Project setup 
# ------------------------------------------------------------------------------

# Function to check and install packages if needed
check_and_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      message("Installing missing package: ", pkg)
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# All required packages (duplicates removed)
required_packages <- c(
  # Core data manipulation and visualization
  "tidyverse",
  "dplyr", 
  "tidyr",
  "purrr",
  "ggplot2",
  "rlang",
  
  # Spatial and raster packages
  "raster",
  "terra", 
  "sf",
  "ggspatial",
  "rnaturalearth",
  "rnaturalearthdata",
  
  # Species distribution modeling
  "dismo",
  "ENMTools",
  "randomForest",
  "gbm",
  "mgcv",
  "glmmTMB",
  "pscl",
  
  # Model evaluation and diagnostics
  "pROC",
  "caret",
  "DHARMa",
  "effects",
  "ggeffects",
  "pdp",
  
  # Utilities and workflow
  "here",
  "fs",
  "tools",
  "parallel",
  "progress",
  "fst",
  
  # Statistics and correlation
  "usdm",
  "MASS",
  "ggcorrplot",
  "RColorBrewer",
  "fields",
  "stats",
  
  # Reporting and visualization
  "xtable",
  "png",
  "grid",
  "cowplot",
  "viridis",
  "glue",
  "ggrepel", 
  "patchwork",
  "tibble",
  "ggh4x",
  "scales"
)

# Check and install all packages
check_and_install(required_packages)

# Load packages silently
suppressPackageStartupMessages({
  invisible(lapply(required_packages, library, character.only = TRUE))
})

# Project root (works when opening farab.Rproj or running scripts via RStudio)
project_root <- here::here()

# Canonical directories (relative to project root)
dir_data   <- file.path(project_root, "data")
dir_plots  <- file.path(project_root, "outputs", "plots")
dir_tables <- file.path(project_root, "outputs", "tables")
dir_pred   <- file.path(project_root, "outputs", "predictions")
dir_code   <- file.path(project_root, "code") 

# Ensure output dirs exist
dir.create(dir_plots,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_pred,   recursive = TRUE, showWarnings = FALSE)

# Reproducibility & parallel
set.seed(123)
options(mc.cores = max(1, parallel::detectCores() - 1))

# Load auxiliary functions if present
aux_file <- file.path(dir_code, "aux_functions.R")
if (file.exists(aux_file)) source(aux_file)


message("✅ Setup complete. Root: ", project_root)

