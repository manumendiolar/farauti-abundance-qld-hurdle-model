
# *Anopheles farauti* abundance in Queensland: a hurdle modelling approach manuscript

This repo contains R scripts to reproduce the results in *Anopheles farauti* abundance in Queensland: a hurdle modelling approach.

 - Fit **distribution models** (hurdle 1st component, presence-absence models).
 - Fit **abundance models** (hurdle 2nd component, truncated count models).
 - Fit **zero-inflated models** (one-framework for comparison). 
 
with **repeated K-fold cross-validation**.
 
---

## 📂 Project Structure
- `data/`     : raw and processed datasets 
- `code/`     : R scripts for modelling and analysis
- `outputs/`  : results (figures, tables, maps)
- `docs/`     : notes, supplementary material
- `renv/`     : project-local R library (auto-managed)

**Tip:** All scripts assume relative paths from the project root and rely on `scripts/00_setup.R`. Avoid hard-coded absolute paths.

## 🔧 Setup

1. **Clone** this repo.
2. **Open** `farauti-abundance-qld-hurdle-model.Rproj` in RStudio (this automatically activates the project-local environment via `renv`).
3. **Install packages (first time only):**

```r
   install.packages("renv")  # only if not already installed globally
   renv::restore()           # only once per fresh clone or new machine
   source("scripts/00_setup.R")
```
   
## ▶️ Running the Models

```r
source("code/01_dist_models.R")
source("code/02_abund_models.R")
source("code/03_zi_models.R")
```

## ▶️ Predictions across Queensland

```r
source("scripts/predictions_at_centroids.R")

# Optional: Map + time series plot (QLD)
source("code/paper_fig_05.R")
```
Results (tables, plots, rasters) will be written under outputs/ in case-specific subfolders when defined.