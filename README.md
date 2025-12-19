
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

## 🦟 Interactive Shiny app

This repository includes a Shiny dashboard for exploring predicted *Anopheles farauti* hurdle-model abundance and suitability across Queensland.

#### Requirements
- R (>= 4.2 recommended)
- RStudio (optional)
- Packages managed via `renv`

### 📦 Large data file (not stored in GitHub)

The Shiny app requires the predictions file:

- `centroids_5x5_qld_with_predictions.fst`

This file is too large for GitHub and is hosted externally here:
- **Download (Zenodo/OSF):** <PASTE YOUR LINK HERE>

After downloading, place it in:

`outputs/predictions/centroids_5x5_qld_with_predictions.fst`

(Or set `FARAUTI_PRED_DIR` to the folder containing the file; see below.)

### ▶️ Run the Shiny app locally

1. Clone the repository
2. Open the `.Rproj` in RStudio
3. Restore packages:

```r
install.packages("renv")   # if needed
renv::restore()
```
4. Ensure the predictions file is available (see “Large data file” above)
5. Run the app:
```r
shiny::runApp()
```

