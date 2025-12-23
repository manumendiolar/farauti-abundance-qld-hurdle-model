
# *Anopheles farauti* abundance in Queensland: a hurdle modelling approach manuscript

This repo contains R scripts to reproduce the results in *Anopheles farauti* abundance in Queensland: a hurdle modelling approach:

 - `01_dist_models.R`: Fit **distribution models** (presence-absence models).
 - `02_abund_models.R`: Fit **abundance models** (truncated count models).
 - `03_zi_models.R`: Fit **zero-inflated models** (one-framework for comparison). 
 
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
source("code/paper_fig_04.R")
```
Results (tables, plots, rasters) will be written under outputs/ in case-specific subfolders when defined.

## 🦟 Interactive Shiny app

This repository includes a Shiny dashboard for exploring predicted *Anopheles farauti* hurdle-model abundance and suitability across Queensland.

#### Requirements
- R (>= 4.2 recommended)
- RStudio (optional)
- Packages managed via `renv`

#### 📦 Large data file (not stored in GitHub)

The Shiny app requires the predictions file:

- `centroids_5x5_qld_with_predictions_shiny-app.fst`

Because this file is too large for GitHub, it is hosted externally in Zenodo. Steps:

1. Download the `.fst` file from the Zenodo record (see “Data availability” / Zenodo link <https://zenodo.org/records/17984922/files/centroids_5x5_qld_with_predictions_shiny-app.fst?download=1>).
2. Place it here (inside cloned repo):

`outputs/predictions/centroids_5x5_qld_with_predictions_shiny-app.fst`

3. Run the app.

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

## 📌 Data availability and citation

The large predictions file used by the Shiny app is hosted on Zenodo:

Zenodo record: https://zenodo.org/records/17984922

How to cite:

Mendiolar, M. (2025). Centroids with predictions: *Anopheles farauti* predicted abundance using hurdle model (Queensland, 5km grid) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.17984922