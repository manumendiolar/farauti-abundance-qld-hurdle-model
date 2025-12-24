# *Anopheles farauti* abundance in Queensland: a hurdle modelling approach manuscript

This repo contains R scripts to reproduce the results in *Anopheles farauti* abundance in Queensland: a hurdle modelling approach. Basic scripts:

-   `01_dist_models.R`: Fit **distribution models** (presence-absence models).
-   `02_abund_models.R`: Fit **abundance models** (truncated count models).
-   `03_zi_models.R`: Fit **zero-inflated models** (one-framework for comparison).

**Note:** all using *repeated K-fold cross-validation*.

------------------------------------------------------------------------

## 📂 Project Structure

-   `data/` : raw and processed datasets
-   `code/` : R scripts for modelling and analysis
-   `outputs/` : results (figures, tables, maps)
-   `docs/` : notes, supplementary material
-   `renv/` : project-local R library (auto-managed)

**Tip:** All scripts assume relative paths from the project root and rely on `scripts/00_setup.R`. Avoid hard-coded absolute paths.

## 🔧 Setup

You can run this project using **any R environment** (Positron, RStudio, or terminal/base R).

1.  **Clone** this repo.
2.  **Start an R session in the project root**  
   - Positron/RStudio: open the project folder  
   - Terminal: `cd` into the repo and run `R`  
   - Or in R: `setwd("path/to/repo")`
3.  **Install/restore packages (first time only):**

``` r
   install.packages("renv")  # only if not already installed 
   renv::restore()           # only once per fresh clone or new machine
   source("scripts/00_setup.R")
```
**Tip:** If `renv::restore()` asks to activate the project, answer yes.

## ▶️ Running the Models

``` r
source("code/01_dist_models.R")
source("code/02_abund_models.R")
source("code/03_zi_models.R")
```

## 🗺️ Predictions across Queensland

``` r
source("scripts/predictions_at_centroids.R")

# Optional: Map + time series plot (QLD)
source("code/paper_fig_04.R")
```

**Note:** Results (tables, plots, rasters) will be written under outputs/ in case-specific subfolders when defined.

## 🦟 Interactive Shiny app

This repository includes a Shiny dashboard for exploring predicted *Anopheles farauti* hurdle-model abundance and suitability across Queensland.

#### 📦 Large data file (not stored in GitHub)

The Shiny app requires a large prediction file that is **not included in this repository** (too large for GitHub):

-   `centroids_5x5_qld_with_predictions_shiny-app.fst`

This file is hosted on Zenodo. 

**Steps**

1. Go to the Zenodo record: https://doi.org/10.5281/zenodo.17984922
2. In the “Files” section, download: centroids_5x5_qld_with_predictions_shiny-app.fst
3. Place the file here (inside your cloned repo):
`outputs/predictions/centroids_5x5_qld_with_predictions_shiny-app.fst`
4. Run the app locally:
```r
shiny::runApp()
```

Run from the terminal (optional)

```bash
R -e "install.packages('renv'); renv::restore(); shiny::runApp()"
```

#### 📌 Data availability and citation

The large predictions file used by the Shiny app is hosted on Zenodo:

Zenodo record: https://zenodo.org/records/17984922

How to cite:

Mendiolar, M. (2025). Centroids with predictions: *Anopheles farauti* predicted abundance using hurdle model (Queensland, 5km grid) \[Data set\]. Zenodo. https://doi.org/10.5281/zenodo.17984922