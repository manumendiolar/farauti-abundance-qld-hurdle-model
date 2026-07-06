# A hurdle modelling approach for estimating mosquito abundance

This repo contains R scripts to reproduce the results in the manuscript "A hurdle modelling approach for estimating mosquito abundance: an *Anopheles farauti* case study". Basic scripts:

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

**Tip:** All scripts assume relative paths from the project root and rely on `code/00_setup.R`. Avoid hard-coded absolute paths.

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
   source("code/00_setup.R")
```
**Tip:** If `renv::restore()` asks to activate the project, answer yes.

## ▶️ Running the Models

**Easiest — run the whole core pipeline with one command.** From an R session at the repo root:

``` r
source("runME.R")
```

This runs every stage in the correct order (distribution → abundance → zero-inflated → trap efficiency → predictions → tables → figures) with pre-flight checks for the required input files. Edit the `run_stages` selector at the top of `runME.R` to run only part of the pipeline. A full run refits every model with repeated cross-validation and a 1,000-iteration bootstrap, so expect it to take a while (tens of minutes to hours).

Or run the stages individually:

``` r
source("code/01_dist_models.R")
source("code/02_abund_models.R")
source("code/03_zi_models.R")
```

## 🗺️ Predictions across Queensland

``` r
source("code/predictions_at_centroids.R")

# Optional: Map + time series plot (QLD)
source("code/paper_fig_04.R")
```

**Note:** Results (tables, plots, rasters) will be written under outputs/ in case-specific subfolders when defined.

## 🦟 Interactive Shiny app

This repository includes a Shiny dashboard for exploring predicted *Anopheles farauti* hurdle-model abundance and suitability across Queensland.

#### 📦 Large data file (not stored in GitHub)

The Shiny app requires a large prediction file that is **not included in this repository** (too large for GitHub):

-   `centroids_5x5_qld_with_predictions_shiny-app.fst`

This file is hosted on the CSIRO Data Access Portal. 

**Steps**

1. Go to the CSIRO Data Access Portal record: https://data.csiro.au/collection/csiro:74972
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

The large predictions file used by the Shiny app is hosted on the CSIRO Data Access Portal:

Data record (centroids predictions): https://data.csiro.au/collection/csiro:74972

How to cite this work:

Mendiolar, Manuela; Hickson, Roslyn; Beeton, Nick; Powell, Francisca; Sexton, Justin; van den Hurk, Andrew; & Trewin, Brendan (2026). *R code for hurdle modelling of Anopheles farauti abundance in Queensland, Australia.* CSIRO. v1. Software. https://data.csiro.au/collection/csiro:75052