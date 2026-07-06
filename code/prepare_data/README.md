# `prepare_data/` — building the environmental covariates (optional stage)

These scripts regenerate the environmental covariates used by the model: the
`*.asc` prediction rasters in `../../data/`, the covariate columns in
`data/ab.csv`, and the prediction grid `data/centroids_5x5_qld.fst`.

> **This stage is OPTIONAL and is _not_ part of the reproducible core pipeline
> (`runME.R`).** It depends on large external datasets and on file paths from the
> machine where the data were originally processed (hardcoded `Z:/work/...` HPC
> paths). The prepared outputs are already provided in the data release, so you
> do **not** need to run these scripts to reproduce the paper's models, figures
> or tables.

## Running these scripts (only if you want to rebuild covariates)

1. Obtain the source datasets (see table below) from their providers.
2. At the top of each script, set `dir_input` / `dir_output` to your own paths
   (they currently point at the original HPC location `Z:/work/...`).
3. Run the relevant script(s). They are largely independent; the `combine_*`
   scripts merge the per-source outputs, and the model table `ab.csv` is the
   final merged product.

## External data sources

| Source | Provides | URL |
|--------|----------|-----|
| **WorldClim 2.1** | Elevation, bioclimatic variables, monthly precip/tmax/tmin | https://www.worldclim.org/ |
| **ESA WorldCover** | Land-cover fractions (trees, mangroves, water, wetland, grassland, shrubs, cropland, built, bare) | https://esa-worldcover.org/ |
| **SILO** (Qld Gov / Long Paddock) | Daily & monthly climate incl. relative humidity, tmax, tmin | https://www.longpaddock.qld.gov.au/silo/ |
| **Copernicus / ERA5** (CDS) | 2 m relative humidity / dewpoint | https://cds.climate.copernicus.eu/ |
| **JRC Global Surface Water** | Surface-water occurrence (1984–2021) | https://global-surface-water.appspot.com/ |
| **NCI** (National Computational Infrastructure) | Gridded daily climate (precipitation, tmax, tmin) | https://nci.org.au/ |
| **GADM v4.1** | Australia administrative boundaries | https://gadm.org/ |

## Scripts by purpose

**Prediction grid & geometry**
- `compute_grid_with_centroids.R` — build the 5 × 5 km Queensland grid → `centroids_5x5_qld.fst`
- `prepare_distance_to_coastline.R`, `compute_dist_coast_raster.R` — `dist_coast`
- `prepare_distance_to_waterbodies.R`, `prepare_distance_to_waterbodies_brackish.R`,
  `prepare_distance_to_water_occ_90-99.R` — distance-to-water metrics (incl. `water_occ_99`)

**Climate — daily near-survey windows (21-day covariates)**
- `prepare_precipitation_NCI_daily.R` — `ppa21` (NCI)
- `prepare_tmax_NCI_daily.R` — `tmaxm21` (NCI)
- `prepare_tmin_NCI_daily.R` — `tminm21` (NCI)
- `prepare_rh_tmax_SILO_daily.R` — relative humidity (SILO)
- `prepare_rh_Copernicus_daily.R` — relative humidity (Copernicus/ERA5)
- `combine_data_daily.R` — merge the daily NCI + SILO + Copernicus outputs

**Climate — monthly / bioclimatic**
- `prepare_precipitation_WorldClim_monthly.R`, `prepare_tmax_WorldClim_monthly.R`,
  `prepare_tmin_WorldClim_monthly.R` — WorldClim monthly climate
- `prepare_rh_tmax_SILO_monthly.R`, `prepare_rh_Copernicus_monthly.R` — monthly relative humidity
- `prepare_data_worldclim.R` — WorldClim bioclim + elevation and ESA WorldCover land-cover rasters
- `combine_data_monthly.R` — merge the monthly SILO + Copernicus outputs

**Land cover & water**
- `prepare_water_occ_JRC.R` — JRC surface-water occurrence (`water_occ`, `water_occ_90`…`water_occ_99`)
- `prepare_vegetation_coverage.R` — vegetation cover (trees / mangroves / rainforest → `mang_rf_*`)
- `prepare_wetland_coverage.R` — wetland cover

**Other**
- `imputation_no_trap.R` — impute missing covariates at non-trap locations
- `DEA.R` — exploratory data visualisation (not a covariate-building step)
