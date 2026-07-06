# `data/` — datasets and data dictionary

This folder holds the inputs for the *Anopheles farauti* abundance hurdle model.

> **Units / definitions marked `(confirm)` are inferred from the code and should be
> checked by the authors before release.**

## Data availability

The tabular collection data (`ab.csv`, `occ.csv`) are **not tracked on GitHub**
(see the repository `.gitignore`). The canonical, citable copies are archived in
the CSIRO Data Access Portal: the code release
([csiro:75052](https://data.csiro.au/collection/csiro:75052)) and the centroids
prediction file used by the Shiny app, held as a separate data collection
([csiro:74972](https://data.csiro.au/collection/csiro:74972)); see the top-level
`README.md`.

Environmental raster layers (`*.asc`), the prediction grid
(`centroids_5x5_qld.fst`) and boundary files (`maps/`) are also excluded from
GitHub by size but are included in the portal release.

---

## Files

| File | What it is | Used by |
|------|------------|---------|
| `ab.csv` | Main modelling table: trap collections of *An. farauti* with matched environmental covariates (1027 records). | `01_dist_models.R`, `02_abund_models.R`, `03_zi_models.R`, `paper_fig_*` |
| `bg_points.csv` | Background points (`lon`,`lat`) for the presence–background distribution models. | `01_dist_models.R` |
| `occ.csv` | 65 `lon`/`lat` coordinates. **Not read by any pipeline script** — confirm its purpose or remove before release. | — |
| `centroids_5x5_qld.fst` | 5 × 5 km Queensland prediction grid with time keys and covariates. | `predictions_at_centroids.R` |
| `centroids_with_predictions.fst` | Appears to be a legacy prediction file (the current pipeline writes to `outputs/predictions/`). **Confirm / remove.** | — |
| `*.asc` | Environmental predictor rasters for Queensland (see groups below). CRS EPSG:4326, ~1 km. | `01_dist_models.R`, `paper_fig_S01.R` |
| `maps/` | GADM v4.1 Australia administrative boundaries (`gadm41_AUS_*`). | `predictions_at_centroids.R`, figures |

### Raster layers (`*.asc`)
- **Topography:** `elev`, `slope`, `aspect`, `dist_coast`
- **Bioclimatic / climate:** `anntemp`, `annrangetemp`, `diurntemp`, `isotherm`, `maxtemp`, `mintemp`, `seasontemp`, `tempwetq`, `tempdryq`, `tempwarmq`, `tempcoldq`, `annprecip`, `precipwetm`, `precipdrym`, `precipwetq`, `precipdryq`, `precipwarmq`, `precipcoldq`, `seasonprecip`
- **Humidity / dewpoint:** `rh3ann`, `rh3jan`, `rh3jul`, `rh9ann`, `rh9jan`, `rh9jul`, `dp3ann`, `dp3jan`, `dp3jul`, `dp9ann`, `dp9jan`, `dp9jul`
- **Surface water (JRC GSW):** `water_occ`, `water_occ_90` … `water_occ_99`
- **Land cover:** `trees`, `mangroves`

---

## `ab.csv` columns used in the analysis

`ab.csv` contains ~280 columns — most are *candidate / derived* predictors
computed at multiple radii (1–9 km) and temporal windows (monthly lags, seasonal
aggregates). The analysis uses the subset below (see the `dplyr::select()` in
`02_abund_models.R` and the predictor vectors in `01_dist_models.R`).

**Identifiers & keys**
| Column | Definition |
|--------|------------|
| `ID` | Unique record identifier |
| `lon`, `lat` | Site coordinates, decimal degrees (EPSG:4326) |
| `date` | Collection date, formatted **`d/m/Y`** in the CSV (parsed as such in code) |
| `year`, `month`, `day`, `week` | Date components (used as factors in models) |
| `season` | Dry / wet |
| `season2` | Early/late dry and early/late wet |
| `method` | Collection method (`T` = trap) |
| `region` | Survey region (`QLD`) |
| `source` | Data source (e.g. `andrew`, `nigel`) |
| `ID_type`, `site`, `site2` | Site metadata / labels |
| `habitat` | Habitat type: high altitude / brackish / freshwater |

**Responses**
| Column | Definition |
|--------|------------|
| `presence` | Presence/absence (0/1) — response for the distribution models |
| `count` | Number of *An. farauti* per trap — response for the abundance/ZI models |

**Environmental predictors**
| Column | Definition | Units `(confirm)` |
|--------|------------|-------------------|
| `ppa21` | Accumulated precipitation over the 21 days prior to collection | mm |
| `tmaxm21` | Mean maximum temperature over the 21 days prior | °C |
| `tminm21` | Mean minimum temperature over the 21 days prior | °C |
| `rhm21` | Mean relative humidity over the 21 days prior | % |
| `elev` | Elevation | m `(confirm)` |
| `dist_coast` | Distance from site to coastline | km |
| `water_occ` | Surface-water occurrence frequency, Mar 1984–Dec 2021 (JRC GSW) | % (0–100) |
| `water_occ_99` | Distance to nearest cell with 99% water-occurrence frequency | km `(confirm units)` |
| `mang_rf_5km` | Mangrove + rainforest cover within a 5 km radius | % / proportion `(confirm)` |

**Standardised predictors** (`*_z`): `ppa21_z`, `tmaxm21_z`, `tminm21_z`,
`rhm21_z`, `elev_z`, `mang_rf_5km_z`, `water_occ_z`, `water_occ_99_z` — each is the
raw variable divided by its maximum (used by the zero-inflated models in
`03_zi_models.R`).

---

## Regenerating the covariates

The `*.asc` rasters and the environmental columns in `ab.csv` are produced by the
scripts in [`../code/prepare_data/`](../code/prepare_data/), which pull from
external sources (WorldClim, SILO, Copernicus ERA5, JRC, NCI). That stage is
**optional** and is **not** part of the reproducible core pipeline (`runME.R`) —
see [`../code/prepare_data/README.md`](../code/prepare_data/README.md).
