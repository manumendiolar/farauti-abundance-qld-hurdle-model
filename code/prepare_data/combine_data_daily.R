# Preparing data to run models
# Manuela, M.
# 28-03-2025

# Combine all outputs from previous scripts 


# Libraries --------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(lubridate)
library(fst)

# Directories ------------------------------------------------------------------

# Local
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"


# Mozzie data ------------------------------------------------------------------
data_mozzie <- read.csv(paste0(dir_input, "mozzie/Farauti_numbers_North_QLD-Andrew_aggreg_trap.csv"))

# Check
head(data_mozzie)


# Static data ------------------------------------------------------------------
data_worldclim <- read.csv(paste0(dir_output, "climate/Farauti_ss_North_QLD_with_worldclim.csv"))
data_worldclim <- data_worldclim %>% select(UID, elevation, humidity, bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, 
                                            bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19, 
                                            trees, mangroves, water, wetland, grassland, shrubs, cropland, built, bare)
head(data_worldclim)

data_dist_coast <- read.csv(paste0(dir_output, "coastline/Farauti_ss_North_QLD_with_dist_to_coast.csv"))
data_dist_coast <- data_dist_coast %>% select(-species, -date, -week, -season, -habitat, -site, -lat, -lon, -site2, -year, -month, -day, -no.trap)
head(data_dist_coast)

data_water_cover <- read_fst(paste0(dir_output, "waterbodies/Farauti_ss_North_QLD_with_water_coverage_wideformat.fst"))
head(data_water_cover)
data_water_cover <- data_water_cover %>% 
  select(UID, 
         # 1km buffer
         freshwater_1km_pct, freshwater_1km_pct2, saline_1km_pct, saline_1km_pct2, salt_1km_pct, salt_1km_pct2,
         sea_1km_pct, sea_1km_pct2, waterbodies1_1km_pct, waterbodies1_1km_pct2, waterbodies2_1km_pct, waterbodies2_1km_pct2,
         # 2km buffer
         freshwater_2km_pct, freshwater_2km_pct2, saline_2km_pct, saline_2km_pct2, salt_2km_pct, salt_2km_pct2, sea_2km_pct, sea_2km_pct2,
         waterbodies1_2km_pct, waterbodies1_2km_pct2, waterbodies2_2km_pct, waterbodies2_2km_pct2,
         # 3km buffer
         freshwater_3km_pct, freshwater_3km_pct2, saline_3km_pct, saline_3km_pct2, salt_3km_pct, salt_3km_pct2, sea_3km_pct, sea_3km_pct2,
         waterbodies1_3km_pct, waterbodies1_3km_pct2, waterbodies2_3km_pct, waterbodies2_3km_pct2,
         # 4km buffer
         freshwater_4km_pct, freshwater_4km_pct2, saline_4km_pct, saline_4km_pct2, salt_4km_pct, salt_4km_pct2, sea_4km_pct, sea_4km_pct2,
         waterbodies1_4km_pct, waterbodies1_4km_pct2, waterbodies2_4km_pct, waterbodies2_4km_pct2,
         # 5km buffer
         freshwater_5km_pct, freshwater_5km_pct2, saline_5km_pct, saline_5km_pct2, salt_5km_pct, salt_5km_pct2, sea_5km_pct, sea_5km_pct2,
         waterbodies1_5km_pct, waterbodies1_5km_pct2, waterbodies2_5km_pct, waterbodies2_5km_pct2,
         # 6km buffer
         freshwater_6km_pct, freshwater_6km_pct2, saline_6km_pct, saline_6km_pct2, salt_6km_pct, salt_6km_pct2, sea_6km_pct, sea_6km_pct2,
         waterbodies1_6km_pct, waterbodies1_6km_pct2, waterbodies2_6km_pct, waterbodies2_6km_pct2,
         # 7km buffer
         freshwater_7km_pct, freshwater_7km_pct2, saline_7km_pct, saline_7km_pct2, salt_7km_pct, salt_7km_pct2, sea_7km_pct, sea_7km_pct2,
         waterbodies1_7km_pct, waterbodies1_7km_pct2, waterbodies2_7km_pct, waterbodies2_7km_pct2,
         # 8km buffer
         freshwater_8km_pct, freshwater_8km_pct2, saline_8km_pct, saline_8km_pct2, salt_8km_pct, salt_8km_pct2, sea_8km_pct, sea_8km_pct2,
         waterbodies1_8km_pct, waterbodies1_8km_pct2, waterbodies2_8km_pct, waterbodies2_8km_pct2,
         # 9km buffer
         freshwater_9km_pct, freshwater_9km_pct2, saline_9km_pct, saline_9km_pct2, salt_9km_pct, salt_9km_pct2, sea_9km_pct, sea_9km_pct2,
         waterbodies1_9km_pct, waterbodies1_9km_pct2, waterbodies2_9km_pct, waterbodies2_9km_pct2
  )
head(data_water_cover)

data_veg_cover <- read_fst(paste0(dir_output, "vegetation/Farauti_ss_North_QLD_with_veg_coverage_wideformat.fst"))
data_veg_cover <- data_veg_cover %>% 
  select(UID,
         mangroves_1km_pct, rainforest_1km_pct, cleared_1km_pct, euca1_1km_pct, acacia_1km_pct, sand_rock_1km_pct, mangroves_and_rainforest_1km_pct,
         mangroves_2km_pct, rainforest_2km_pct, cleared_2km_pct, euca1_2km_pct, acacia_2km_pct, sand_rock_2km_pct, mangroves_and_rainforest_2km_pct,
         mangroves_3km_pct, rainforest_3km_pct, cleared_3km_pct, euca1_3km_pct, acacia_3km_pct, sand_rock_3km_pct, mangroves_and_rainforest_3km_pct,
         mangroves_4km_pct, rainforest_4km_pct, cleared_4km_pct, euca1_4km_pct, acacia_4km_pct, sand_rock_4km_pct, mangroves_and_rainforest_4km_pct,
         mangroves_5km_pct, rainforest_5km_pct, cleared_5km_pct, euca1_5km_pct, acacia_5km_pct, sand_rock_5km_pct, mangroves_and_rainforest_5km_pct,
         mangroves_6km_pct, rainforest_6km_pct, cleared_6km_pct, euca1_6km_pct, acacia_6km_pct, sand_rock_6km_pct, mangroves_and_rainforest_6km_pct,
         mangroves_7km_pct, rainforest_7km_pct, cleared_7km_pct, euca1_7km_pct, acacia_7km_pct, sand_rock_7km_pct, mangroves_and_rainforest_7km_pct,
         mangroves_8km_pct, rainforest_8km_pct, cleared_8km_pct, euca1_8km_pct, acacia_8km_pct, sand_rock_8km_pct, mangroves_and_rainforest_8km_pct,
         mangroves_9km_pct, rainforest_9km_pct, cleared_9km_pct, euca1_9km_pct, acacia_9km_pct, sand_rock_9km_pct, mangroves_and_rainforest_9km_pct
  )
head(data_veg_cover)


# Daily data from NCI ----------------------------------------------------
data_precip <- read_fst(paste0(dir_output,"precipitation/Farauti_ss_North_QLD_with_NCI_precip_1995-1997.fst"))
data_tmax <- read_fst(paste0(dir_output,"temperature/Farauti_ss_North_QLD_with_NCI_tmax_1995-1997.fst"))
data_tmin <- read_fst(paste0(dir_output,"temperature/Farauti_ss_North_QLD_with_NCI_tmin_1995-1997.fst"))

# Inspect the structure of each dataset
head(data_precip)
head(data_tmax)
head(data_tmin)

# Select only necessary columns to avoid duplication
data_precip <- data_precip %>% select(UID, date, precip, accum_precip_21d)
data_tmax <- data_tmax %>% select(UID, date, tmax, mean_tmax_21d)
data_tmin <- data_tmin %>% select(UID, date, tmin, mean_tmin_21d)

# Merge the datasets
data_nci <- data_precip %>%
  left_join(data_tmax, by = c("UID", "date")) %>%
  left_join(data_tmin, by = c("UID", "date"))

# View the combined data
head(data_nci)

# Clean env.
rm(data_precip, data_tmax, data_tmin)
gc()


# Daily data from SILO ---------------------------------------------------
data_rh_tmax <- read_fst(paste0(dir_output, "humidity/SILO/Farauti_ss_North_QLD_with_SILO_rh_tmax_1995-1997.fst"))
data_rh_tmin <- read_fst(paste0(dir_output, "humidity/SILO/Farauti_ss_North_QLD_with_SILO_rh_tmin_1995-1997.fst"))

# Inspect the structure of each dataset
head(data_rh_tmax)
head(data_rh_tmin)

# Select only necessary columns to avoid duplication
data_rh_tmax <- data_rh_tmax %>% select(UID, date, rh_tmax, mean_rh_tmax_21d)
data_rh_tmin <- data_rh_tmin %>% select(UID, date, rh_tmin, mean_rh_tmin_21d)

# Merge the datasets
data_silo <- data_rh_tmax %>% left_join(data_rh_tmin, by = c("UID", "date")) 

# View the combined data
head(data_silo)

# Clean env.
rm(data_rh_tmax, data_rh_tmin)
gc()


# Daily data from Copernicus ---------------------------------------------
data_copernicus <- read_fst(paste0(dir_output, "humidity/Copernicus/Farauti_ss_North_QLD_with_Copernicus_rh_1995-1997.fst"))
data_copernicus <- data_copernicus %>% select(UID, date, rh, mean_rh_21d)
head(data_copernicus)


# Combine all ------------------------------------------------------------------
data_merged <- data_nci %>%
  left_join(data_silo, by = c("UID", "date")) %>%
  left_join(data_copernicus, by = c("UID", "date")) %>% 
  left_join(data_worldclim, by = c("UID")) %>%
  left_join(data_dist_coast, by = c("UID")) %>%
  left_join(data_water_cover, by = c("UID")) %>%
  left_join(data_veg_cover, by = c("UID")) 

# Check
head(data_merged)

# Check # days (these should coincide)
length(unique(data_merged$date))
length(unique(data_mozzie$date))

# Mozzie data + predictors ----------------------------------------------------- 
data_merged$date <- as.Date(data_merged$date)
data_mozzie$date <- as.Date(data_mozzie$date)
data_mozzie_updated <- data_mozzie %>% left_join(data_merged, by = c("UID","date"))
head(data_mozzie_updated)

# Clean environment
rm(data_nci, data_silo, data_copernicus, data_worldclim, data_dist_coast, data_water_cover, data_veg_cover)
rm(data_merged)
gc()


# Add region column ------------------------------------------------------------
data_mozzie_updated <- data_mozzie_updated %>% mutate(region = "QLD")
head(data_mozzie_updated)


# Add presence column ----------------------------------------------------------
data_mozzie_updated <- data_mozzie_updated %>% mutate(presence = ifelse(no_trap_mod > 0, 1, 0)) 
head(data_mozzie_updated)



# Add mean precip, tmax, tmin, rh_tmax, rh_tmin and rh over season (dry/wet) and 
# season 2 (early/late dry/wet) ------------------------------------------------
# From data_mozzie_updated monthly (already generated)
data_mozzie_updated_monthly <- read_fst(paste0(dir_output, "mozzie/Farauti_numbers_North_QLD-Andrew_with_covariates_monthly.fst"))
head(data_mozzie_updated_monthly)

# Identify the last 36 columns from data_mozzie_updated_monthly
monthly_cols <- tail(names(data_mozzie_updated_monthly), 36)

# Combine with cbind
data_mozzie_updated <- cbind(data_mozzie_updated, data_mozzie_updated_monthly[, monthly_cols])

# Check
head(data_mozzie_updated)



# Save final data set ----------------------------------------------------------
write_fst(
  data_mozzie_updated,
  paste0(dir_output, "mozzie/Farauti_numbers_North_QLD-Andrew_with_covariates_daily.fst"),
  compress = 50
)


