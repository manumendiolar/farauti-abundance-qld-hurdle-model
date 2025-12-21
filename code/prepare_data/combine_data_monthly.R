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
data_dist_coast <- data_dist_coast %>% dplyr::select(-species, -date, -week, -season, -habitat, -site, -lat, -lon, -site2, -year, -month, -day, -no.trap)
head(data_dist_coast)

data_water_cover <- read_fst(paste0(dir_output, "waterbodies/Farauti_ss_North_QLD_with_water_coverage_wideformat.fst"))
head(data_water_cover)
data_water_cover <- data_water_cover %>% 
  dplyr::select(UID, 
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
  dplyr::select(UID,
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


# Monthly data from Worldclim --------------------------------------------------
data_precip <- read_fst(paste0(dir_output,"precipitation/Farauti_numbers_North_QLD-Andrew_with_worldclim_precipitation.fst"))
data_tmax <- read_fst(paste0(dir_output,"temperature/Farauti_numbers_North_QLD-Andrew_with_worldclim_tmax.fst"))
data_tmin <- read_fst(paste0(dir_output,"temperature/Farauti_numbers_North_QLD-Andrew_with_worldclim_tmin.fst"))

# Inspect the structure of each dataset
head(data_precip)
head(data_tmax)
head(data_tmin)

# Select only necessary columns to avoid duplication
data_precip <- data_precip %>% dplyr::select(UID, precip_1, precip_2, precip_3, precip_4, precip_5, precip_6, precip_7, precip_8, precip_9, precip_10, precip_11, precip_12)
data_tmax <- data_tmax %>% dplyr::select(UID, tmax_1, tmax_2, tmax_3, tmax_4, tmax_5, tmax_6, tmax_7, tmax_8, tmax_9, tmax_10, tmax_11, tmax_12)
data_tmin <- data_tmin %>% dplyr::select(UID, tmin_1, tmin_2, tmin_3, tmin_4, tmin_5, tmin_6, tmin_7, tmin_8, tmin_9, tmin_10, tmin_11, tmin_12)

# Inspect the structure of each dataset
head(data_precip)
head(data_tmax)
head(data_tmin)


# Monthly data from SILO ---------------------------------------------------
data_rh_tmax <- read_fst(paste0(dir_output, "humidity/Farauti_numbers_North_QLD-Andrew_with_SILO_monthly_mean_rh_tmax.fst"))
data_rh_tmin <- read_fst(paste0(dir_output, "humidity/Farauti_numbers_North_QLD-Andrew_with_SILO_monthly_mean_rh_tmin.fst"))

# Inspect the structure of each dataset
head(data_rh_tmax)
head(data_rh_tmin)

# Select only necessary columns to avoid duplication
data_rh_tmax <- data_rh_tmax %>% dplyr::select(UID, rh_tmax_1, rh_tmax_2, rh_tmax_3, rh_tmax_4, rh_tmax_5, rh_tmax_6, rh_tmax_7, rh_tmax_8, rh_tmax_9, rh_tmax_10, rh_tmax_11, rh_tmax_12)
data_rh_tmin <- data_rh_tmin %>% dplyr::select(UID, rh_tmin_1, rh_tmin_2, rh_tmin_3, rh_tmin_4, rh_tmin_5, rh_tmin_6, rh_tmin_7, rh_tmin_8, rh_tmin_9, rh_tmin_10, rh_tmin_11, rh_tmin_12)

# Check
head(data_rh_tmax)
head(data_rh_tmin)


# Monthly data from Copernicus -------------------------------------------------
data_rh <- read_fst(paste0(dir_output, "humidity/Farauti_numbers_North_QLD-Andrew_with_Copernicus_monthly_mean_rh.fst"))

# Check
head(data_rh)

# Select only necessary columns to avoid duplication
data_rh <- data_rh %>% dplyr::select(UID, rh_1, rh_2, rh_3, rh_4, rh_5, rh_6, rh_7, rh_8, rh_9, rh_10, rh_11, rh_12)

# Check
head(data_rh)


# Combine all ------------------------------------------------------------------
data_merged <- data_precip %>%
  left_join(data_tmax, by = "UID") %>%
  left_join(data_tmin, by = "UID") %>% 
  left_join(data_rh_tmax, by = "UID") %>%
  left_join(data_rh_tmin, by = "UID") %>%
  left_join(data_rh, by = "UID") %>%
  left_join(data_worldclim, by = "UID") %>%
  left_join(data_dist_coast, by = "UID") %>%
  left_join(data_water_cover, by = "UID") %>%
  left_join(data_veg_cover, by = "UID") 

# Check
glimpse(data_merged)


# Mozzie data + predictors -----------------------------------------------------
data_mozzie_updated <- data_mozzie %>% left_join(data_merged, by = "UID")
head(data_mozzie_updated)

# Clean environment
rm(data_precip, data_tmax, data_tmin, data_rh_tmax, data_rh_tmin, data_rh, 
   data_worldclim, data_dist_coast, data_water_cover, data_veg_cover
)
rm(data_merged)


# Add region column ------------------------------------------------------------
data_mozzie_updated <- data_mozzie_updated %>% mutate(region = "QLD")
head(data_mozzie_updated)


# Add presence column ----------------------------------------------------------
data_mozzie_updated <- data_mozzie_updated %>% mutate(presence = ifelse(no_trap_mod > 0, 1, 0)) 
head(data_mozzie_updated)


# Add mean precip, tmax, tmin, rh_tmax, rh_tmin and rh over season (dry/wet) and 
# season 2 (early/late dry/wet) ------------------------------------------------
# From data_mozzie_updated monthly (already generated)
data_mozzie_updated <- data_mozzie_updated %>%
  mutate(
    precip_EW = rowMeans(dplyr::select(., precip_12, precip_1, precip_2), na.rm = TRUE),
    precip_LW = rowMeans(dplyr::select(., precip_3, precip_4, precip_5), na.rm = TRUE),
    precip_W = rowMeans(dplyr::select(., precip_12, precip_1, precip_2, precip_3, precip_4, precip_5), na.rm = TRUE),
    precip_ED = rowMeans(dplyr::select(., precip_6, precip_7, precip_8), na.rm = TRUE),
    precip_LD = rowMeans(dplyr::select(., precip_9, precip_10, precip_11), na.rm = TRUE),
    precip_D = rowMeans(dplyr::select(., precip_6, precip_7, precip_8, precip_9, precip_10, precip_11), na.rm = TRUE),
    
    tmin_EW = rowMeans(dplyr::select(., tmin_12, tmin_1, tmin_2), na.rm = TRUE),
    tmin_LW = rowMeans(dplyr::select(., tmin_3, tmin_4, tmin_5), na.rm = TRUE),
    tmin_W = rowMeans(dplyr::select(., tmin_12, tmin_1, tmin_2, tmin_3, tmin_4, tmin_5), na.rm = TRUE),
    tmin_ED = rowMeans(dplyr::select(., tmin_6, tmin_7, tmin_8), na.rm = TRUE),
    tmin_LD = rowMeans(dplyr::select(., tmin_9, tmin_10, tmin_11), na.rm = TRUE),
    tmin_D = rowMeans(dplyr::select(., tmin_6, tmin_7, tmin_8, tmin_9, tmin_10, tmin_11), na.rm = TRUE),
    
    tmax_EW = rowMeans(dplyr::select(., tmax_12, tmax_1, tmax_2), na.rm = TRUE),
    tmax_LW = rowMeans(dplyr::select(., tmax_3, tmax_4, tmax_5), na.rm = TRUE),
    tmax_W = rowMeans(dplyr::select(., tmax_12, tmax_1, tmax_2, tmax_3, tmax_4, tmax_5), na.rm = TRUE),
    tmax_ED = rowMeans(dplyr::select(., tmax_6, tmax_7, tmax_8), na.rm = TRUE),
    tmax_LD = rowMeans(dplyr::select(., tmax_9, tmax_10, tmax_11), na.rm = TRUE),
    tmax_D = rowMeans(dplyr::select(., tmax_6, tmax_7, tmax_8, tmax_9, tmax_10, tmax_11), na.rm = TRUE),
    
    rh_tmin_EW = rowMeans(dplyr::select(., rh_tmin_12, rh_tmin_1, rh_tmin_2), na.rm = TRUE),
    rh_tmin_LW = rowMeans(dplyr::select(., rh_tmin_3, rh_tmin_4, rh_tmin_5), na.rm = TRUE),
    rh_tmin_W = rowMeans(dplyr::select(., rh_tmin_12, rh_tmin_1, rh_tmin_2, rh_tmin_3, rh_tmin_4, rh_tmin_5), na.rm = TRUE),
    rh_tmin_ED = rowMeans(dplyr::select(., rh_tmin_6, rh_tmin_7, rh_tmin_8), na.rm = TRUE),
    rh_tmin_LD = rowMeans(dplyr::select(., rh_tmin_9, rh_tmin_10, rh_tmin_11), na.rm = TRUE),
    rh_tmin_D = rowMeans(dplyr::select(., rh_tmin_6, rh_tmin_7, rh_tmin_8, rh_tmin_9, rh_tmin_10, rh_tmin_11), na.rm = TRUE),
    
    rh_tmax_EW = rowMeans(dplyr::select(., rh_tmax_12, rh_tmax_1, rh_tmax_2), na.rm = TRUE),
    rh_tmax_LW = rowMeans(dplyr::select(., rh_tmax_3, rh_tmax_4, rh_tmax_5), na.rm = TRUE),
    rh_tmax_W = rowMeans(dplyr::select(., rh_tmax_12, rh_tmax_1, rh_tmax_2, rh_tmax_3, rh_tmax_4, rh_tmax_5), na.rm = TRUE),
    rh_tmax_ED = rowMeans(dplyr::select(., rh_tmax_6, rh_tmax_7, rh_tmax_8), na.rm = TRUE),
    rh_tmax_LD = rowMeans(dplyr::select(., rh_tmax_9, rh_tmax_10, rh_tmax_11), na.rm = TRUE),
    rh_tmax_D = rowMeans(dplyr::select(., rh_tmax_6, rh_tmax_7, rh_tmax_8, rh_tmax_9, rh_tmax_10, rh_tmax_11), na.rm = TRUE),
    
    rh_EW = rowMeans(dplyr::select(., rh_12, rh_1, rh_2), na.rm = TRUE),
    rh_LW = rowMeans(dplyr::select(., rh_3, rh_4, rh_5), na.rm = TRUE),
    rh_W = rowMeans(dplyr::select(., rh_2, rh_1, rh_2, rh_3, rh_4, rh_5), na.rm = TRUE),
    rh_ED = rowMeans(dplyr::select(., rh_6, rh_7, rh_8), na.rm = TRUE),
    rh_LD = rowMeans(dplyr::select(., rh_9, rh_10, rh_11), na.rm = TRUE),
    rh_D = rowMeans(dplyr::select(., rh_6, rh_7, rh_8, rh_9, rh_10, rh_11), na.rm = TRUE)
  )
head(data_mozzie_updated)


# Save final data set ----------------------------------------------------------
write_fst(
  data_mozzie_updated,
  paste0(dir_output, "mozzie/Farauti_numbers_North_QLD-Andrew_with_covariates_monthly.fst"),
  compress = 50
)




