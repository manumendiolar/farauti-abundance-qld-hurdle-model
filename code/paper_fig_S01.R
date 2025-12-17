# ==============================================================================
# Relative abundance modelling for Anopheles farauti in QLD
# Checking correlation among predictors and response 
# Manuela, M. 
# 09-10-2025
# ==============================================================================

# Setup / Auxiliary functions --------------------------------------------------
source(here::here("code","00_setup.R"))

# Load species presence/absence data
pa <- ab_data <- read.csv(file.path(dir_data, "ab.csv"))  |> 
  filter(source %in% c("nigel","andrew")) |>  
  filter(region == "QLD") |> 
  dplyr::select(lon, lat, presence) |>
  group_by(lon, lat) |>
  slice_max(presence, with_ties = FALSE) |> 
  ungroup()

# Bowen & Mackay locations 
new_pt1 <- data.frame( lon = c(148.1430), lat = c(-19.9540), presence = c(1) )  
new_pt2 <- data.frame( lon = c(149.1190), lat = c(-21.1040), presence = c(1) )  
pa <- rbind(pa, new_pt1, new_pt2)

# Define occurrence species data
occ <- pa %>% filter(presence == 1) %>% dplyr::select(lon, lat)

# Convert to coordinates
presence_points <- pa[pa$presence == 1, c("lon", "lat")]
absence_points  <- pa[pa$presence == 0, c("lon", "lat")]

# Load climate and non-climate rasters + stack 
asc_files  <- list.files(dir_data, pattern = "\\.asc$", full.names = TRUE)
env_stack  <- stack(asc_files)

# Check with a plot
#plot(env_stack[[1:16]])
#plot(env_stack[[17:32]])
#plot(env_stack[[33:48]])

# Group labels
names_bio <- c(
  "anntemp",       # bio1
  "diurntemp",     # bio2
  "isotherm",      # bio3
  "seasontemp",    # bio4
  "maxtemp",       # bio5
  "mintemp",       # bio6
  "annrangetemp",  # bio7
  "tempwetq",      # bio8
  "tempdryq",      # bio9
  "tempwarmq",     # bio10
  "tempcoldq",     # bio11
  "annprecip",     # bio12
  "precipwetm",    # bio13 (wettest month)
  "precipdrym",    # bio14 (driest month)
  "seasonprecip",  # bio15 (precip seasonality)
  "precipwetq",    # bio16 (wettest quarter)
  "precipdryq",    # bio17 (driest quarter)
  "precipwarmq",   # bio18 (warmest quarter)
  "precipcoldq"    # bio19 (coldest quarter)
)

names_humidity <- c(
  "rh9ann", "rh9jan", "rh9jul", "rh3ann", "rh3jan", "rh3jul",  # relative humidity at 9 am / 3 pm from Copernicus (long-term mean 1979-2000)  
  "dp9ann", "dp9jan", "dp9jul", "dp3ann", "dp3jan", "dp3jul"   # dewpoint at 9 am / 3 pm from Copernicus (long-term mean 1970-2000) 
  )

names_water <- c(
  "water_occ",                    # water occurrence between 1984-2021 from JRC
  "water_occ_90", "water_occ_91", # derived from water occurrence between 1984-2021 from JRC
  "water_occ_92", "water_occ_93", "water_occ_94", "water_occ_95",
  "water_occ_96", "water_occ_97", "water_occ_98", "water_occ_99")

names_landscape <- c(
  "mangroves", "trees",  # landuse data from WorldCover (snapshot of present time)
  "dist_coast",          # derived
  "elev",                # from Worldclim 
  "aspect",              # derived from elev from Worldclim 
  "slope"                # derived from elev from Worldclim
)

names_all <- c(names_bio, names_humidity, "mangroves","trees","dist_coast","elev","water_occ", "water_occ_90", "water_occ_95", "water_occ_99") 

names_vars_used <- c(
  "diurntemp",     # bio2
  "seasontemp",    # bio4
  "maxtemp",       # bio5
  "mintemp",       # bio6
  "annrangetemp",  # bio7
  "annprecip",     # bio12
  "precipwetm",    # bio13 (wettest month)
  "precipdrym",    # bio14 (driest month)
  "seasonprecip",  # bio15 (precip seasonality)
  "rh3ann",        # relative humidity at 3 pm from Copernicus (long-term mean 1979-2000)  
  "dp3ann",        # dewpoint at 3 pm from Copernicus (long-term mean 1970-2000)
  "mangroves",     # landuse data from WorldCover (snapshot of present time)
  "trees",
  "water_occ",     # water occurrence between 1984-2021 from JRC
  "water_occ_99",  # derived from water occurrence between 1984-2021 from JRC
  "dist_coast",    # derived
  "elev"           # from Worldclim 
)


# Computing and saving
correlations_all <- raster.cor.matrix(env_stack[[ names_all ]])

# Save
write.csv(correlations_all, file.path(dir_tables, "corr_all.csv"))


# ------------------------------------------------------------------------------
#                            Corr Matrix plot
#                                All vars
# ------------------------------------------------------------------------------
p <- ggcorrplot(
  correlations_all,
  hc.order   = TRUE,
  type       = "lower",
  lab        = TRUE,
  lab_size   = 2.5,
  tl.cex     = 25,           # text label size
  show.diag  = FALSE
) +
  scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(-1, 1), name = "Pearson's\ncorrelation") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, face = 1),
    axis.text.y = element_text(size = 12, face = 1),
    legend.title    = element_text(face = "bold", size = 12),
    legend.text     = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    panel.grid      = element_blank(),
    axis.ticks      = element_blank()
  )

# Preview
print(p)

# Save with white background
ggsave(file.path(dir_plots, "corrmatrix_predictors_all.png"), plot = p, width = 15, height = 15, dpi = 400, bg = "white")
