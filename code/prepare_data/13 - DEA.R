# Farauti_ss_North_QLD exploratory
# Manuela, M.
# 15-03-2025


# Libraries --------------------------------------------------------------------
library(spatialRF)
library(randomForest)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(patchwork)
library(tmap)
library(sf)
library(fst)
library(leaflet)


# Directories ------------------------------------------------------------------
dir_input <- "Z:/work/data/"
dir_output <- "Z:/work/outputs/"


# Mozzie data + predictors -----------------------------------------------------
data <- read_fst(paste0(dir_output, "mozzie/Farauti_numbers_North_QLD-Andrew_with_covariates_daily.fst"))
glimpse(data)


# Formatting
data$fsite <- factor(data$site, levels = c("BM1", "BM2", "K1", "K2", "C1", "C2", "RR1", "RR2", "E1", "E2", "M1", "M2"))
data$season_year <- factor(data$season_year, 
                           levels = c("W-1995","D-1995","W-1996","D-1996","W-1997","D-1997"))
data$season2_year <- factor(data$season2_year, 
                            levels = c("EW-1995","LW-1995","ED-1995","LD-1995",
                                       "EW-1996","LW-1996","ED-1996","LD-1996",
                                       "EW-1997","LW-1997","ED-1997","LD-1997"))
data$month_year <- factor(data$month_year, 
                          levels = c("8-1995","9-1995","10-1995","11-1995","12-1995",
                                     "1-1996","2-1996","3-1996","4-1996","5-1996","6-1996","7-1996","8-1996","9-1996","10-1996","11-1996",
                                     "1-1997","2-1997","3-1997","4-1997","5-1997","6-1997","7-1997","8-1997","9-1997"))



# Number of observations
with(data, table(month, year))
with(data, table(season2, year))
with(data, table(season, year))


# Number of mosquitoes per trap by site ----------------------------------------
ggplot(data, aes(x = date, y = no_trap_mod, group = fsite, col = habitat)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  facet_wrap(~fsite, scales = "free_y") +
  labs(title = "At each location", x = "Date", y = "Number of Mosquitoes per trap", col = "Habitat",
    caption = "Source: Farauti_ss_North_QLD_with_covariates.csv") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot to a file (e.g., PNG)
ggsave("results/mozzies_over_time_per_site.png", width = 12, height = 8, dpi = 300)


# Total number of mosquitoes per trap by date ----------------------------------
data_aggregated <- data %>%
  group_by(date) %>%
  summarise(total_no_trap_mod = sum(no_trap_mod, na.rm = TRUE))

# Create the plot
ggplot(data_aggregated, aes(x = date, y = total_no_trap_mod)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  labs(title = "", x = "Date", y = "Total Number of Mosquitoes per trap",
       caption = "Source: Farauti_ss_North_QLD_with_covariates.csv") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("results/mozzies_over_time.png", width = 12, height = 8, dpi = 300)


# Precipitation ----------------------------------------------------------------
ggplot(data, aes(x = date, y = precip, group = fsite, fill = habitat)) +
  geom_col() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
  facet_wrap(~fsite) +
  labs(title = "Precipitation over time at each location", x = "Date", 
       y = "Total rainfall (mm)", caption = "Source: NCI") +
  theme_minimal(base_size = 10) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("results/total_precip_over_time_per_site.png", width = 12, height = 8, dpi = 300)

# Accumulated precipitation 
ggplot(data, aes(x = date, y = accum_precip_21d, group = fsite, fill = habitat)) +
  geom_col() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
  facet_wrap(~fsite) +
  labs(title = "Accumulated precipitation over 21 days prior at each location", 
       x = "Date", y = "Total rainfall (mm)", caption = "Source: NCI") +
  theme_minimal(base_size = 10) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("results/total_precip_21d_over_time_per_site.png", width = 12, height = 8, dpi = 300)

# Relationship with mozzies
ggplot(data, aes(x = precip, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(title = "All observations", x = "Precipitation (mm)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
ggsave("results/mozzies_vs_precip_all_obs.png", width = 12, height = 8, dpi = 300)

ggplot(subset(data, no_trap_mod > 0), aes(x = precip, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + 
  labs(title = "Only positive observations", x = "Precipitation (mm)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
ggsave("results/mozzies_vs_precip_positive_obs.png", width = 12, height = 8, dpi = 300)


ggplot(data, aes(x = accum_precip_21d, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(title = "All observations", x = "Accumulated precipitation over 21 days prior (mm)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
ggsave("results/mozzies_vs_precip_21d_all_obs.png", width = 12, height = 8, dpi = 300)

ggplot(subset(data, no_trap_mod > 0), aes(x = accum_precip_21d, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + 
  labs(title = "Only positive observations", x = "Accumulated precipitation over 21 days prior (mm)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
ggsave("results/mozzies_vs_precip_21d_positive_obs.png", width = 12, height = 8, dpi = 300)


# Temperature ------------------------------------------------------------------
ggplot(data) +
  geom_line(aes(x = date, y = tmax, group = fsite, col = habitat), size = 0.5) +
  geom_line(aes(x = date, y = tmin, group = fsite, col = habitat), size = 0.5, linetype = 2) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
  facet_wrap(~fsite) +
  labs(title = "Maximum temperature (solid) and Minimum temperature (dashed)", 
       x = "Date", y = "Temperature (C)", col = "Habitat", caption = "Source: NCI") +
  theme_minimal(base_size = 10) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("results/temp_over_time_per_site.png", width = 12, height = 8, dpi = 300)



# Mean temperature over 21d
ggplot(data) +
  geom_line(aes(x = date, y = mean_tmax_21d, group = fsite, col = habitat), size = 0.5) +
  geom_line(aes(x = date, y = mean_tmin_21d, group = fsite, col = habitat), size = 0.5, linetype = 2) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
  facet_wrap(~fsite) +
  labs(title = "Maximum temperature (solid) and Minimum temperature (dashed)", 
       x = "Date", y = "Mean temperature over 21 days prior (C)", 
       col = "Habitat", caption = "Source: NCI") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("results/temp_21d_over_time_per_site.png", width = 12, height = 8, dpi = 300)


# Relationship with mozzies
ggplot(data, aes(x = tmax, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(title = "All observations", x = "Maximum temperature (C)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
ggsave("results/mozzies_vs_temp_all_obs.png", width = 12, height = 8, dpi = 300)


ggplot(subset(data, no_trap_mod > 0), aes(x = tmax, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + 
  labs(title = "Only positive observations", x = "Maximum temperature (C)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
ggsave("results/mozzies_vs_temp_positive_obs.png", width = 12, height = 8, dpi = 300)

ggplot(data, aes(x = tmin, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(title = "All observations", x = "Minimum temperature (C)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(subset(data, no_trap_mod > 0), aes(x = tmin, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + 
  labs(title = "Only positive observations", x = "Minimum temperature (C)", y = "Number of mosquitoes per trap", caption = "Source: NCI and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Humidity ---------------------------------------------------------------------
ggplot(data) +
  geom_line(aes(x = date, y = rh, group = fsite, col = habitat, linetype = "rh"), size = 0.5, ) +
  geom_line(aes(x = date, y = mean_rh_21d, group = fsite, col = habitat, linetype = "mean_rh_21d"), size = 0.5, ) +
  geom_line(aes(x = date, y = rh_tmax, group = fsite, col = habitat, linetype = "rh_tmax"), size = 0.5) +
  geom_line(aes(x = date, y = rh_tmin, group = fsite, col = habitat, linetype = "rh_tmin"), size = 0.5) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
  facet_wrap(~fsite) +
  labs(title = "Relative humidity over time at each location", x = "Date", y = "Relative humidity (%)", col = "Habitat", linetype = "", caption = "Source: SILO and Copernicus") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
# Relationship with mozzies
ggplot(data, aes(x = rh, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(title = "All observations", x = "Relative humidity (%)", y = "Number of mosquitoes per trap", caption = "Source: Copernicus and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(subset(data, no_trap_mod > 0), aes(x = rh, y = no_trap_mod)) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(title = "Only positive observations", x = "Relative humidity (%)", y = "Number of mosquitoes per trap", caption = "Source: Copernicus and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Distance to coastline --------------------------------------------------------
ggplot(data) +
  geom_point(aes(x = dist_coast_km, y = no_trap_mod), size = 1) +
  geom_smooth(aes(x = dist_coast_km, y = no_trap_mod), method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(x = "Distance to coastline (km)", y = "Number of mosquitoes per trap", caption = "") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Elevation --------------------------------------------------------------------
ggplot(data, aes(x = elevation, y = no_trap_mod)) +
  geom_point( size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 0.5) + # Add smoother
  labs(x = "Elevation (m)", y = "Number of mosquitoes per trap", caption = "Source: Worldclim and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Vegetation coverage ----------------------------------------------------------

# Mangroves and mosquitoes
ggplot(data) +
  geom_point(aes(x = mangroves_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = mangroves_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = mangroves_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = mangroves_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = mangroves_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = mangroves_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = mangroves_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = mangroves_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = mangroves_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "All observations", x = "Mangroves coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(subset(data, no_trap_mod > 0)) +
  geom_point(aes(x = mangroves_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = mangroves_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = mangroves_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = mangroves_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = mangroves_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = mangroves_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = mangroves_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = mangroves_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = mangroves_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "Only positive observations", x = "Mangroves coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Rainforest and mosquitoes
ggplot(data) +
  geom_point(aes(x = rainforest_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = rainforest_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = rainforest_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = rainforest_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = rainforest_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = rainforest_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = rainforest_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = rainforest_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = rainforest_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "All observations", x = "Rainforest coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(subset(data, no_trap_mod > 0)) +
  geom_point(aes(x = rainforest_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = rainforest_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = rainforest_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = rainforest_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = rainforest_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = rainforest_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = rainforest_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = rainforest_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = rainforest_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "Only positive observations", x = "Rainforest coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Cleared and mosquitoes
ggplot(data) +
  geom_point(aes(x = cleared_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = cleared_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = cleared_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = cleared_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = cleared_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = cleared_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = cleared_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = cleared_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = cleared_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "All observations", x = "Cleared coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(subset(data, no_trap_mod > 0)) +
  geom_point(aes(x = cleared_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = cleared_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = cleared_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = cleared_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = cleared_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = cleared_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = cleared_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = cleared_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = cleared_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "Only positive observations", x = "Cleared coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))



# Waterbodies coverage ---------------------------------------------------------

# Waterbodies1 combination and mosquitoes
ggplot(data) +
  geom_point(aes(x = waterbodies1_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = waterbodies1_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = waterbodies1_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = waterbodies1_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = waterbodies1_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = waterbodies1_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = waterbodies1_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = waterbodies1_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = waterbodies1_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "All observations", x = "Waterbodies1 coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(subset(data, no_trap_mod > 0)) +
  geom_point(aes(x = waterbodies1_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = waterbodies1_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = waterbodies1_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = waterbodies1_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = waterbodies1_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = waterbodies1_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = waterbodies1_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = waterbodies1_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = waterbodies1_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "Only positive observations", x = "Waterbodies1 coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Waterbodies2 combination and mosquitoes
ggplot(data) +
  geom_point(aes(x = waterbodies2_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = waterbodies2_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = waterbodies2_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = waterbodies2_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = waterbodies2_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = waterbodies2_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = waterbodies2_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = waterbodies2_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = waterbodies2_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "All observations",x = "Waterbodies2 coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(subset(data, no_trap_mod > 0)) +
  geom_point(aes(x = waterbodies2_1km_pct, y = no_trap_mod, col = "1km"),  size = 1) +
  geom_point(aes(x = waterbodies2_2km_pct, y = no_trap_mod, col = "2km"),  size = 1) +
  geom_point(aes(x = waterbodies2_3km_pct, y = no_trap_mod, col = "3km"),  size = 1) +
  geom_point(aes(x = waterbodies2_4km_pct, y = no_trap_mod, col = "4km"),  size = 1) +
  geom_point(aes(x = waterbodies2_5km_pct, y = no_trap_mod, col = "5km"),  size = 1) +
  geom_point(aes(x = waterbodies2_6km_pct, y = no_trap_mod, col = "6km"),  size = 1) +
  geom_point(aes(x = waterbodies2_7km_pct, y = no_trap_mod, col = "7km"),  size = 1) +
  geom_point(aes(x = waterbodies2_8km_pct, y = no_trap_mod, col = "8km"),  size = 1) +
  geom_point(aes(x = waterbodies2_9km_pct, y = no_trap_mod, col = "9km"),  size = 1) +
  labs(title = "Only positive observations", x = "Waterbodies2 coverage (%)", y = "Number of mosquitoes per trap", col = "Buffer",
       caption = "Source: NVIS Aus Gov (Department of the Environmental and Energy) and Andrew's") +
  theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
