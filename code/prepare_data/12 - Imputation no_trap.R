# Farauti_ss_North_QLD with imputations for no_trap
# Manuela, M.
# 24-01-2025


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
data <- read_fst(paste0(dir_output, "mozzie/Farauti_ss_North_QLD_with_covariates.fst"))
glimpse(data)


# Formatting
data <- data %>% rename(no_trap = `no.trap`)
data$fsite <- factor(data$site, levels = c("BM1", "BM2", "K1", "K2", "C1", "C2", "RR1", "RR2", "E1", "E2", "M1", "M2"))
data <- data %>% mutate(date = ymd(date))  # Convert to Date if not already in Date format
data <- data  %>%  mutate(
  season2 = case_when(
    month %in% c(6, 7, 8) ~ "ED",    # Early Dry Season
    month %in% c(9, 10, 11) ~ "LD",  # Late Dry Season
    month %in% c(12, 1, 2) ~ "EW",   # Early Wet Season
    TRUE ~ "LW"                      # Late Wet Season
  )
) 
data <- data %>% mutate(season_year = paste(season, year, sep = "-")) 
data <- data %>% mutate(season2_year = paste(season2, year, sep = "-"))
data$season_year <- factor(data$season_year, levels = c("W-1995","D-1995","W-1996","D-1996","W-1997","D-1997"))
data$season2_year <- factor(data$season2_year, levels = c("EW-1995","LW-1995","ED-1995","LD-1995",
                                                          "EW-1996","LW-1996","ED-1996","LD-1996",
                                                          "EW-1997","LW-1997","ED-1997","LD-1997"))
data <- data %>% mutate(month_year = paste(month, year, sep = "-")) 
data$month_year <- factor(data$month_year, levels = c("8-1995","9-1995","10-1995","11-1995","12-1995",
                                                      "1-1996","2-1996","3-1996","4-1996","5-1996","6-1996","7-1996","8-1996","9-1996","10-1996","11-1996",
                                                      "1-1997","2-1997","3-1997","4-1997","5-1997","6-1997","7-1997","8-1997","9-1997"))

# Check
glimpse(data)

# Number of observations
with(data, table(month, year))
with(data, table(season2, year))
with(data, table(season, year))



# Missing values: Imputation ---------------------------------------------------

# This is a temporary 'solution'/patch !!!! 
missing_no_trap <- data[is.na(data$no_trap), ]

# Display rows with missing values
print(missing_no_trap)

# Count the number of missing values in no.trap
cat("Number of rows with missing no.trap:", nrow(missing_no_trap), "\n")

# Work on an alternative column
data$no_trap_mod <- data$no_trap

# Auxiliary function
replace_na_sequential <- function(data) {
  
  # Function to fill missing values by 
  # sequentially computing the mean of 
  # the previous two values
  
  for (i in seq_len(nrow(data))) {
    if (is.na(data$no_trap_mod[i])) {
      if (i > 1) {
        previous_values <- data$no_trap_mod[max(1, i - 2):(i - 1)]
        if (length(previous_values) > 0) {
          data$no_trap_mod[i] <- round(mean(previous_values, na.rm = TRUE))
        }
        # Alternative:
        # # Compute the mean of the previous two non-NA values (if available)
        # previous_values <- na.omit(data$no.trap_mod[max(1, i - 2):(i - 1)])
      }
    }
  }
  return(data)
}

# Apply the function to each site group
data <- data %>%
  group_by(site) %>%
  arrange(date, .by_group = TRUE) %>%
  group_modify(~ replace_na_sequential(.x)) %>%
  ungroup()
data <- as.data.frame(data)

# Last check
head(data)


# Save updated version ---------------------------------------------------------
write_fst(
  data,
  paste0(dir_output, "mozzie/Farauti_ss_North_QLD_with_covariates_updated.fst"),
  compress = 50
)
