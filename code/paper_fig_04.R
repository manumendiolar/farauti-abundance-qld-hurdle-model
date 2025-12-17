# ------------------------------------------------------------------------------
# FIGURE 4
#
# Heatmap of mean and inset time series of predicted An. farauti abundance 
# across Queensland. The heatmap shows the mean predicted abundance for the 
# 1995--1997 period in each 5 km by 5 km  grid cell, derived from a hurdle model
# (BRT distribution, RF abundance). Grid cells are coloured where suitability 
# exceeds a threshold of $\tau = 0.25$, with warmer colours indicating higher
# mean abundance (light yellow = lower, dark red = higher); note that values are 
# averaged over the whole period, so the colour scale differs from the abundances 
# shown in the insets. Insets correspond to two example locations: a coastal grid 
# cell in Boigu Island (top inset) and a coastal grid cell in Cape Tribulation 
# (bottom inset) and show the predicted time series of abundance (black line) and 
# corresponding negative-binomial predictive intervals (shaded area) for each 
# indicated locations.
# ------------------------------------------------------------------------------


# Setup / Auxiliary functions 
source(here::here("code","00_setup.R"))


# FIRST RUN:
# predictions_at_centroids.R (once, it's to generate centroids_5x5_qld_with_predictions.fst)
# uncomment the follwoing if not
# source(here::here("code",predictions_at_centroids.R"))

# Load Queensland 5 x 5 km grid info
centroids <- fst::read_fst(file.path(dir_pred, "centroids_5x5_qld_with_predictions.fst")) |>  
  dplyr::mutate(
    date   = as.Date(date),
    year   = as.factor(year),
    month  = as.factor(month),
    season = as.factor(season)
  )

# Check
glimpse(centroids)


# More setup ------------------------------------------------------------------------
dist_model <- "brt"
abund_model <- "rf"
tau <- 0.25
col_name <- paste0(dist_model, "_", abund_model, "_t", sprintf("%03d", tau * 100), "_abund") 
chosen_conf <- 0.95 
th <- estimate_theta(ab_data)    # assumes ab_data is loaded in current env.


# Choose example locations (Boigu + Cape Tribulation) -------------------------------
centroids_unique <- centroids %>% dplyr::distinct(grid_id, lon, lat)

get_nearest_grid <- function(lon0, lat0, df){
  df %>%
    dplyr::mutate(d = (lon - lon0)^2 + (lat - lat0)^2) %>%
    dplyr::slice_min(d, n = 1) %>%
    dplyr::pull(grid_id)
}

# Approx coords (nearest grid cell will be used)
chosen_grid1  <- get_nearest_grid(142.220, -9.240,  centroids_unique)  # Boigu Island
chosen_grid2  <- get_nearest_grid(145.480, -16.060, centroids_unique)  # Cape Tribulation
chosen_place1 <- "Boigu Is"
chosen_place2 <- "Cape Tribulation"
boigu_xy <- centroids_unique %>% dplyr::filter(grid_id == chosen_grid1)
cape_xy  <- centroids_unique %>% dplyr::filter(grid_id == chosen_grid2)


# ------------------------------------------------------------------------------
#                           Trap counts to abundance
#                                using MRR data
#
# From Chow et al. (2025): "Estimating the dispersal of the malaria vector 
# Anopheles farauti through a natural ecosystem in north Queensland, Australia 
# using mark release and recapture experiments"
#
# 
# Year-Season  |  Marked & Rel.  | Tot. Collected  | Marked Collected  | N 
# ______________________________________________________________________________
# 2015-wet     |  3346           | 16675           |  39               | 1430629                    
# 2015-dry     |  1110           | 17425           |  14               | 1381553
# 2016-wet     |  2374           | 17051           |  19               | 2130478    
# 2016-dry     |  5422           | 16997           | 119               |  774434
# 2017-wet     |   752           |  4287           |  13               |  247986
# 2017-dry     |  6885           |  2599           | 105               |  170420   
#
#
# Trap efficiency as % 
# 2015-wet 1.165571
# 2015-dry 1.261261
# 2016-wet 0.800337
# 2016-dry 2.194762
# 2017-wet 1.728723
# 2017-dry 1.525054

# Mean trap efficiency as %
p_wet <- 1.231544
p_dry <- 1.660359

# Convert % to proportions
eff_wet <- p_wet / 100
eff_dry <- p_dry / 100

# Add trapcounts and abundance to centroids
centroids <- centroids %>%
  mutate(
    trapcounts = .data[[col_name]],          # make it explicit
    abundance  = if_else(
      season == "D",                         # dry season
      trapcounts / eff_dry,                 # N = C / p
      trapcounts / eff_wet                  # wet season
    )
  )
# ------------------------------------------------------------------------------


# Rename our target column 
col_name <- "abundance"

# Filter and compute descriptive statistics at particular season and year
centroids_plot <- centroids %>%
  transmute(grid_id, date, month, year, season, lon, lat, value = .data[[col_name]]) |> 
  mutate(value = round(value)) |> 
  filter(value > 0) |> 
  group_by(grid_id, lon, lat) %>%
  summarise(
    n      = n(),
    mean   = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    .groups = "drop"
  )

# Check
head(centroids_plot)

# Split into grey background + coloured overlay
#centroids_grey <- centroids_plot %>% dplyr::filter(mean <= 0)
#centroids_col  <- centroids_plot %>% dplyr::filter(mean > 0)

# Optional sampling for speed
set.seed(1)
if (nrow(centroids_plot) > 5e5) centroids_plot <- dplyr::slice_sample(centroids_plot, n = 5e5)
#if (nrow(centroids_grey) > 5e5) centroids_grey <- dplyr::slice_sample(centroids_grey, n = 5e5)
#if (nrow(centroids_col)  > 5e5) centroids_col  <- dplyr::slice_sample(centroids_col,  n = 5e5)


# Places (add Boigu + Cape Tribulation to labels/points) -------------------
places_qld <- tibble::tribble(
  ~name,                  ~lon,     ~lat,
  "Boigu Is",          142.220,   -9.240,
  "Horn Is.",          142.287,  -10.612,
  "Bamaga",            142.391,  -10.886,
  "Weipa",             141.870,  -12.630,  
  "Lockhart River",    143.407,  -12.789,
  "Aurukun",           141.729,  -13.606,
  "Pormpuraaw",        141.620,  -14.900,
  "Cooktown",          145.250,  -15.470, 
  "Cape Tribulation",  145.480,  -16.060,
  "Cairns",            145.770,  -16.925,
  "Innisfail",         146.025,  -17.520,
  "Townsville",        146.810,  -19.260
)

# Plot bbox (east coast only)
plot_bbox <- c(xmin = 138, xmax = 155, ymin = -20, ymax = -9) 

# Coastline
aus <- terra::vect("data/maps/gadm41_AUS_0.shp")
mainland <- aus[aus$COUNTRY == "Australia", ]

# Mainland to sf
mainland_sf <- sf::st_as_sf(mainland) 

# East-coast limits
xlim_ec <- c(137.9, 154.1)
ylim_ec <- c(-29.2,  -9.0)

# Optional: 5-km *inland* contour
crs_metric <- 3577  # Australian Albers (meters)
inland_5km_line <- mainland_sf |>
  sf::st_transform(crs_metric) |>
  sf::st_buffer(dist = -5000) |>   
  sf::st_boundary() |>
  sf::st_transform(4326)

main_panel <- ggplot() +
  geom_sf(
    data = mainland_sf, 
    fill = "grey95", 
    color = "grey60",
    linewidth = 0.3,
    alpha = 0.5
  ) +
  coord_sf(
    xlim = plot_bbox[1:2], 
    ylim = plot_bbox[3:4], 
    expand = FALSE
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.ticks = element_line(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14, angle = 90),
    legend.position   = "right",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 12),
    legend.key        = element_rect(fill = "transparent", colour = NA),
    legend.background = element_blank(),
    panel.background  = element_rect(fill = "white", color = "black"),
    panel.border      = element_rect(colour = "gray40", fill = NA, linewidth = 0.25)
  ) 
# Check
#print(main_panel)


# Adding predictions -----------------------------------------------------------
centroids_plot_sf <- sf::st_as_sf(centroids_plot, coords = c("lon","lat"), crs = 4326)

# Colour palette
pal_yr <- brewer.pal(9, "YlOrRd")

# Range for abundance 
#upper <- max(centroids_plot$mean, na.rm = TRUE) 
upper <- quantile(centroids_plot$mean, 0.99, na.rm = TRUE)  # legend range [1, upper]; use a high quantile to avoid outliers blowing it out


# Time-series data for the two chosen grid cells
# Filter and compute descriptive statistics at particular season and year
centroids_ts <- centroids %>%
  transmute(grid_id, date, month, year, season, lon, lat, E_hat = .data[[col_name]]) |>  # E_hat: pred. abund. at grid level
  mutate(E_hat = round(E_hat)) #|> filter(E_hat > 0) 

# Check
head(centroids_ts)

# Boigu Island
centroids_ts1 <- centroids_ts |>
  filter(grid_id == chosen_grid1) |> 
  mutate(
    lower = qnbinom((1 - chosen_conf)/2, mu = pmax(E_hat, 1e-8), size = th),  # th comes from earlier estimates
    upper = qnbinom(1 - (1 - chosen_conf)/2, mu = pmax(E_hat, 1e-8), size = th)
  ) |> 
  group_by(date) |> 
  summarise(
    E_hat = mean(E_hat, na.rm = TRUE),
    lower = min(lower, na.rm = TRUE), 
    upper = max(upper, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(date)

# Check
head(centroids_ts1)

#Cape Tribulation
centroids_ts2 <- centroids_ts |>
  filter(grid_id == chosen_grid2) |> 
  mutate(
    lower = qnbinom((1 - chosen_conf)/2, mu = pmax(E_hat, 1e-8), size = th),  # th comes from earlier estimates
    upper = qnbinom(1 - (1 - chosen_conf)/2, mu = pmax(E_hat, 1e-8), size = th)
  ) |> 
  group_by(date) |> 
  summarise(
    E_hat = mean(E_hat, na.rm = TRUE),
    lower = min(lower, na.rm = TRUE), 
    upper = max(upper, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(date)

# Check
head(centroids_ts2)


# Time series plot of predicted relative abundance at chosen_grid
p_ts1 <- ggplot(centroids_ts1, aes(date)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper), 
    fill = "grey50",
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(
    aes(y = E_hat),
    colour = "#222222",
    linewidth = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    colour = "grey75",
    linewidth = 0.25
  ) +
  scale_x_date(
    date_labels = "%b-%y",
    date_breaks = "2 months",
    expand = expansion(c(0.01, 0.03))
  ) +
  labs(
    x = NULL, y = "Abundance", title = "A"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 7.5, angle = 90),
    panel.background = element_rect(fill = scales::alpha("white", 0.88), colour = NA),
    plot.background  = element_rect(fill = scales::alpha("white", 0.88), colour = NA),
    plot.title  = element_text(size = 8, face = "bold", hjust = 1.0)
  ) 

# Check
#print(p_ts1)

# Time series plot of predicted relative abundance at chosen_grid
p_ts2 <- ggplot(centroids_ts2, aes(date)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper), 
    fill = "grey50", 
    alpha = 0.15, 
    colour = NA) +
  geom_line(
    aes(y = E_hat), 
    colour = "#222222", 
    linewidth = 0.25
  ) +
  geom_hline(
    yintercept = 0, 
    colour = "grey75", 
    linewidth = 0.25
  ) +
  scale_x_date(
    date_labels = "%b-%y",
    date_breaks = "2 months",
    expand = expansion(c(0.01, 0.03))
  ) +
  labs(
    x = NULL, y = "Abundance", title = "B"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 7.5, angle = 90),
    panel.background = element_rect(fill = scales::alpha("white", 0.88), colour = NA),
    plot.background  = element_rect(fill = scales::alpha("white", 0.88), colour = NA),
    plot.title  = element_text(size = 8, face = "bold", hjust = 1.0)
  )

# Check
#print(p_ts2)



## Build base heat map + north arrow + scale 
heat_map <- main_panel +  
  geom_point(
    data = centroids_plot, 
    aes(lon, lat, colour = mean), 
    inherit.aes = FALSE, 
    size = 0.22, 
    alpha = 0.9
  ) +
  scale_colour_gradientn(
    colours = pal_yr,
    name    = "Abundance",
    limits  = c(1, 1250),
    oob     = scales::squish,
    breaks  = seq(0, 1250, 250),
    guide   = guide_colorbar(
      title.position = "top", title.hjust = 0,
      barheight = unit(140, "pt"), barwidth = unit(10, "pt"))
  ) +
  geom_point(
    data = places_qld, 
    aes(lon, lat), 
    inherit.aes = FALSE,
    shape = 21, 
    size = 1, 
    fill = "black",
    colour = "black", 
    stroke = 0.35
  ) +
  ggrepel::geom_text_repel(
    data = dplyr::filter(places_qld, name != "Cairns"),
    aes(lon, lat, label = name),
    inherit.aes = FALSE,
    size = 2.25,
    seed = 123,
    box.padding  = 0.25,
    point.padding = 0.25,
    segment.size = 0,
    segment.color = NA
  ) +
  # Cairns label, nudged upward
  ggrepel::geom_text_repel(
    data = dplyr::filter(places_qld, name == "Cairns"),
    aes(lon, lat, label = name),
    inherit.aes = FALSE,
    size = 2.25,
    seed = 123,
    box.padding  = 0.25,
    point.padding = 0.25,
    segment.size = 0,
    segment.color = NA,
    nudge_y = 0.3          # move up; tweak (e.g. 0.2, 0.4) to taste
) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 12, face = 1),
    legend.text     = element_text(size = 10),
    axis.text.x     = element_text(size = 12),
    axis.text.y     = element_text(size = 12),
    axis.title.x    = element_text(size = 14),
    axis.title.y    = element_text(size = 14)
  ) +
  # NORTH ARROW & SCALE *INSIDE* THE MAP (bottom-left)
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x  = unit(0.20, "in"),   # tweak these two
    pad_y  = unit(0.20, "in"),   # to move arrow inland
    style  = north_arrow_fancy_orienteering,
    height = unit(0.8, "cm"),
    width  = unit(0.8, "cm")
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.8, "in"),    # just to the right of arrow
    pad_y = unit(0.25, "in"),
    bar_cols = c("grey20", "white"),
    text_cex = 0.8
  )

# Add two straight arrows + small squares at their origins ---------------------
## Add two straight arrows + semi-transparent squares at their origins
heat_map_arrows <- heat_map +
  geom_segment(
    data = data.frame(lon = boigu_xy$lon, lat = boigu_xy$lat),
    aes(x = lon, y = lat, xend = 146.7, yend = -10.90),
    inherit.aes = FALSE,
    colour = "grey25",
    linewidth = 0.30,   # thinner, more elegant
    arrow = arrow(length = unit(1.30, "mm"), type = "closed")
  ) +
  geom_point(
    data = data.frame(lon = boigu_xy$lon, lat = boigu_xy$lat),
    aes(x = lon, y = lat),
    inherit.aes = FALSE,
    shape  = 22,                                   # filled square
    size   = 2.50,
    stroke = 0.4,
    fill   = scales::alpha("white", 0.30),          # semi-transparent
    colour = "black"
  ) +
  geom_segment(
    data = data.frame(lon = cape_xy$lon, lat = cape_xy$lat),
    aes(x = lon, y = lat, xend = 147.80, yend = -16.75),
    inherit.aes = FALSE,
    colour = "grey25",
    linewidth = 0.30,   # thinner
    arrow = arrow(length = unit(1.30, "mm"), type = "closed")
  ) +
  geom_point(
    data = data.frame(lon = cape_xy$lon, lat = cape_xy$lat),
    aes(x = lon, y = lat),
    inherit.aes = FALSE,
    shape  = 22,
    size   = 2.5,
    stroke = 0.4,
    fill   = scales::alpha("white", 0.35),          # semi-transparent
    colour = "black"
  )


## Compose final map with insets (no extra annotations here) 
final_map <- ggdraw(heat_map_arrows) +
  draw_plot(p_ts1, x = 0.51, y = 0.55, width = 0.32, height = 0.30) +
  draw_plot(p_ts2, x = 0.56, y = 0.21, width = 0.32, height = 0.28)

## Check
print(final_map)

## Save 
filename <- file.path(dir_plots, paste0("heatmap-ts_hurdle_", dist_model,"_", abund_model,"_tau_",sprintf("%03d", tau * 100),"_grid_5x5_QLD_test.png"))
ggsave(filename, final_map, width = 8, height = 6.5, dpi = 300)


