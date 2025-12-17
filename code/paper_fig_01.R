# ------------------------------------------------------------------------------
# FIGURE 01
# Map of surveys
# Caption: Map of survey locations for Anopheles farauti between 1985 and 2000. 
# Distribution of presence (black circles) and absence (white circles) survey 
# sites across the study region (Beebe N. W., unpublished field records; 
# Sweeney et al., 2006, 2007; van den Hurk et al., 1998). Black triangles denote
# sites where the longitudinal abundance surveys were undertaken 
# (van den Hurk et al., 2000).
# ------------------------------------------------------------------------------


# Setup / Auxiliary functions 
source(here::here("code","00_setup.R"))


# Coastline
aus <- terra::vect("data/maps/gadm41_AUS_0.shp")
mainland <- aus[aus$COUNTRY == "Australia", ]

# Observed presence/absence points
obs <- ab_data <- read.csv(file.path(dir_data, "ab.csv"))  |> 
  filter(source %in% c("nigel","andrew")) |>  
  filter(region == "QLD") |> 
  dplyr::select(lon, lat, presence) |>
  group_by(lon, lat) |>
  slice_max(presence, with_ties = FALSE) |> 
  ungroup()

# Add Bowen & Mackay
new_pt1 <- data.frame(lon = 148.1430, lat = -19.9540, presence = 1) # Bowen
new_pt2 <- data.frame(lon = 149.1190, lat = -21.1040, presence = 1) # Mackay
obs <- rbind(obs, new_pt1, new_pt2)

# Formatting
obs <- obs |>
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    presence = as.integer(presence),
    pres_f = factor(ifelse(presence == 1, "Presence", "Absence"), levels = c("Absence","Presence"))
  )
obs_sf <- st_as_sf(obs, coords = c("lon", "lat"), crs = 4326)

# Longitudinal abundance survey sites (triangles)
ab <- read.csv("data/ab.csv") |>
  dplyr::filter(source == "andrew", region == "QLD", method == "T") |>
  dplyr::select(lon, lat, habitat, site) |>
  dplyr::distinct()

# Map from site *group* (first letter of the code) to full name
site_map <- c(
  K = "Kuranda",
  B = "Black Mountain Road",
  C = "Cairns",
  R = "Russell River",
  M = "Ninds Creek",
  E = "Eubenangee Swamp"
)
ab <- ab |>
  mutate(
    site_key  = substring(gsub("[^A-Z]", "", site), 1, 1),
    site_name = unname(site_map[site_key]),
    site_name = factor(site_name, levels = unname(site_map))
  )
ab_sf <- st_as_sf(ab, coords = c("lon","lat"), crs = 4326)

# ---- Unified legend mapping ---------------------------------------------------
obs_sf <- obs_sf |>
  mutate(
    type = factor(ifelse(pres_f == "Presence", "Presence", "Absence"), levels = c("Absence","Presence","Abundance"))
  )
ab_sf  <- ab_sf  |>
  mutate(type = factor("Abundance", levels = levels(obs_sf$type)))

# Plot bbox (east coast only)
plot_bbox <- c(xmin = 138, xmax = 155, ymin = -28, ymax = -9)

# Mainland to sf
mainland_sf <- st_as_sf(mainland) |> st_transform(st_crs(obs_sf))

# Optional: 5-km *inland* contour (kept commented)
crs_metric <- 3577  # Australian Albers (meters)

# --- Auto zoom bbox around abundance sites ------------------------------------
bb  <- st_bbox(ab_sf); pad <- 0.35
zoom_bbox <- st_bbox(
  c(xmin = as.numeric(bb["xmin"]) - pad,
    ymin = as.numeric(bb["ymin"]) - pad,
    xmax = as.numeric(bb["xmax"]) + pad,
    ymax = as.numeric(bb["ymax"]) + pad),
  crs = st_crs(ab_sf)
)
bbox_poly <- st_as_sfc(zoom_bbox)


# --- Main panel ---------------------------------------------------------------
main_panel <- ggplot() +
  geom_sf(
    data = mainland_sf, 
    fill = "grey95", 
    color = "grey60", 
    linewidth = 0.3, 
    alpha = 0.5
  ) +
  geom_sf(
    data = obs_sf, 
    aes(shape = type, fill = type),
    colour = "black", 
    stroke = 0.25, 
    size = 1.25, 
    alpha = 0.95, 
    show.legend = TRUE
  ) +
  geom_sf(
    data = ab_sf,
    aes(shape = type, fill = type),
    colour = "black", 
    stroke = 0.25, 
    size = 1.5, 
    alpha = 0.95, 
    show.legend = TRUE
  ) +
  scale_shape_manual(
    values = c("Absence" = 21, "Presence" = 21, "Abundance" = 24),
    name = "Survey"
  ) +
  scale_fill_manual(
    values = c("Absence" = "white", "Presence" = "black", "Abundance" = "black"),
    name = "Survey"
  ) +
  coord_sf(xlim = plot_bbox[1:2], ylim = plot_bbox[3:4], expand = FALSE) +
  guides(
    fill  = guide_legend(order = 1, override.aes = list(shape = c(21,21,24), colour = "black", size = 2)),
    shape = "none"
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.ticks  = element_line(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14, angle = 90),
    legend.position   = "right",
    legend.title      = element_text(size = 12),
    legend.text       = element_text(size = 12),
    legend.key        = element_rect(fill = "transparent", colour = NA),
    legend.background = element_blank(),
    panel.background  = element_rect(fill = "white", color = "gray80"), 
    panel.border      = element_rect(colour = "gray40", fill = NA, linewidth = 0.25)
  ) 

# Adding north arrow + scale
main_panel <- main_panel +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",
    pad_x = unit(0.25, "in"), 
    pad_y = unit(0.25, "in"),
    style  = north_arrow_fancy_orienteering,
    height = unit(0.8, "cm"), 
    width = unit(0.8, "cm")
  )  +
  coord_sf(
    xlim = c(zoom_bbox["xmin"], zoom_bbox["xmax"]),
    ylim = c(zoom_bbox["ymin"], zoom_bbox["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(1.0, "in"), 
    pad_y = unit(0.25, "in"),
    bar_cols = c("grey20","white")
  ) +
  geom_sf(
    data = bbox_poly, 
    fill = NA, 
    colour = "grey20", 
    linetype = 2, 
    linewidth = 0.25
  ) +
 coord_sf(
    xlim = plot_bbox[1:2], 
    ylim = plot_bbox[3:4], 
    expand = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "right"
  )

# ------------------ inset with place names ------------------------------------
places_inset <- tibble::tribble(
  ~name,                  ~lon,     ~lat,
  "Kuranda",              145.635, -16.820,
  "Black Mountain Road",  145.620, -16.805,
  "Cairns",               145.770, -16.925,
  "Eubenangee Swamp",     146.040, -17.535,
  "Russell River",        145.860, -17.130,
  "Ninds Creek",          146.015, -17.535
)

lon_breaks <- pretty(c(as.numeric(zoom_bbox["xmin"]), as.numeric(zoom_bbox["xmax"])), n = 4)
lat_breaks <- pretty(c(as.numeric(zoom_bbox["ymin"]), as.numeric(zoom_bbox["ymax"])), n = 4)
lon_lab <- function(x) sprintf("%.1f°E", x)
lat_lab <- function(y) sprintf("%.1f°S", abs(y))

inset_zoom <- ggplot() +
  geom_sf(
    data = mainland_sf, 
    fill = "grey95", 
    color = "grey60", 
    linewidth = 0.25
  ) +
  geom_sf(
    data = obs_sf,
    aes(shape = type, fill = type),
    colour = "black", 
    stroke = 0.25, 
    size = 1.5, 
    alpha = 0.95,
    show.legend = FALSE
  ) +
  geom_sf(
    data = ab_sf,
    aes(shape = type, fill = type),
    colour = "black", 
    stroke = 0.25,
    size = 2.2,
    alpha = 0.95,
    show.legend = FALSE
  ) +
  geom_sf(
    data = bbox_poly,
    fill = NA,
    colour = "grey20",
    linetype = 2,
    linewidth = 0.40
  ) +
  scale_shape_manual(
    values = c("Absence" = 21, "Presence" = 21, "Abundance" = 24)
  ) +
  scale_fill_manual(
    values = c("Absence" = "white", "Presence" = "black", "Abundance" = "black")
  ) +
  coord_sf(
    xlim = c(zoom_bbox["xmin"], zoom_bbox["xmax"]),
    ylim = c(zoom_bbox["ymin"], zoom_bbox["ymax"]),
    expand = FALSE
  ) +
  geom_point(
    data = places_inset, 
    aes(lon, lat), 
    size = 0.5
  ) +
  geom_text_repel(
    data = places_inset, 
    aes(lon, lat, label = name),
    size = 3, 
    seed = 123, 
    box.padding = 0.25,
    point.padding = 0.25,
    segment.size = 0.3,
    min.segment.length = 0
  ) +
  scale_x_continuous(
    breaks = lon_breaks, 
    labels = lon_lab
  ) +
  scale_y_continuous(
    breaks = lat_breaks, 
    labels = lat_lab
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_text(size = 8),
    axis.ticks = element_line(linewidth = 0.25),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.20, "in"),
    pad_y = unit(0.25, "in"),
    bar_cols = c("grey20","white")
  )

# Final map!
final_map <- ggdraw(main_panel) + 
  draw_plot(inset_zoom, x = 0.46, y = 0.53, width = 0.35, height = 0.44)

# Check
print(final_map)

# Save 
ggsave(
  file.path(dir_plots, "map_surveys.png"),
  final_map, 
  width = 8, 
  height = 8, 
  dpi = 300
)


# REFERENCES
#
# Sweeney, A. W., Beebe, N. W. and Cooper, R. D. (2007) Analysis of environmental factors influencing
#   the range of anopheline mosquitoes in northern Australia using a genetic algorithm and data mining
#   methods. Ecological Modelling, 203, 375–386.
# Sweeney, A. W., Beebe, N. W., Cooper, R. D., Bauer, J. T. and Peterson, A. T. (2006) Environmental
#   factors associated with distribution and range limits of malaria vector Anopheles farauti in
#   Australia. Journal of Medical Entomology, 43, 1068–1075.
# van den Hurk, A. F., Ritchie, S. A., Ingram, A. and Cooper, R. D. (1998) The first report of Anopheles
#   farauti sensu stricto below the nineteenth parallel at Mackay, Queensland. Medical Journal of
#   Australia, 169, 89–90.
# van den Hurk, A. F., Cooper, R. D., Beebe, N. W., Williams, G. M., Bryan, J. H. and Ritchie, S. A.
#   (2000) Seasonal abundance of Anopheles farauti (Diptera: Culicidae) sibling species in far north
#   Queensland, Australia. Journal of Medical Entomology, 37, 153–161.