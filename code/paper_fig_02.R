# ------------------------------------------------------------------------------
# FIGURE 02 (+ FIGURES S02--S06) 
# Distribution + Uncertainty maps for ALL distribution models
#
# Predictions for Anopheles farauti across Queensland. Left panel: predicted 
# suitability based on a Boosted Regression Trees (BRT) model fitted to 
# environmental predictors and historical survey data. Values range from 0 
# (lowest suitability) to 1 (highest suitability). Right panel: uncertainty in
# this prediction represented as the coefficient of variation (CV; standard 
# deviation divided by the mean predicted suitability) at grid-cell level across 
# all folds and repetitions.
# ------------------------------------------------------------------------------


# Setup / Auxiliary functions 
source(here::here("code","00_setup.R"))

# Comment out if you haven't ran 01_dist_models.R 
#source(here::here("code","01_dist_models.R"))

# Load suitability rasters
rf_raster  <- terra::rast(file.path(dir_pred, "rf_prediction.asc"))
brt_raster <- terra::rast(file.path(dir_pred, "brt_prediction.asc"))
max_raster <- terra::rast(file.path(dir_pred, "max_prediction.asc"))
glm_raster <- terra::rast(file.path(dir_pred, "glm_prediction.asc"))
gam_raster <- terra::rast(file.path(dir_pred, "gam_prediction.asc"))
ens_raster <- terra::rast(file.path(dir_pred, "ens_prediction.asc"))

# Load uncertainty rasters
rf_raster_sd  <- terra::rast(file.path(dir_pred, "rf_cv_sd.asc"))
brt_raster_sd <- terra::rast(file.path(dir_pred, "brt_cv_sd.asc"))
max_raster_sd <- terra::rast(file.path(dir_pred, "max_cv_sd.asc"))
glm_raster_sd <- terra::rast(file.path(dir_pred, "glm_cv_sd.asc"))
gam_raster_sd <- terra::rast(file.path(dir_pred, "gam_cv_sd.asc"))
ens_raster_sd <- terra::rast(file.path(dir_pred, "ens_cv_sd.asc"))

# Load CV rasters
rf_raster_cv  <- terra::rast(file.path(dir_pred, "rf_cv_cv.asc"))
brt_raster_cv <- terra::rast(file.path(dir_pred, "brt_cv_cv.asc"))
max_raster_cv <- terra::rast(file.path(dir_pred, "max_cv_cv.asc"))
glm_raster_cv <- terra::rast(file.path(dir_pred, "glm_cv_cv.asc"))
gam_raster_cv <- terra::rast(file.path(dir_pred, "gam_cv_cv.asc"))
ens_raster_cv <- terra::rast(file.path(dir_pred, "ens_cv_cv.asc"))


# Coastline: transform to raster CRS and crop to raster bbox
aus <- terra::vect("data/maps/gadm41_AUS_0.shp")
mainland <- aus[aus$COUNTRY == "Australia", ]

# Convert mainland to sf object
mainland_sf <- sf::st_as_sf(mainland)

# Set the custom bounding box for the plot's view
plot_bbox <- c(xmin = 138, xmax = 155, ymin = -28, ymax = -9)


# A few colour palettes

# Heat map style 
heat_diverging <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", 
                    "#f7f786ff", "#FFFF8C",  "#FFE75E", "#FCD070",
                    "#D6604D", "#B2182B", "#67001F")

# Ocean to land (blue to brown)
ocean_land <- c("#08519C", "#3182BD", "#6BAED6", "#9ECAE1", "#C6DBEF",
                "#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15")

# Sunset palette (purple to orange)
sunset_palette <- c("#2D1E3E", "#453268", "#5E4892", "#8B679C", 
                    "#C287A6", "#E6A394", "#F4B982", "#FCD070", 
                    "#FFE75E", "#FFFF8C")

# Grayscale palette (black to white)
grey_palette <- c("#FFFFFF", "#E8E8E8", "#D0D0D0", "#A8A8A8", 
                  "#808080", "#585858", "#303030", "#000000")

# White to red
white_red <- c("#FFFFFF", "#FDE0DD", "#FCC5C0", "#FA9FB5",
               "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D")

# Grey to red
grey_red <- c("#F7F7F7", "#D9D9D9", "#BDBDBD", "#969696",
              "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D")

# Red to red
grey_red <- c(  
  "#8f8a8aff",  # low–moderate CV
  "#9a9797ff",  
  "#cfceceff",
  "#eee8e8ff",  # very low CV (very light grey)
  "#FCA5A5",
  #"#FB6A4A",
  "#EF3B2C",
  "#CB181D",
  "#99000D"   # very high CV (dark red)
)


# Purple to green: purple (high) → grey → green (low)
purple_green <- c(
  "#3F007D",  # very high (dark purple)
  "#6A51A3",  # high
  "#D9D9D9",  # medium-high (grey)
  "#C7E9B4",  # medium-low (pale green)
  "#41AB5D",  # low (green)
  "#006D2C"   # very low (dark green)
)

# Grey to purple
purple_grey <- c(
  "#3F007D",  # very low CV (light grey)
  "#6A51A3",
  "#D9D9D9",
  "#A8A8A8", 
  "#808080",
  "#3e3e3eff"   # very high CV (dark purple)
)

# For species distribution (blue to yellow to red)
distribution_palette <- heat_diverging

# For uncertainty (light to dark, or single color gradient)
uncertainty_palette <- grey_red

# For coefficient of variation: 
cv_palette <- purple_green



# Function to create a single map panel with improved legend
create_map_panel <- function(raster_input, mainland_sf, color_palette, 
                             title, legend_name, value_range = NULL,
                             legend_title_position = "top") {
  
  raster_df <- as.data.frame(raster_input, xy = TRUE, na.rm = TRUE)
  names(raster_df) <- c("x", "y", "value")
  
  if (is.null(value_range)) {
    value_range <- range(raster_df$value, na.rm = TRUE)
  }
  
  p <- ggplot(raster_df, aes(x = x, y = y)) +
    geom_sf(
      data = mainland_sf, 
      fill = NA, 
      color = "gray30", 
      size = 0.3, 
      inherit.aes = FALSE
    ) +
    geom_raster(
      aes(fill = value)
    ) +
    scale_fill_gradientn(
      colors = color_palette,
      limits = value_range,
      breaks = seq(value_range[1], value_range[2], length.out = 5),
      labels = sprintf("%.2f", seq(value_range[1], value_range[2], length.out = 5)),
      name = legend_name
    ) +
    labs(
      title = title, 
      x = "Longitude", 
      y = "Latitude") +
    coord_sf(
      xlim = c(plot_bbox[1], plot_bbox[2]),
      ylim = c(plot_bbox[3], plot_bbox[4]),
      expand = FALSE
    ) +
    theme_bw() +   
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text  = element_text(size = 7.0),
      axis.title = element_text(size = 9.0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 11, face = "bold", hjust = 0.5, vjust = ifelse(legend_title_position == "top", 0, 1)),
      legend.title.position = legend_title_position,
      legend.text = element_text(size = 9),
      legend.key.width  = unit(1.8, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.margin = ggplot2::margin(t = 5, b = 5),
      legend.box.spacing = unit(0.2, "cm"),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(colour = "gray60", fill = NA, linewidth = 0.5),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    ) +
    guides(fill = guide_colorbar(
      title.position = legend_title_position,
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.15,
      ticks.colour = "black"
    ))
  
  return(p)
}


# Load rasters
aux_raster <- list(
  RF = rf_raster, 
  BRT = brt_raster, 
  MAX = max_raster, 
  GLM = glm_raster, 
  GAM = gam_raster, 
  ENS = ens_raster
)
aux_raster_sd <- list(
  RF = rf_raster_sd,
   BRT = brt_raster_sd, 
   MAX = max_raster_sd, 
   GLM = glm_raster_sd, 
   GAM = gam_raster_sd, 
   ENS = ens_raster_sd
  )

aux_raster_cv <- list(
  RF = rf_raster_cv,
   BRT = brt_raster_cv, 
   MAX = max_raster_cv, 
   GLM = glm_raster_cv, 
   GAM = gam_raster_cv, 
   ENS = ens_raster_cv
  )

cities <- tribble(
  ~city,         ~lon,    ~lat,
  # Far north
  "Thursday Island", 142.21, -10.58,
  "Weipa",           141.87, -12.62,
  "Cooktown",        145.25, -15.47,
  
  "Cairns",      145.77, -16.92,
  "Townsville",  146.82, -19.26,
  "Mackay",      149.19, -21.14,
  "Rockhampton", 150.51, -23.38,
  # South
  "Brisbane",        153.02, -27.47
  # add/remove as you like
)


# Create the probability map with title on top
for (i in 1:6){
  rast <- aux_raster[[i]]
  rast_sd <- aux_raster_sd[[i]]
  rast_cv <- aux_raster_cv[[i]]
  
  prob_map <- create_map_panel(
    raster_input = rast,
    mainland_sf = mainland_sf,
    color_palette = distribution_palette,  # heat diverging, ocean land
    title = "Distribution",
    legend_name = "Suitability",
    value_range = c(0, 1),
    legend_title_position = "top"  # Title above the bar
  ) 
  # Add version with city labels
  prob_map_with_cities <- prob_map +
    geom_point(
      data = cities,
      aes(x = lon, y = lat),
      inherit.aes = FALSE,
      shape = 15,
      size = 1.5,
      colour = "black"
    ) +
    geom_text(
      data = cities,
      aes(x = lon, y = lat, label = city),
      inherit.aes = FALSE,
      hjust = -0.1,
      vjust = -0.4,
      size = 2.5,
      colour = "black"
    )

  # Get the range for uncertainty
  uncertainty_range <- c(0, max(values(rast_sd), na.rm = TRUE))
  
  # Create the uncertainty map with title on top
  uncertainty_map <- create_map_panel(
    raster_input = rast_sd,
    mainland_sf = mainland_sf,
    color_palette = uncertainty_palette,
    title = "Uncertainty",
    legend_name = "Standard Deviation",
    value_range = uncertainty_range,
    legend_title_position = "top"  # Title above the bar
  )
  # Add version with city labels
  uncertainty_map_with_cities <- uncertainty_map +
    geom_point(
      data = cities,
      aes(x = lon, y = lat),
      inherit.aes = FALSE,
      size = 1.5,
      colour = "black"
    ) +
    geom_text(
      data = cities,
      aes(x = lon, y = lat, label = city),
      inherit.aes = FALSE,
      hjust = -0.1,
      vjust = -0.4,
      size = 2.5,
      colour = "black"
    )

  # Get the range for uncertainty
  cv_range <- c(0, max(values(rast_cv), na.rm = TRUE))
  
  # Create the uncertainty map with title on top
  cv_map <- create_map_panel(
    raster_input = rast_cv,
    mainland_sf = mainland_sf,
    color_palette = cv_palette,
    title = "Uncertainty",
    legend_name = "Coefficient of Variation",
    value_range = cv_range,
    legend_title_position = "top"  # Title above the bar
  )

  # Combine the two maps using patchwork
  combined_plot <- prob_map + cv_map + #uncertainty_map + 
    plot_layout(ncol = 2) +
    plot_annotation(
      theme = theme(
        plot.title = element_blank(), #element_text(size = 14, face = "bold", hjust = 0.5)
      )
    )
  
  # Display the plot
  print(combined_plot)
  
  # Save the figure
  ggsave(
    paste0("outputs/plots/model_predictions_", names(aux_raster)[[i]], "_cv.png"), 
    combined_plot, 
    width = 8, height = 6, dpi = 300
  )

}



# IDEA
# Using five levels of suitability: 
# (0.0‒0.2)  unsuitability 
# (0.2‒0.4)  low suitability 
# (0.4‒0.6)  medium suitability 
# (0.6‒0.8)  suitability 
# (0.8‒1.0)  high suitability 