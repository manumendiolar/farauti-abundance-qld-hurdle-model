library(shiny)
library(shinythemes)
library(data.table)
library(fst)
library(leaflet)
library(dplyr)
library(DT)  # For interactive data tables

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      table.dataTable th, table.dataTable td {
        padding: 4px 8px;
        font-size: 12px; 
      }
    "))
  ),
  
  # HEADER ROW (title + image)
  fluidRow(
    column(
      width = 12,
      div(
        style = "padding: 20px; background-color: #f8f8f8;", 
        h2("Mapping mosquitoes hotspots", style = "margin-top: 0;"),
        h4(em("Anopheles Farauti"), style = "margin: 0;"),
        h5(em("Modelling abundance in northern Australia"), style = "margin: 0;"),
        img(src = "anopheles_farauti_dontbugme.png", 
            height = "90px", 
            style = "float: right; margin-top: -80px;")
      )
    )
  ),
  
  # NAVBAR PAGE BELOW THE HEADER
  navbarPage(
    title = "Mozzie Dashboard",
    id = "top_navbar",
    
    # TAB: Data
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 # Radio buttons for dataset selection
                 radioButtons("dataset", "Choose dataset:", 
                              choices = c("Nigel's", "Andrew's"), 
                              selected = "Nigel's"),
                 actionButton("load_data", "Load", class = "btn btn-primary"),
                 br(), br(),
                 # For Nigel's dataset, show region filter only after data has been loaded.
                 conditionalPanel(
                   condition = "input.load_data > 0 && input.dataset == \"Nigel's\"",
                   checkboxGroupInput("region_filter", "Select Region:", 
                                      choices = c("QLD", "NT", "WA"), 
                                      selected = c("QLD", "NT", "WA"))
                 )
               ),
               mainPanel(
                 width = 9,
                 # Display the map
                 leafletOutput("data_map", height = "400px"),
                 br(), hr(),
                 # Display the data table
                 DT::dataTableOutput("data_table")
               )
             )
    ),
    
    # TAB: Distribution
    tabPanel("Distribution",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 uiOutput("dist_year_ui"),  # Dynamic year selection based on loaded dataset
                 selectInput("dist_model", "Distribution Model", 
                             choices = c("BRT", "RF", "GLM", "GAM", "MAX", "Ensemble")),
                 selectInput("dist_data_type", "Data Type", 
                             choices = c("train", "test", "centroids"), selected = "centroids"),
                 checkboxInput("dist_save_map", "Save Distribution Map", value = FALSE),
                 conditionalPanel(
                   condition = "input.dist_save_map == true",
                   textInput("dist_output_file_name", "Output File Name", value = "")
                 ),
                 actionButton("update_distribution", "Update Map", class = "btn btn-primary")
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(
                     width = 12,
                     h4("Wet Season (Distribution)"),
                     uiOutput("dist_map_W_html"),
                     br(),
                     h4("Dry Season (Distribution)"),
                     uiOutput("dist_map_D_html")
                   )
                 )
               )
             )
    ),
    
    # TAB: Abundance
    tabPanel("Abundance",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 uiOutput("abund_year_ui"),  # Dynamic year selection based on loaded dataset
                 selectInput("abund_dist_model", "Distribution Model", 
                             choices = c("BRT", "RF", "GLM", "GAM", "MAX", "Ensemble")),
                 selectInput("abund_model", "Abundance Model", 
                             choices = c("BRT", "RF", "GLM", "GAM")),
                 selectInput("abund_data_type", "Data Type", 
                             choices = c("train", "test", "centroids"), selected = "centroids"),
                 numericInput("abund_threshold", "Threshold", value = 0.05, 
                              min = 0, max = 0.95, step = 0.05),
                 selectInput("abund_id_col", "Identifier Column", 
                             choices = c("UIN", "UID", "grid_id"), selected = "grid_id"),
                 checkboxInput("abund_save_map", "Save Abundance Map", value = FALSE),
                 conditionalPanel(
                   condition = "input.abund_save_map == true",
                   textInput("abund_output_file_name", "Output File Name", value = "")
                 ),
                 actionButton("update_abundance", "Update Map", class = "btn btn-primary")
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(
                     width = 12,
                     h4("Wet Season (Abundance)"),
                     uiOutput("abund_map_W_html"),
                     br(),
                     h4("Dry Season (Abundance)"),
                     uiOutput("abund_map_D_html")
                   )
                 )
               )
             )
    ),
    
    # TAB: About
    tabPanel("About",
             fluidPage(
               fluidRow(
                 column(
                   width = 12,
                   h3("About This App"),
                   p("Add any information about your application, authors, references, or other relevant details here.")
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Use reactiveValues to store the loaded data and its dataset type
  loaded <- reactiveValues(data = NULL, dataset = NULL)
  
  # Dynamic UI for Distribution tab year selection based on loaded dataset
  output$dist_year_ui <- renderUI({
    req(loaded$dataset)
    if (loaded$dataset == "Nigel's") {
      selectInput("dist_year", "Year", choices = c(1986, 1987, 1991), selected = 1986)
    } else if (loaded$dataset == "Andrew's") {
      selectInput("dist_year", "Year", choices = c(1995, 1996, 1997), selected = 1995)
    }
  })
  
  # Dynamic UI for Abundance tab year selection based on loaded dataset
  output$abund_year_ui <- renderUI({
    req(loaded$dataset)
    if (loaded$dataset == "Nigel's") {
      selectInput("abund_year", "Year", choices = c(1986, 1991), selected = 1986)
    } else if (loaded$dataset == "Andrew's") {
      selectInput("abund_year", "Year", choices = c(1995, 1996, 1997), selected = 1995)
    }
  })
  
  # Load data when the "Load" button is clicked
  observeEvent(input$load_data, {
    if (input$dataset == "Nigel's") {
      cols_nigel <- c("UIN", "region", "year", "lon", "lat", "far1", "method", 
                      "elevation", "dist_coast_km", 
                      "precip_surveyperiod", "tmin_surveyperiod", "tmax_surveyperiod",
                      "rh_surveyperiod",
                      "mangroves_4km_pct", "waterbodies2_2km_pct2")
      data <- read_fst("C:/Users/men118/Documents/gd-project/outputs/mozzie/Farauti_numbers_AUS-Nigel_with_covariates.fst",
                       columns = cols_nigel)
      data <- data %>% 
        rename(
          count = far1,
          precip = precip_surveyperiod,
          tmin = tmin_surveyperiod,
          tmax = tmax_surveyperiod,
          rh = rh_surveyperiod,
          mangroves_4km = mangroves_4km_pct,
          water_4km = waterbodies2_2km_pct2
        )
      loaded$data <- data
      loaded$dataset <- "Nigel's"
    } else {
      cols_andrew <- c("UID", "region", "date", "year", "month", "day", "lon", "lat", "no_trap_mod",  
                       "elevation", "dist_coast_km", 
                       "accum_precip_21d", 
                       "tmin", "tmax",
                       "rh",
                       "mangroves_4km_pct", "waterbodies2_2km_pct2")
      data <- read_fst("C:/Users/men118/Documents/gd-project/outputs/mozzie/Farauti_numbers_North_QLD-Andrew_with_covariates_daily.fst",
                       columns = cols_andrew)
      data <- data %>% 
        rename(
          count = no_trap_mod,
          precip_21d = accum_precip_21d,
          mangroves_4km = mangroves_4km_pct,
          water_4km = waterbodies2_2km_pct2
        )
      loaded$data <- data
      loaded$dataset <- "Andrew's"
    }
  })
  
  # Render the Data Map using loaded$data and loaded$dataset
  output$data_map <- renderLeaflet({
    req(loaded$data)
    d <- loaded$data
    if (loaded$dataset == "Nigel's") {
      if (!is.null(input$region_filter))
        d <- d %>% filter(region %in% input$region_filter)
      
      leaflet(d) %>% 
        addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 0.99)) %>%
        addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>%
        addControl(
          html = htmltools::HTML(
            "<div style='text-align:left; font-size:0.8em; font-style:italic; color:#555;'>
              Note: mean over survey period (late wet season)
            </div>"
          ),
          position = "bottomright"
        ) %>% 
        addCircleMarkers(
          ~lon, ~lat,
          radius = ~sqrt(count),
          color = ~ifelse(count == 0, "blue", "red"),
          popup = ~paste(
            "UIN:", UIN, "<br>",
            "Year:", year, "<br>",
            "# count:", count, "<br>",
            "Elevation:", round(elevation, 2), "m<br>",
            "Distance from coast:", round(dist_coast_km, 2), "km<br>",
            "Mean accum. precip.:", round(precip, 2), "mm<br>",
            "Mean Tmin:", round(tmin, 2), "C<br>",
            "Mean Tmax:", round(tmax, 2), "C<br>",
            "Mean RH:", round(rh, 2), "%<br>"
          ),
          label = ~paste("Year:", year, "| #:", count),
          labelOptions = labelOptions(direction = "auto")
        ) %>% 
        addLegend(
          position = "topright",
          colors = c("red","blue"),
          labels = c("Yes", "No"),
          title = htmltools::HTML("<em>An. farauti</em> presence")
        ) %>% 
        setView(lng = 146, lat = -18, zoom = 6)
      
    } else if (loaded$dataset == "Andrew's") {
      if (!is.null(input$region_filter))
        d <- d %>% filter(region %in% input$region_filter)
      
      leaflet(d) %>%
        addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 0.99)) %>%
        addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>%
        addCircleMarkers(
          ~lon, ~lat,
          radius = ~sqrt(count),
          color = ~ifelse(count == 0, "blue", "red"),
          popup = ~paste(
            "UID:", UID, "<br>",
            "Year:", year, "<br>",
            "# count:", count, "<br>",
            "Elevation:", round(elevation, 2), "m<br>",
            "Distance from coast:", round(dist_coast_km, 2), "km<br>",
            "Mean accum. precip. (21 d):", round(precip_21d, 2), "mm<br>",
            "Mean Tmin:", round(tmin, 2), "C<br>",
            "Mean Tmax:", round(tmax, 2), "C<br>",
            "Mean RH:", round(rh, 2), "%<br>"
          ),
          label = ~paste("Year:", year, "| #:", count),
          labelOptions = labelOptions(direction = "auto")
        ) %>%
        addLegend(
          position = "topright",
          colors = c("red","blue"),
          labels = c("Yes", "No"),
          title = htmltools::HTML("<em>An. farauti</em> presence")
        ) %>%
        setView(lng = 146, lat = -18, zoom = 6)
    }
  })
  
  # Render the Data Table using loaded$data
  output$data_table <- DT::renderDataTable({
    req(input$load_data > 0)
    req(loaded$data)
    d <- loaded$data
    if (loaded$dataset == "Nigel's") {
      exceptions <- c("UIN", "region", "year", "count", "method")
      numeric_cols <- names(d)[sapply(d, is.numeric)]
      cols_to_format <- setdiff(numeric_cols, exceptions)
      d[cols_to_format] <- lapply(d[cols_to_format], function(x) sprintf("%.2f", x))
      
      DT::datatable(
        d,
        rownames = FALSE,
        colnames = c("UIN", "Region", "Year", "Lon", "Lat", "Count", "Method", 
                     "Elevation (m)", "Distance to coast (km)", "Precipitation (mm)", 
                     "Tmin (C)", "Tmax (C)", "Rel. Humidity (%)", "Mangroves 4km (%)", 
                     "Waterbodies 4km (%)"),
        filter = 'top',
        options = list(
          columnDefs = list(
            list(className = 'dt-right', targets = '_all')
          )
        )
      )
      
    } else if (loaded$dataset == "Andrew's") {
      exceptions <- c("UID", "region", "date", "year", "month", "day", "count")
      numeric_cols <- names(d)[sapply(d, is.numeric)]
      cols_to_format <- setdiff(numeric_cols, exceptions)
      d[cols_to_format] <- lapply(d[cols_to_format], function(x) sprintf("%.2f", x))
      
      DT::datatable(
        d,
        rownames = FALSE,
        colnames = c("UID", "Region", "Date", "Year", "Month", "Day", "Lon", "Lat", "Count",
                     "Elevation (m)", "Distance to coast (km)", "Precip. 21 d (mm)",
                     "Tmin (C)", "Tmax (C)", "Rel. Humidity (%)", "Mangroves (%)",
                     "Waterbodies (%)"),
        filter = 'top',
        class = 'display compact',
        options = list(
          columnDefs = list(
            list(className = 'dt-right', targets = '_all')
          )
        )
      )
    }
  })
  
  # DISTRIBUTION OBSERVER
  observeEvent(input$update_distribution, {
    if (loaded$dataset == "Nigel's") {
      base_dir <- "C:/Users/men118/Documents/gd-project/outputs/centroids/grid_5x5-QLD/predictions_distribution_nigel"
    } else if (loaded$dataset == "Andrew's") {
      base_dir <- "C:/Users/men118/Documents/gd-project/outputs/centroids/grid_5x5-QLD/predictions_distribution_andrew"
    }
    folder <- "maps"
    target_folder <- file.path(base_dir, folder)
    
    if (loaded$dataset == "Nigel's") {
      target_file_dist_w <- paste0("distribution_map_",
                                   input$dist_model, "_data_",
                                   input$dist_data_type, "_",
                                   input$dist_year,
                                   "_W.html")
      target_file_dist_d <- ""  # For Nigel's dataset, no dry map
    } else {
      target_file_dist_w <- paste0("distribution_map_",
                                   input$dist_model, "_data_",
                                   input$dist_data_type, "_",
                                   input$dist_year,
                                   "_W.html")
      target_file_dist_d <- paste0("distribution_map_",
                                   input$dist_model, "_data_",
                                   input$dist_data_type, "_",
                                   input$dist_year,
                                   "_D.html")
    }
    
    dist_wet_html_file <- file.path(target_folder, target_file_dist_w)
    dist_dry_html_file <- file.path(target_folder, target_file_dist_d)
    
    output$dist_map_W_html <- renderUI({
      if (file.exists(dist_wet_html_file)) {
        html_content <- paste(readLines(dist_wet_html_file, warn = FALSE), collapse = "\n")
        tags$iframe(srcdoc = html_content, width = "100%", height = "600px", frameborder = "0")
      } else {
        h5("Wet Season (Distribution) map file not found.")
      }
    })
    
    output$dist_map_D_html <- renderUI({
      if (target_file_dist_d != "" && file.exists(dist_dry_html_file)) {
        html_content <- paste(readLines(dist_dry_html_file, warn = FALSE), collapse = "\n")
        tags$iframe(srcdoc = html_content, width = "100%", height = "600px", frameborder = "0")
      } else {
        if (loaded$dataset == "Nigel's") {
          h5("No map available for this dataset.")
        } else {
          h5("Dry Season (Distribution) map file not found.")
        }
      }
      })
    })
  
  # ABUNDANCE OBSERVER
  observeEvent(input$update_abundance, {
    thres_formatted <- sprintf("%.2f", input$abund_threshold)
    if (loaded$dataset == "Nigel's") {
      base_dir <- "C:/Users/men118/Documents/gd-project/outputs/centroids/grid_5x5-QLD/predictions_abundance_nigel"
    } else if (loaded$dataset == "Andrew's") {
      base_dir <- "C:/Users/men118/Documents/gd-project/outputs/centroids/grid_5x5-QLD/predictions_abundance_andrew"
    }
    folder <- paste0("centroids_predictions_thres_", thres_formatted, "/maps")
    target_folder <- file.path(base_dir, folder)
    if (loaded$dataset == "Nigel's") {
      target_file_w <- paste0("abundance_map_",
                              input$abund_dist_model, "_",
                              input$abund_model, "_data_",
                              input$abund_data_type, 
                              "_thres_",
                              thres_formatted, "_",
                              input$abund_year,
                              "_W.html")
      target_file_d <- ""  # No dry map for Nigel's dataset
    } else {
      target_file_w <- paste0("abundance_map_",
                              input$abund_dist_model, "_",
                              input$abund_model, "_data_",
                              input$abund_data_type, 
                              "_thres_",
                              thres_formatted, "_",
                              input$abund_year,
                              "_W.html")
      target_file_d <- paste0("abundance_map_",
                              input$abund_dist_model, "_",
                              input$abund_model, "_data_",
                              input$abund_data_type, 
                              "_thres_",
                              thres_formatted, "_",
                              input$abund_year,
                              "_D.html")
    }
    wet_html_file <- file.path(target_folder, target_file_w)
    dry_html_file <- file.path(target_folder, target_file_d)
    
    output$abund_map_W_html <- renderUI({
      if (file.exists(wet_html_file)) {
        html_content <- paste(readLines(wet_html_file, warn = FALSE), collapse = "\n")
        tags$iframe(srcdoc = html_content, width = "100%", height = "600px", frameborder = "0")
      } else {
        h5("Wet Season (Abundance) map file not found.")
      }
    })
    
    output$abund_map_D_html <- renderUI({
      if (target_file_d != "" && file.exists(dry_html_file)) {
        html_content <- paste(readLines(dry_html_file, warn = FALSE), collapse = "\n")
        tags$iframe(srcdoc = html_content, width = "100%", height = "600px", frameborder = "0")
      } else {
        h5("Dry Season (Abundance) map file not found.")
      }
    })
  })
}

shinyApp(ui, server)
