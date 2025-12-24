# -----------------------------------------------------------------------------
# Farauti Shiny app (MINIMAL: abundance only)
# -----------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(data.table)
library(fst)
library(leaflet)
library(DT)
library(ggplot2)
library(scales)


# ---- Paths / large file config ------------------------------------------------
library(here)
# Allow larger uploads (default is ~5MB)
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB
CENTROIDS_NAME <- "centroids_5x5_qld_with_predictions_shiny-app.fst"
# Predictions folder (used by the UI + default file location)
pred_dir <- tryCatch(
  here::here("outputs", "predictions"),
  error = function(e) file.path(getwd(), "outputs", "predictions")
)
default_centroids <- file.path(pred_dir, CENTROIDS_NAME)
# Optional override via env var (power users / different file location)
env_centroids <- Sys.getenv("FARAUTI_CENTROIDS", unset = "")
centroids_file <- if (nzchar(env_centroids)) env_centroids else default_centroids

# columns we need for abundance computation
needed_cols <- c(
  "grid_id","lon","lat","date","season",
  "elev", "water_occ", "water_occ_99", "mang_rf_5km", "ppa21", "tmaxm21", "tminm21", "rhm21",       
  paste0("pi_", c("rf","brt","max","glm","gam","ens")),
  paste0("mu_", c("rf","brt","glm","gam","ens"))
)

# ---- App metadata ------------------------------------------------------------
APP_TITLE   <- "Identifying mozzie hotspots"
APP_SUBTITLE <- "Abundance across Queensland using hurdle models"

# Put the names you want shown in the About tab:
APP_AUTHORS <- c(
  "Manuela Mendiolar" #,
  #"CO-AUTHOR NAME 2",
  #"CO-AUTHOR NAME 3"
)

# Optional (shows a contact line)
APP_CONTACT <- "manuela.mendiolar@csiro.au"
# Optional (if you want a citation line)
APP_CITATION <- "Please cite: https://doi.org/10.5281/zenodo.17984922."



# -----------------------------------------------------------------------------
# UI helpers (small "pretty" wrappers)
# -----------------------------------------------------------------------------
infoPill <- function(label, value) {
  div(class = "pill",
      span(class = "pill-label", label),
      span(class = "pill-value", value)
  )
}

# ==========================================
# UI
# ==========================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$title("🦟 An. farauti"),
    tags$meta(charset = "utf-8"),
    tags$style(HTML("
      body { background: #f5f7fb; }
      .app-header { background: white; border-radius: 14px; padding: 16px 18px; margin-bottom: 14px;
                    box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06); display: flex;
                    align-items: center; justify-content: space-between; gap: 12px; }
      .app-title h3 { margin: 0; }
      .app-title h4, .app-title h5 { margin: 0; color: #475569; }
      .app-sub { margin-top: 6px; color: #64748b; font-size: 12px; }
      .app-badges code { background: #f1f5f9; padding: 2px 6px; border-radius: 6px; }
      .card { background: white; border-radius: 14px; padding: 14px; margin-bottom: 12px;
              box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
              border: 1px solid rgba(148,163,184,0.25); }
      .btn-primary { border-radius: 10px; }
      #hurdle_status { height: 170px; overflow-y: auto; background: #ffffffff; color: #3a5983ff;
                       border-radius: 12px; padding: 10px; }
      pre { white-space: pre-wrap; }
      #status, #status_box { background:#ffffffff; color: #3a5983ff; border-radius: 12px; padding: 10px; }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border-radius: 10px !important;
        border: 1px solid rgba(148,163,184,0.35) !important;
        padding: 4px 8px !important;
      }
      .dataTables_wrapper .dt-buttons .btn { border-radius: 10px !important; }
    "))
  ),
  div(
    class = "app-header",
    div(
      class = "app-title",
      h3("Identifying mozzie hotspots"),
      h4(em("Anopheles farauti")),
      h5("Abundance across Queensland using hurdle models")
    )
  ),
  navbarPage(
    title = tagList(tags$span("🦟", style = "font-size:18px; margin-right:6px;")),
    # ABUNDANCE TAB
    tabPanel(
      "Abundance",
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          tags$h4("Settings"),
          selectInput("dist_model_h", "Distribution model", choices = c("BRT","RF","MaxEnt","GLM","GAM","ENS"), selected = "BRT"),
          selectInput("abund_model_h", "Abundance model", choices = c("RF","BRT","GLM","GAM","ENS"), selected = "RF"),
          numericInput("tau_h", "Threshold (τ)", value = 0.25, min = 0, max = 1, step = 0.05),
          tags$h4("Trap efficiency (%)"),
          fluidRow(
            column(6, numericInput("p_wet", "Wet season", value = 1.23, min = 0.0001, step = 0.01)),
            column(6, numericInput("p_dry", "Dry season", value = 1.66, min = 0.0001, step = 0.01))
          ),
          tags$h4("Uncertainty bands"),
          sliderInput("conf", "Confidence", min = 0.5, max = 0.99, value = 0.95, step = 0.01),
          numericInput("theta_nb", "NegBin theta (size)", value = 8, min = 0.01, step = 0.1),
          actionButton("update_hurdle", "Update map"),
          hr(),
          tags$h4("Status"),
          verbatimTextOutput("hurdle_status"),
          verbatimTextOutput("centroids_path_txt")
        ),
        mainPanel(
          width = 8,
          leafletOutput("hurdle_map", height = 520),
          hr(),
          div(
            class = "card",
            tags$h4("Time series at selected grid"),
            tags$p(class = "hint", "Tip: click any point on the map to draw the time series and predictive interval."),
            plotOutput("ts_plot", height = 280)
          )
        )
      )
    ),
    # DATA TAB
    tabPanel(
      "Data",
      fluidPage(
        div(
          class = "card",
          tags$h4("Centroids file (.fst)"),
          tags$p("Recommended: download from Zenodo and place it in the predictions folder. Or choose a local file below (best for local runs)."),
          uiOutput("upload_ui"), #fileInput("centroids_upload", "Choose centroids .fst file", accept = c(".fst"), multiple = FALSE),
          tags$small(
            style="color:#64748b;", 
            "Default path: ", tags$code(default_centroids), tags$br(),
            "Override with env var FARAUTI_CENTROIDS: ", tags$code(if (nzchar(env_centroids)) env_centroids else "<not set>")
          )
        ),
        div(class="card", tags$h4("Loaded data preview"), DTOutput("head_dt")) #,
        #div(class="card", tags$h4("Computed map_dt preview"), DTOutput("map_preview"))
      )
    ),
    # ABOUT TAB
    tabPanel(
      "About",
      fluidPage(
        div(
          class = "card",
          fluidRow(
            # ---- LEFT: text ----
            column(
              width = 7,
              h3("About this app"),
              p(
                "This Shiny app provides an interactive view of ",
                tags$em("Anopheles farauti"),
                " predicted abundance across Queensland using a hurdle modelling approach."
              ),
              div(style="height:14px;"),
              tags$h4("How to use"),
              tags$ul(
                tags$li(tags$b("Abundance tab:"), " choose a distribution model and an abundance model, set the threshold (τ) and trap efficiencies, then click ", tags$em("Update map"), ". Click any grid cell to show its time series with predictive intervals."),
                tags$li(tags$b("Data tab:"), " confirm the centroids .fst file is found (or upload it), then inspect a preview of the loaded data. The preview highlights the currently selected model outputs (pi_… and mu_…) alongside environmental covariates.")
              ),
              div(style="height:14px;"),
              tags$h4("Data & file location"),
              p("This app reads a large centroids predictions file (", tags$code(CENTROIDS_NAME), ")."),
              tags$ul(
                tags$li("Default expected location: ", tags$code(file.path("outputs", "predictions", CENTROIDS_NAME))),
                tags$li("You can override the file path via environment variable: ", tags$code("FARAUTI_CENTROIDS"), " (useful if you keep the file elsewhere).")
              ),
              div(style="height:14px;"),
              tags$h4("Authors"),
              p("Manuela Mendiolar"),
              p(tags$b("Contact:"), " manuela.mendiolar@csiro.au"),
              div(style="height:14px;"),
              tags$h4("Citation"),
              p("Please cite: ", tags$code("https://doi.org/10.5281/zenodo.17984922"))
            ),
            # ---- RIGHT: image ----
            column(
              width = 5,
              tags$img(
                src = "Anopheles-farauti.png",
                style = "width: 100%; max-width: 520px; display: block; margin: 8px auto; border-radius: 14px;
                         border: 1px solid rgba(148,163,184,0.35); box-shadow: 0 8px 24px rgba(15, 23, 42, 0.08);"
              ),
              tags$div(
                style = "text-align:center; color:#94a3b8; font-size:11px; margin-top:6px;",
                  "Image: CDC/ James Gathany"
              )
            )
          )
        )
      )
    )
  )
)



# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session) {

  # ---- Reactive values ----
  hurdle_msg <- reactiveVal("Starting up...")
  centroids  <- reactiveVal(NULL)
  map_dt_rv  <- reactiveVal(NULL)
  selected_grid <- reactiveVal(NULL)
  centroids_path <- reactiveVal(centroids_file)

  output$upload_ui <- renderUI({
  fp <- centroids_path()
  if (file.exists(fp)) {
    tags$div(
      class = "hint",
      tags$strong("✓ File found."),
      tags$span(" Using default/ENV path (no upload needed).")
    )
  } else {
    fileInput("centroids_upload", "Choose centroids .fst file", accept = c(".fst"), multiple = FALSE)
  }
})
  
  output$centroids_path_txt <- renderText({
    fp <- centroids_path()
    paste0("Using centroids file:\n", normalizePath(fp, winslash = "/", mustWork = FALSE))
  })
  
  # ---- Centroids file (if uploading) ----
  observeEvent(input$centroids_upload, {
    req(input$centroids_upload)
    centroids_path(input$centroids_upload$datapath)
    hurdle_msg(paste0("Using uploaded file:\n", input$centroids_upload$name))
  })
  
  # ----- Load data ONCE -----
  observeEvent(centroids_path(), {
    fp <- centroids_path()
    hurdle_msg(paste0("Reading: ", fp))
    # error msg
    if (!file.exists(fp)) {
      hurdle_msg(paste0(
        "ERROR: file not found:\n", fp,
        "\n\nDownload the .fst from Zenodo and place it in:\n", centroids_file,
        "\nOr use the Data tab to upload/select it."
      ))
      centroids(NULL)
      return()
    }
    # only read the columns we need
    dt <- fst::read_fst(fp, columns = needed_cols) |> as.data.table()
    
    # basic sanity
    missing <- setdiff(needed_cols, names(dt))
    if (length(missing) > 0) {
      hurdle_msg(paste0("ERROR: missing columns in file:\n- ", paste(missing, collapse = "\n- ")))
      centroids(NULL)
      return()
    }
    # date handling
    if (!inherits(dt$date, "Date")) {
      if (is.numeric(dt$date)) dt[, date := as.Date(date, origin = "1970-01-01")]
      else dt[, date := as.Date(date)]
    }
    # season handling
    dt[, season := as.character(season)]
    dt[season %in% c("dry","Dry","D"), season := "D"]
    dt[season %in% c("wet","Wet","W"), season := "W"]
    
    centroids(dt)
    hurdle_msg(paste0(
      "Loaded OK.\nFile: ", fp, "\n",
      "Rows: ", nrow(dt), "\nCols: ", ncol(dt), "\n",
      "date class: ", paste(class(dt$date), collapse=", "), "\n",
      "season values: ", paste(head(unique(dt$season), 5), collapse=", ")
    ))
  }, ignoreInit = FALSE)

  # empty map placeholder
  output$hurdle_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager,  group = "Voyager (more labels)") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light (Positron)") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo (terrain)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (imagery)") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street (Esri)") %>%
      addLayersControl(
        baseGroups = c("Voyager (more labels)","Light (Positron)","Street (Esri)","Topo (terrain)","Satellite (imagery)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Street (Esri)", "Topo (terrain)", "Satellite (imagery)")) %>%
      setView(lng = 148.5, lat = -15.75, zoom = 5.5)
  })

  # status output MUST be character
  output$hurdle_status <- renderText({
    txt <- hurdle_msg()
    if (is.null(txt)) return("")
    paste(as.character(txt), collapse = "\n")
  })

  
  # ----- Compute abundance ONLY when button clicked -----
  observeEvent(input$update_hurdle, {
    dt <- centroids()
    req(dt)
    selected_grid(NULL) # reset the selected grid when the map updates
    leafletProxy("hurdle_map") %>% removeMarker("selected") # clear the highlight when you click “Update map”

    # validate inputs
    validate(
      need(is.finite(input$p_wet) && input$p_wet > 0, "Wet efficiency must be > 0"),
      need(is.finite(input$p_dry) && input$p_dry > 0, "Dry efficiency must be > 0"),
      need(is.finite(input$tau_h) && input$tau_h >= 0 && input$tau_h <= 1, "tau must be in [0,1]")
    )

    eff_wet <- input$p_wet / 100
    eff_dry <- input$p_dry / 100
    tau <- input$tau_h

    pi_col <- paste0("pi_", tolower(input$dist_model_h))
    mu_col <- paste0("mu_", tolower(input$abund_model_h))

    validate(
      need(pi_col %in% names(dt), paste0("Missing column: ", pi_col)),
      need(mu_col %in% names(dt), paste0("Missing column: ", mu_col))
    )

    # filter to time window
    dt2 <- dt[date >= as.Date("1995-01-01") & date <= as.Date("1997-12-31")]
    validate(need(nrow(dt2) > 0, "No rows in 1995–1997."))

    # filter to rows that pass tau + finite preds
    dt_sel <- dt2[is.finite(get(pi_col)) & is.finite(get(mu_col)) & get(pi_col) > tau]
    validate(need(nrow(dt_sel) > 0, "No rows pass tau after filtering."))

    # compute mean abundance per grid (trap-eff adjusted)
    map_dt <- dt_sel[, .(
      lon = lon[1],
      lat = lat[1],
      mean_abund = mean((get(pi_col) * get(mu_col)) / ifelse(season == "D", eff_dry, eff_wet), na.rm = TRUE)
    ), by = grid_id]

    # IMPORTANT debug checkpoint BEFORE plotting
    map_dt <- map_dt[is.finite(mean_abund) & mean_abund > 0]

    map_dt_rv(map_dt)

    # print some debug stats in the status box
    hurdle_msg(paste0(
      "Computed map_dt.\n",
      "pi=", pi_col, "\n",
      "mu=", mu_col, "\n",
      "tau=", tau,"\n",
      "rows(map_dt)=", nrow(map_dt), "\n",
      "mean_abund summary:\n",
      paste(capture.output(print(summary(map_dt$mean_abund))), collapse="\n")
      )
    )

    # If empty after filtering, don't plot
    validate(need(nrow(map_dt) > 0, "map_dt empty after mean_abund > 0 filtering."))

    # ---- ABUNDANCE MAP ----
    pal <- colorNumeric("YlOrRd", domain = range(map_dt$mean_abund, na.rm = TRUE))

    leafletProxy("hurdle_map", data = map_dt) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat,
        layerId = ~as.character(grid_id),
        radius = 3, 
        stroke = FALSE, 
        fillOpacity = 0.85,
        color = ~pal(mean_abund)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~mean_abund, title = "Abundance") %>%
      addLayersControl(
        baseGroups = c("Voyager (more labels)","Light (Positron)","Street (Esri)","Topo (terrain)","Satellite (imagery)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Street (Esri)", "Topo (terrain)", "Satellite (imagery)"))
  })


  # ---- CLICKED GRID ID ---- 
  # store clicked grid
  observeEvent(input$hurdle_map_marker_click, {
    click <- input$hurdle_map_marker_click
    req(click$id)
    gid <- as.integer(click$id)
    selected_grid(gid)
    
    md <- map_dt_rv()
    req(md)
    pt <- md[grid_id == gid]
    validate(need(nrow(pt) == 1, "Selected grid not found in current map view."))
    
    leafletProxy("hurdle_map") %>%
      removeMarker("selected") %>%   # remove previous ring
      addCircleMarkers(
        lng = pt$lon, 
        lat = pt$lat,
        radius = 7,                  # bigger than base marker (3)
        stroke = TRUE, 
        weight = 2,
        fillColor = "white",
        fillOpacity = 0,             # ring only
        color = "#111111",
        layerId = "selected"
      )

  })
  # and draw the time series
  output$ts_plot <- renderPlot({
    dt <- centroids()
    gid <- selected_grid()
    req(dt)
    validate(need(!is.null(gid), "Click a grid cell on the map to show its time series.")) # show a friendly message 

    tau <- input$tau_h
    eff_wet <- input$p_wet / 100
    eff_dry <- input$p_dry / 100
    pi_col <- paste0("pi_", tolower(input$dist_model_h))
    mu_col <- paste0("mu_", tolower(input$abund_model_h))
    req(pi_col %in% names(dt), mu_col %in% names(dt))
    # same window as map
    dtg <- dt[grid_id == gid & date >= as.Date("1995-01-01") & date <= as.Date("1997-12-31")]
    validate(need(nrow(dtg) > 0, "No data for selected grid in 1995–1997."))

    # hurdle abundance per-row (below tau => 0)
    dtg[, pi := get(pi_col)]
    dtg[, mu := get(mu_col)]
    dtg[, eff := ifelse(season == "D", eff_dry, eff_wet)]
    dtg[, abund := data.table::fifelse(
      is.finite(pi) & is.finite(mu) & is.finite(eff) & eff > 0 & pi > tau, (pi * mu) / eff, 0
      )]
    ts <- dtg[, .(abund = mean(abund, na.rm = TRUE)), by = date][order(date)]
    #ts[, abund := round(abund)]

    # NB predictive intervals (like your manuscript)
    conf <- input$conf
    th   <- input$theta_nb
    alpha <- 1 - conf
    
    ts[, lower := qnbinom(alpha/2, mu = pmax(abund, 1e-8), size = th)]
    ts[, upper := qnbinom(1 - alpha/2, mu = pmax(abund, 1e-8), size = th)]
    
    # Time-series plot: mean abundance + bands
    ggplot(ts, aes(date)) +
      geom_ribbon(
        aes(ymin = lower, ymax = upper), 
        fill = "grey50", 
        alpha = 0.15, 
        colour = NA
      ) +
      geom_line(
        aes(y = abund),
         colour = "#222222", 
         linewidth = 0.35
        ) +
      geom_hline(
        yintercept = 0, 
        colour = "grey75", 
        linewidth = 0.25
      ) +
      scale_x_date(
        date_labels = "%b-%y", 
        date_breaks = "1 months",
        expand = expansion(c(0.01, 0.03))
      ) +
      labs(
        x = NULL, 
        y = "Abundance",
        title = paste0("Grid ", gid)
      ) +
      theme_bw(base_size = 12) +
      theme(
        panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13, angle = 90),
        plot.title  = element_text(size = 10, face = "bold", hjust = 1.0)
      )
  })

  
  # ---- For the DATA TAB ----
  
  # show computed table
  output$map_preview <- renderDT({
    md <- map_dt_rv()
    req(md)
    md_show <- head(md[order(-mean_abund)], 50)  # show more if you want
    dt_out <- DT::datatable(
      md_show,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tip",
        columnDefs = list(
          list(targets = 0, visible = FALSE)  # hide first column (grid_id)
          )
      )
    )
    # round ALL numeric columns to 2 dp
    num_cols <- names(md_show)[vapply(md_show, is.numeric, logical(1))]
    DT::formatRound(dt_out, columns = num_cols, digits = 2)
  })
  # show centroids table
  output$head_dt <- renderDT({
    dt <- centroids()
    req(dt)
    # build the selected columns from the dropdowns
    pi_col <- paste0("pi_", tolower(input$dist_model_h))
    mu_col <- paste0("mu_", tolower(input$abund_model_h))
    # "other covariates" / context columns to always show
    base_cols <- c("grid_id", "lon", "lat", "date", "season",
      "elev","water_occ","water_occ_99",
      "mang_rf_5km","ppa21","tmaxm21","tminm21", "rhm21")
    keep_cols <- unique(c(base_cols, pi_col, mu_col))
    keep_cols <- keep_cols[keep_cols %in% names(dt)]  # safety
    dt_show <- head(dt[, ..keep_cols], 200)
    dt_out <- DT::datatable(
      dt_show,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tip",
        columnDefs = list(list(targets = 0, visible = FALSE))
      )
    )
    # round ALL numeric columns to 2 dp
    num_cols <- names(dt_show)[vapply(dt_show, is.numeric, logical(1))]
    DT::formatRound(dt_out, columns = num_cols, digits = 4)
  })

}

shinyApp(ui, server)
