# -----------------------------------------------------------------------------
# Farauti Shiny app
# -----------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(data.table)
library(fst)
library(leaflet)
library(ggplot2)
library(MASS)   
library(shinycssloaders)
library(DT)

Sys.setenv(FARAUTI_PRED_DIR = "C:/Users/men118/Projects/farauti-abundance-qld-hurdle-model/outputs/predictions")

# ---- Paths (NO dir_data/dir_tables; use env var or relative path) ----
pred_dir <- Sys.getenv(
  "FARAUTI_PRED_DIR",
  unset = file.path(getwd(), "predictions")
)

centroids_file <- file.path(pred_dir, "centroids_5x5_qld_with_predictions.fst")

# ---- Helpers ----
std_model <- function(x) tolower(trimws(x))

guess_suit_col <- function(df, dist_model) {
  dm <- std_model(dist_model)
  cand <- paste0("pi_", dm)
  if (cand %in% names(df)) return(cand)
  NA_character_
}

# ---- Suitability colour palette ----
heat_diverging <- c(
  "#053061", "#2166AC", "#4393C3", "#92C5DE",
  "#f7f786ff", "#FFFF8C", "#FFE75E", "#FCD070",
  "#D6604D", "#B2182B", "#67001F"
)


# ==========================================
# ---- UI ----
# ==========================================
ui <- fluidPage(

  theme = shinytheme("flatly"),

  tags$head(
    tags$title("🦟 An. farauti"),
    tags$meta(charset = "utf-8"),
    tags$style(HTML("
    body { background: #f5f7fb; }
    
    .app-header {
      background: white;
      border-radius: 14px;
      padding: 16px 18px;
      margin-bottom: 14px;
      box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 12px;
    }
    .app-title h3 { margin: 0; }
    .app-title h4, .app-title h5 { margin: 0; color: #475569; }
    .app-sub { margin-top: 6px; color: #64748b; font-size: 12px; }
    .app-badges code { background: #f1f5f9; padding: 2px 6px; border-radius: 6px; }

    .card {
      background: white;
      border-radius: 14px;
      padding: 14px;
      margin-bottom: 12px;
      box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
      border: 1px solid rgba(148,163,184,0.25);
    }

    .sidebar .form-group { margin-bottom: 10px; }
    .btn-primary { border-radius: 10px; }

    #hurdle_status { height: 170px; overflow-y: auto; background: #ffffffff; color: #3a5983ff; border-radius: 12px; padding: 10px; }
    pre { white-space: pre-wrap; }

    #status { background: #ffffffff; color: #3a5983ff; border-radius: 12px; padding: 10px; }

    #status_box { background:#ffffffff; color: #3a5983ff; border-radius: 12px; padding: 10px; }

    .dataTables_wrapper .dataTables_filter input,
    .dataTables_wrapper .dataTables_length select {
      border-radius: 10px !important;
      border: 1px solid rgba(148,163,184,0.35) !important;
      padding: 4px 8px !important;
    }
    .dataTables_wrapper .dt-buttons .btn {
      border-radius: 10px !important;
    }
  "))),

  div(
    class = "app-header",
    div(
      class = "app-title",
      h3("Identifying mozzie hotspots"),
      h4(em("Anopheles farauti")),
      h5("Predicting abundance across Queensland"),
      div(class = "app-sub",
          span("Data source: "),
          span(class="app-badges", tags$code(pred_dir)),
          span(" • Centroids file: "),
          span(class="app-badges", tags$code(basename(centroids_file)))
      )
    ),
    #tags$img(
    #  src = "anopheles_farauti_dontbugme.png",
    #  height = "100px",
    #  style = "opacity: 0.95;"
    #)
  ),

  navbarPage(
    title = tagList(
      tags$span("🦟", style = "font-size:18px; margin-right:6px;")
    ),
    tabPanel(
      "Suitability",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class = "card",
            selectInput("dist_model", "Distribution model",
                        choices = c("BRT"="brt","RF"="rf","MAX"="max","GLM"="glm","GAM"="gam","Ensemble"="ens"),
                        selected = "brt"),
            numericInput("tau", label = HTML("Threshold &tau;"), value = 0.25, min = 0, max = 1, step = 0.01),
            actionButton("update_suit", "Update map", class = "btn btn-primary")
          ),
          div(class = "card",
            tags$small("Status"),
            div(id = "status_box", verbatimTextOutput("status", placeholder = TRUE))
          )
        ),
        mainPanel(
          div(class="card",
            withSpinner(leafletOutput("suit_map", height = "720px"), type = 6)
          )
        )
      )
    ),

    tabPanel(
      "Abundance",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class="card",
            selectInput("dist_model_h", "Distribution model",
                        choices = c("BRT"="brt","RF"="rf","MAX"="max","GLM"="glm","GAM"="gam","Ensemble"="ens"),
                        selected = "brt"),
            selectInput("abund_model_h", "Abundance model (conditional on presence)",
                        choices = c("RF"="rf","BRT"="brt","GLM"="glm","GAM"="gam","ENS"="ens"),
                        selected = "rf"),
            numericInput("tau_h", label = HTML("Threshold &tau;"), value = 0.25, min = 0, max = 1, step = 0.25),
            selectInput("conf", "Predictive interval",
                        choices = c("90%"=0.90, "95%"=0.95, "99%"=0.99),
                        selected = 0.95),
            numericInput("theta", label = HTML("&theta; (NB dispersion)"), value = NA_real_, min = 0.001, step = 0.1),
            actionButton("update_hurdle", "Update map", class = "btn btn-primary")
            ),
          div(class="card",
            tags$h5("Trap efficiency"),
            tags$small("Defaults are the mean trap efficiencies from Chow et al. (used in Fig. 4). Enter as percent (%)."),
            br(),
            numericInput("p_wet", "Wet season (%)", value = 1.231544, min = 0.0001, step = 0.01),
            numericInput("p_dry", "Dry season (%)", value = 1.660359, min = 0.0001, step = 0.01),
            tags$small(tags$em("Conversion used: abundance = trapcounts / (p/100)")),
            br(),
            actionButton("reset_eff", "Reset to defaults", class = "btn btn-default btn-sm")
          ),
          div(class="card",
            tags$small("Clicked grid summary"),
            verbatimTextOutput("grid_summary", placeholder = TRUE)
          ),
          div(class="card",
            tags$small("Status / messages"),
            verbatimTextOutput("hurdle_status")
          )
        ),

        mainPanel(
          div(class="card",
            withSpinner(leafletOutput("hurdle_map", height = "520px"), type = 6)
          ),
          div(class="card",
            withSpinner(plotOutput("ts_plot", height = "260px"), type = 6)
          )
        )
      )
    ),

    tabPanel(
      "Data",
      fluidPage(
        h4("Loaded data preview"),
        DTOutput("head_dt"),
        hr(),
        h4("Columns"),
        verbatimTextOutput("cols_txt")
      )
    ),

    tabPanel(
      "About",
      fluidPage(
        div(class = "card",
          h3("About this app"),
          p(
            "This Shiny app provides an interactive view of ",
            tags$em("Anopheles farauti"),
            " predicted abundance across Queensland based on a hurdle model approach."
          ),
          tags$ul(
            tags$li(tags$b("Suitability tab:"), " explore predicted suitability (pi_*) and apply a threshold \u03C4."),
            tags$li(tags$b("Abundance tab:"), " view mean abundance (trap-efficiency adjusted) for 1995–1997 and click a grid cell to see its time series with predictive intervals."),
            tags$li(tags$b("Data tab:"), " sanity-check what was loaded from the centroids predictions file.")
          ),
          hr(),
          p(tags$b("Data location:"), " ", tags$code(pred_dir)),
          p(tags$b("Centroids file:"), " ", tags$code(basename(centroids_file))),
          tags$img(
            src = "anopheles_farauti_dontbugme.png",
            style = "width: 100%; max-width: 520px; display: block; margin: 12px auto; border-radius: 14px; border: 1px solid rgba(148,163,184,0.35); box-shadow: 0 8px 24px rgba(15, 23, 42, 0.08);"
          )
        )
      )
    )
  )
)

# ==========================================
# ---- Server ----
# ==========================================
server <- function(input, output, session) {

  centroids <- reactiveVal(NULL)

  output$cols_txt <- renderPrint({
    dt <- centroids()
    req(dt)
    cat(paste(names(dt), collapse = "\n"))
  })

  eff_defaults <- list(p_wet = 1.231544, p_dry = 1.660359)
  observeEvent(input$reset_eff, {
    updateNumericInput(session, "p_wet", value = eff_defaults$p_wet)
    updateNumericInput(session, "p_dry", value = eff_defaults$p_dry)
  })
  
  hurdle_dt <- reactiveVal(NULL)
  
  map_dt_rv <- reactiveVal(NULL)

  selected_grid <- reactiveVal(NA_integer_)

  observeEvent(input$hurdle_map_marker_click, {
    click <- input$hurdle_map_marker_click
    if (!is.null(click$id)) selected_grid(as.integer(click$id))
  })

  # Selected grid
  observeEvent(selected_grid(), {
    gid <- selected_grid()
    req(is.finite(gid))
    
    dt_ts <- hurdle_dt()
    req(dt_ts)

    xy <- dt_ts[grid_id == gid][1]
    req(nrow(xy) == 1)

    leafletProxy("hurdle_map") %>%
      removeShape("selected") %>%
      addCircleMarkers(
        lng = xy$lon, 
        lat = xy$lat,
        radius = 7,
        color = "black",
        weight = 2,
        fillColor = "white",
        fillOpacity = 0.2,
        layerId = "selected"
      )
  })

  # Load once on startup
  observeEvent(TRUE, {
    if (!file.exists(centroids_file)) {
      centroids(NULL)
      return()
    }
    dt <- fst::read_fst(centroids_file) |> as.data.table()
    centroids(dt)
  }, once = TRUE)

  # ---- Auto-fill theta if possible (works if ab_data + estimate_theta exist) ----
  observe({
    if (is.finite(input$theta)) return()  # user already set it

    if (exists("estimate_theta", mode = "function") &&
        exists("ab_data", envir = .GlobalEnv)) {

      th <- try(estimate_theta(get("ab_data", envir = .GlobalEnv)), silent = TRUE)
      if (!inherits(th, "try-error") && is.finite(th) && th > 0) {
        updateNumericInput(session, "theta", value = th)
      }
    }
  })

  output$status <- renderPrint({
    dt <- centroids()
    if (is.null(dt)) {
      cat("Data not loaded.\n")
      cat("Check centroids_file path:\n", centroids_file, "\n")
    } else {
      cat("Loaded rows:", nrow(dt), "\n")
      cat("Columns:", length(names(dt)), "\n")
      cat("Example cols:", paste(head(names(dt), 12), collapse = ", "), "\n")
      if ("date" %in% names(dt)) {
        cat("date class:", paste(class(dt$date), collapse = ", "), "\n")
      }
    }
  })

  output$head_dt <- renderDT({
    dt <- centroids()
    req(dt)

    wanted <- c("elev","water_occ","water_occ_99","mang_rf_5km","ppa21","tmaxm21","tminm21","rhm21")
    missing <- setdiff(wanted, names(dt))
    validate(need(length(missing) == 0,
                  paste("Missing columns in centroids data:", paste(missing, collapse = ", "))))

    out <- dt[, c(
      .(lon = lon[1], lat = lat[1]),
      lapply(.SD, function(x) mean(x, na.rm = TRUE))
    ), by = grid_id, .SDcols = wanted]

    out <- out[order(grid_id)]

    DT::datatable(
      out,
      rownames = FALSE,
      filter = "top",
      extensions = c("Buttons"),
      class = "compact stripe hover cell-border",
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )) |>
      DT::formatRound(columns = c("lon","lat"), digits = 2) %>%
      DT::formatRound(columns = wanted, digits = 2)
  })


  # ---- Suitability map ----
  output$suit_map <- renderLeaflet({
  leaflet() %>%
    # --- Base maps (pick the ones you like) ---
    addProviderTiles(providers$CartoDB.Positron, group = "Light (Positron)") %>%         # clean, good labels
    addProviderTiles(providers$CartoDB.Voyager,  group = "Voyager (more labels)") %>%   # more place names
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo (terrain)") %>%         # terrain-ish topo
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (imagery)") %>%    # landscape features

    # Optional extra: strong road/city labels
    addProviderTiles(providers$Esri.WorldStreetMap, group = "Street (Esri)") %>%

    # Layer control
    addLayersControl(
      baseGroups = c(
        "Light (Positron)",
        "Voyager (more labels)",
        "Street (Esri)",
        "Topo (terrain)",
        "Satellite (imagery)"
      ),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    hideGroup(c("Street (Esri)", "Topo (terrain)", "Satellite (imagery)")) %>%  # start clean
    setView(lng = 145.5, lat = -18.0, zoom = 5.5)
})


  observeEvent(input$update_suit, {
    dt <- centroids()
    req(dt)

    suit_col <- guess_suit_col(dt, input$dist_model)
    validate(need(!is.na(suit_col), "Could not find a suitability column for this model in the centroids file."))

    tau <- input$tau
    validate(need(is.finite(tau), "Pick a valid tau."))
    validate(need(all(c("lon", "lat") %in% names(dt)), "Expected columns named 'lon' and 'lat'."))

    map_dt <- dt[, .(grid_id, lon, lat, suit = get(suit_col))] |> unique(by = c("grid_id", "lon", "lat"))
    map_dt <- map_dt[is.finite(lon) & is.finite(lat) & is.finite(suit)]
    map_dt[, present := suit >= tau]

    pal <- colorNumeric(palette = heat_diverging, domain = map_dt$suit) # alternative: domain = c(0,1)

    leafletProxy("suit_map", data = map_dt) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 2.5, 
        stroke = FALSE,
        fillOpacity = 0.85,
        color = ~pal(suit),
        label = ~sprintf("grid: %s | suit: %.3f | present(tau=%.2f): %s", grid_id, suit, tau, present)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~suit, title = "Suitability")
  })


  # ---- Abundance map ----
  output$hurdle_map <- renderLeaflet({
  leaflet() %>%
    # --- Base maps ---
    addProviderTiles(providers$CartoDB.Positron, group = "Light (Positron)") %>%         # clean
    addProviderTiles(providers$CartoDB.Voyager,  group = "Voyager (more labels)") %>%   # more place names
    addProviderTiles(providers$Esri.WorldStreetMap, group = "Street (Esri)") %>%        # strong labels
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo (terrain)") %>%         # terrain context
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (imagery)") %>%    # imagery

    addLayersControl(
      baseGroups = c(
        "Light (Positron)",
        "Voyager (more labels)",
        "Street (Esri)",
        "Topo (terrain)",
        "Satellite (imagery)"
      ),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    hideGroup(c("Street (Esri)", "Topo (terrain)", "Satellite (imagery)")) %>%  # start clean
    setView(lng = 145.5, lat = -20.5, zoom = 6)
})


  hurdle_msg <- reactiveVal("Click 'Update map' to draw the abundance map.")
  output$hurdle_status <- renderText(hurdle_msg())

  observeEvent(input$update_hurdle, {

    dt <- centroids()
    req(dt)
    hurdle_msg("Starting…")

    # Ensure date is Date
    if ("date" %in% names(dt) && !inherits(dt$date, "Date")) {
      if (is.numeric(dt$date)) dt[, date := as.Date(dt$date, origin = "1970-01-01")]
      else dt[, date := as.Date(dt$date)]
    }

    # Ensure season is usable
    if (!("season" %in% names(dt))) {
      if (!("date" %in% names(dt)) || all(is.na(dt$date))) {
        hurdle_msg("ERROR: 'season' is missing and couldn't derive it (date missing/invalid).")
        return()
      }
      m <- as.integer(format(dt$date, "%m"))
      dt[, season := ifelse(m %in% 5:10, "D", "W")]
    } else {
      dt[, season := as.character(season)]
      dt[season %in% c("dry","Dry","D"), season := "D"]
      dt[season %in% c("wet","Wet","W"), season := "W"]
    }

    suit_col <- guess_suit_col(dt, input$dist_model_h)
    if (is.na(suit_col)) {
      hurdle_msg("ERROR: Could not find a suitability column for the selected distribution model.")
      return()
    }

    tau <- input$tau_h
    if (!is.finite(tau) || tau < 0 || tau > 1) {
      hurdle_msg("ERROR: tau must be between 0 and 1.")
      return()
    }

    abund_col <- paste0(
      std_model(input$dist_model_h), "_",
      std_model(input$abund_model_h), "_t",
      sprintf("%03d", round(tau * 100)),
      "_abund"
    )

    if (!(abund_col %in% names(dt))) {
      avail <- grep("_t\\d{3}_abund$", names(dt), value = TRUE)
      hurdle_msg(paste0(
        "ERROR: Missing abundance column: ", abund_col, "\n\n",
        "Available *_abund cols (first 25):\n",
        paste(head(avail, 25), collapse = "\n")
      ))
      return()
    }

    hurdle_msg(paste0("Using suit=", suit_col, " | abund=", abund_col, " | tau=", sprintf("%.2f", tau)))

    # Trap efficiency (Fig 4)
    #eff_wet <- 1.231544 / 100
    #eff_dry <- 1.660359 / 100
    # Trap efficiency (user-adjustable; inputs are percent)
    validate(need(is.finite(input$p_wet) && input$p_wet > 0, "Wet season trap efficiency must be > 0%."))
    validate(need(is.finite(input$p_dry) && input$p_dry > 0, "Dry season trap efficiency must be > 0%."))
    eff_wet <- input$p_wet / 100
    eff_dry <- input$p_dry / 100
    
    dt[, trapcounts := get(abund_col)]
    dt[, abundance := ifelse(season == "D", trapcounts / eff_dry, trapcounts / eff_wet)]

    # Fig 4 time window
    dt <- dt[date >= as.Date("1995-01-01") & date <= as.Date("1997-12-31")]

    # Store time-series data (pre-threshold, but already time-windowed)
    hurdle_dt(dt[, .(grid_id, lon, lat, date, abundance)])
    selected_grid(NA_integer_)

    # Apply threshold and summarise mean abundance
    dt <- dt[get(suit_col) >= tau]

    map_dt <- dt[
      is.finite(abundance) & abundance > 0,
      .(lon = lon[1], lat = lat[1], mean_abund = mean(abundance, na.rm = TRUE)),
      by = grid_id
    ]
    map_dt_rv(map_dt) # save map
    
    pal <- colorNumeric("YlOrRd", domain = map_dt$mean_abund)

    leafletProxy("hurdle_map", data = map_dt) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 3, stroke = FALSE, fillOpacity = 0.85,
        color = ~pal(mean_abund),
        label = ~sprintf("grid: %s | mean abundance: %.0f", grid_id, mean_abund),
        layerId = ~as.character(grid_id)
      ) %>%
      addLegend(
        "bottomright", 
        pal = pal, 
        values = ~mean_abund, 
        title = HTML("Abundance<br/><span style='font-size:11px;color:#475569'>(trap-efficiency adjusted)</span>")
      )

    hurdle_msg(paste0("Done. Mapped ", nrow(map_dt), " grid cells. Click a cell for the time series."))

    hurdle_msg(paste0(
      "Using suit=", suit_col,
      " | abund=", abund_col,
      " | tau=", sprintf("%.2f", tau),
      " | eff_wet=", sprintf("%.3f%%", input$p_wet),
      " | eff_dry=", sprintf("%.3f%%", input$p_dry)
    ))
  })

  output$grid_summary <- renderPrint({
    gid <- selected_grid()
    validate(need(!is.na(gid), "Click a grid cell on the map."))

    map_dt <- map_dt_rv()
    dt_ts  <- hurdle_dt()
    dt_all <- centroids()

    validate(need(!is.null(map_dt), "Run 'Update map' first."))
    validate(need(!is.null(dt_ts),  "Run 'Update map' first."))
    validate(need(!is.null(dt_all), "Centroids data not loaded."))

    # row from the mapped summary table (mean abundance)
    row <- map_dt[grid_id == gid]
    validate(need(nrow(row) == 1, "That grid isn’t in the mapped layer (maybe filtered out by τ or abundance>0)."))

    # time-series slice
    ts <- dt_ts[grid_id == gid][order(date)]
    validate(need(nrow(ts) > 0, "No time-series data for that grid cell."))

    # covariates to summarise (means at this location)
    covs <- c("elev","water_occ","water_occ_99","mang_rf_5km","ppa21","tmaxm21","tminm21","rhm21")
    missing <- setdiff(covs, names(dt_all))
    validate(need(length(missing) == 0, paste("Missing covariate columns:", paste(missing, collapse = ", "))))

    # make sure date is Date (safe)
    if ("date" %in% names(dt_all) && !inherits(dt_all$date, "Date")) {
      if (is.numeric(dt_all$date)) dt_all[, date := as.Date(date, origin = "1970-01-01")]
      else dt_all[, date := as.Date(date)]
    }

    # same window as mapping
    dt_g <- dt_all[
      grid_id == gid &
       date >= as.Date("1995-01-01") & date <= as.Date("1997-12-31"),
      ..covs
    ]

    # mean covariates (handle NAs)
    cov_means <- dt_g[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), .SDcols = covs]

    # ---- print (ONLY requested fields) ----
    cat("lon/lat:", sprintf("%.4f, %.4f", row$lon[1], row$lat[1]), "\n")
    cat("mean predicted abundance (mapped):", sprintf("%.1f", row$mean_abund[1]), "\n\n")

    cat("Mean covariates (1995–1997):\n")
    cat("  elev:",       sprintf("%.2f", cov_means$elev), "\n")
    cat("  water_occ:",  sprintf("%.2f", cov_means$water_occ), "\n")
    cat("  water_occ_99:",sprintf("%.2f", cov_means$water_occ_99), "\n")
    cat("  mang_rf_5km:",sprintf("%.2f", cov_means$mang_rf_5km), "\n")
    cat("  ppa21:",      sprintf("%.3f", cov_means$ppa21), "\n")
    cat("  tmaxm21:",    sprintf("%.2f", cov_means$tmaxm21), "\n")
    cat("  tminm21:",    sprintf("%.2f", cov_means$tminm21), "\n")
    cat("  rhm21:",      sprintf("%.2f", cov_means$rhm21), "\n\n")

    cat("abundance (ts) mean:", sprintf("%.1f", mean(ts$abundance, na.rm = TRUE)), "\n")
    cat("abundance (ts) max :", sprintf("%.1f", max(ts$abundance, na.rm = TRUE)), "\n")
    
  })


  
  # ---- Time series with NB predictive interval bands ----
  output$ts_plot <- renderPlot({
    dt_ts <- hurdle_dt()
    req(dt_ts)

    gid <- selected_grid()
    validate(need(!is.na(gid), "Click a grid cell on the map to show its time series."))

    ts <- dt_ts[grid_id == gid][order(date)]
    validate(need(nrow(ts) > 0, "No data for that grid cell."))

    conf <- as.numeric(input$conf)
    th   <- as.numeric(input$theta)

    # if theta isn't available, plot line only
    if (!is.finite(th) || th <= 0) {
      return(
        ggplot(ts, aes(date, abundance)) +
          geom_line(linewidth = 0.35) +
          labs(x = NULL, y = "Abundance (trap-efficiency adjusted)", title = paste0("Grid ", gid)) +
          theme_bw(base_size = 11)
      )
    }

    alpha <- 1 - conf
    mu <- pmax(ts$abundance, 1e-8)

    ts[, lower := qnbinom(alpha/2, size = th, mu = mu)]
    ts[, upper := qnbinom(1 - alpha/2, size = th, mu = mu)]

    ggplot(ts, aes(date, abundance)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
      geom_line(linewidth = 0.35) +
      labs(
        #title = paste0("Grid ", gid, " (", round(conf*100), "% PI)")
        x = NULL,
        y = "Abundance (trap-efficiency adjusted)"       
      ) +
      theme_bw(base_size = 11) 
  })
}

shinyApp(ui, server)
