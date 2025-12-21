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

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(
    tags$title("🦟 Mozzie Dashboard"),
    tags$meta(charset = "utf-8"),
    tags$style(HTML("
      body { background: #f5f7fb; }

      /* keep content from stretching too wide */
      .container-app{
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 16px;
      }

      .app-header {
        background: white;
        border-radius: 14px;
        padding: 14px 16px;
        margin: 14px 0 12px 0;
        box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
        border: 1px solid rgba(148,163,184,0.20);
      }
      .app-title h3 { margin: 0; font-weight: 700; }
      .app-title h4 { margin: 2px 0 0 0; color: #475569; font-size: 15px; }
      .app-title h5 { margin: 0; color: #64748b; font-size: 13px; }
      .app-sub { margin-top: 6px; color: #64748b; font-size: 12px; }
      .app-badges code { background: #f1f5f9; padding: 2px 6px; border-radius: 6px; }

      /* cleaner navbar */
      .navbar { border-radius: 14px; margin-bottom: 12px; }
      .navbar-default { background: #0f172a; border-color: transparent; }
      .navbar-default .navbar-nav > li > a {
        color: rgba(255,255,255,0.86);
        padding: 12px 14px;
        font-weight: 600;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover {
        background: rgba(255,255,255,0.14);
        color: #fff;
        border-radius: 10px;
        margin: 6px 4px;
      }
      .navbar-brand { display:none !important; } /* remove left 'brand' space */

      .card {
        background: white;
        border-radius: 14px;
        padding: 14px;
        margin-bottom: 12px;
        box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
        border: 1px solid rgba(148,163,184,0.20);
      }
      .card-title {
        font-weight: 700;
        font-size: 13px;
        color: #0f172a;
        margin: 0 0 10px 0;
      }

      .form-group { margin-bottom: 10px; }
      .control-label { font-weight: 700; color: #0f172a; font-size: 13px; }
      .btn-primary { border-radius: 10px; font-weight: 700; padding: 10px 12px; }
      .btn-default { border-radius: 10px; }

      #hurdle_status, #status_box {
        background: #f8fafc;
        color: #0f172a;
        border-radius: 12px;
        padding: 10px;
        border: 1px solid rgba(148,163,184,0.25);
      }
      pre { white-space: pre-wrap; font-size: 12px; }

      /* DT cosmetics */
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border-radius: 10px !important;
        border: 1px solid rgba(148,163,184,0.35) !important;
        padding: 4px 8px !important;
      }
      .dataTables_wrapper .dt-buttons .btn {
        border-radius: 10px !important;
      }
    "))
  ),

  div(class = "container-app",

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
        )
      ),

      navbarPage(
        title = NULL,   # <-- removes title in-page; browser tab title is set by tags$title()
        tabPanel(
          "Suitability",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              div(class = "card",
                  div(class="card-title", "Model settings"),
                  selectInput("dist_model", "Distribution model",
                              choices = c("BRT"="brt","RF"="rf","MAX"="max","GLM"="glm","GAM"="gam","Ensemble"="ens"),
                              selected = "brt"),
                  numericInput("tau", label = HTML("Threshold &tau;"),
                               value = 0.25, min = 0, max = 1, step = 0.01),
                  actionButton("update_suit", "Update map", class = "btn btn-primary")
              ),
              div(class = "card",
                  div(class="card-title", "Status"),
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
                  div(class="card-title", "Model settings"),
                  selectInput("dist_model_h", "Distribution model",
                              choices = c("BRT"="brt","RF"="rf","MAX"="max","GLM"="glm","GAM"="gam","Ensemble"="ens"),
                              selected = "brt"),
                  selectInput("abund_model_h", "Abundance model (conditional on presence)",
                              choices = c("RF"="rf","BRT"="brt","GLM"="glm","GAM"="gam","ENS"="ens"),
                              selected = "rf"),
                  numericInput("tau_h", label = HTML("Threshold &tau;"),
                               value = 0.25, min = 0, max = 1, step = 0.01),
                  selectInput("conf", "Predictive interval",
                              choices = c("90%"=0.90, "95%"=0.95, "99%"=0.99),
                              selected = 0.95),
                  numericInput("theta", label = HTML("&theta; (NB dispersion)"),
                               value = NA_real_, min = 0.001, step = 0.1),
                  actionButton("update_hurdle", "Update map", class = "btn btn-primary")
              ),

              div(class="card",
                  div(class="card-title", "Trap efficiency"),
                  tags$small("Defaults are the mean trap efficiencies from Chow et al. (used in Fig. 4). Enter as percent (%)."),
                  br(),
                  numericInput("p_wet", "Wet season (%)", value = 1.23, min = 0.0001, step = 0.01),
                  numericInput("p_dry", "Dry season (%)", value = 1.66, min = 0.0001, step = 0.01),
                  tags$small(tags$em("Conversion used: abundance = trapcounts / (p/100)")),
                  br(),
                  actionButton("reset_eff", "Reset to defaults", class = "btn btn-default btn-sm")
              ),

              div(class="card",
                  div(class="card-title", "Clicked grid summary"),
                  verbatimTextOutput("grid_summary", placeholder = TRUE)
              ),

              div(class="card",
                  div(class="card-title", "Status / messages"),
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
            div(class="card",
                div(class="card-title", "Loaded data preview"),
                DTOutput("head_dt")
            )
          )
        ),

        tabPanel(
          "About",
          fluidPage(
            div(class = "card",
                h3("About this app"),
                p(
                  "This repo contains R scripts to reproduce the results in ",
                  tags$em("Anopheles farauti"),
                  " abundance in Queensland: a hurdle modelling approach."
                ),
                tags$img(
                  src = "anopheles_farauti_dontbugme.png",
                  style = "width: 100%; max-width: 520px; display: block; margin: 12px auto;
                           border-radius: 14px; border: 1px solid rgba(148,163,184,0.35);
                           box-shadow: 0 8px 24px rgba(15, 23, 42, 0.08);"
                )
            )
          )
        )
      )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  centroids <- reactiveVal(NULL)

  # defaults with full precision for the model, but UI shows 2dp
  eff_defaults <- list(p_wet = 1.231544, p_dry = 1.660359)
  observeEvent(input$reset_eff, {
    updateNumericInput(session, "p_wet", value = round(eff_defaults$p_wet, 2))
    updateNumericInput(session, "p_dry", value = round(eff_defaults$p_dry, 2))
  })

  hurdle_dt <- reactiveVal(NULL)
  map_dt_rv <- reactiveVal(NULL)
  selected_grid <- reactiveVal(NA_integer_)

  observeEvent(input$hurdle_map_marker_click, {
    click <- input$hurdle_map_marker_click
    if (!is.null(click$id)) selected_grid(as.integer(click$id))
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
    if (is.finite(input$theta)) return()
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
      if ("date" %in% names(dt)) cat("date class:", paste(class(dt$date), collapse = ", "), "\n")
    }
  })

  # ---- Data tab table (NO grid_id column) ----
  output$head_dt <- renderDT({
    dt <- centroids()
    req(dt)

    wanted <- c("elev","water_occ","water_occ_99","mang_rf_5km","ppa21","tmaxm21","tminm21","rhm21")
    out <- dt[, c(
      .(lon = lon[1], lat = lat[1]),
      lapply(.SD, function(x) mean(x, na.rm = TRUE))
    ), by = grid_id, .SDcols = wanted]

    out <- out[order(grid_id)]
    out[, grid_id := NULL]  # <-- remove it from display

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
      )
    ) |>
      DT::formatRound(columns = c("lon","lat"), digits = 2) |>
      DT::formatRound(columns = wanted, digits = 2)
  })

  # ---- Suitability map ----
  output$suit_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light (Positron)") %>%
      addProviderTiles(providers$CartoDB.Voyager,  group = "Voyager (more labels)") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo (terrain)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (imagery)") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street (Esri)") %>%
      addLayersControl(
        baseGroups = c("Light (Positron)", "Voyager (more labels)", "Street (Esri)", "Topo (terrain)", "Satellite (imagery)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Street (Esri)", "Topo (terrain)", "Satellite (imagery)")) %>%
      setView(lng = 145.5, lat = -18.0, zoom = 5.5)
  })

  observeEvent(input$update_suit, {
    dt <- centroids()
    req(dt)

    suit_col <- guess_suit_col(dt, input$dist_model)
    validate(need(!is.na(suit_col), "Could not find a suitability column for this model in the centroids file."))
    validate(need(all(c("lon", "lat") %in% names(dt)), "Expected columns named 'lon' and 'lat'."))

    tau <- input$tau
    map_dt <- dt[, .(grid_id, lon, lat, suit = get(suit_col))] |> unique(by = c("grid_id", "lon", "lat"))
    map_dt <- map_dt[is.finite(lon) & is.finite(lat) & is.finite(suit)]
    map_dt[, present := suit >= tau]

    pal <- colorNumeric(palette = heat_diverging, domain = map_dt$suit)

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
      addProviderTiles(providers$CartoDB.Positron, group = "Light (Positron)") %>%
      addProviderTiles(providers$CartoDB.Voyager,  group = "Voyager (more labels)") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street (Esri)") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo (terrain)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (imagery)") %>%
      addLayersControl(
        baseGroups = c("Light (Positron)", "Voyager (more labels)", "Street (Esri)", "Topo (terrain)", "Satellite (imagery)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Street (Esri)", "Topo (terrain)", "Satellite (imagery)")) %>%
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
      validate(need("date" %in% names(dt) && !all(is.na(dt$date)), "ERROR: season missing and date invalid."))
      m <- as.integer(format(dt$date, "%m"))
      dt[, season := ifelse(m %in% 5:10, "D", "W")]
    } else {
      dt[, season := as.character(season)]
      dt[season %in% c("dry","Dry","D"), season := "D"]
      dt[season %in% c("wet","Wet","W"), season := "W"]
    }

    suit_col <- guess_suit_col(dt, input$dist_model_h)
    validate(need(!is.na(suit_col), "ERROR: Could not find suitability column for selected model."))

    tau <- input$tau_h
    validate(need(is.finite(tau) && tau >= 0 && tau <= 1, "ERROR: tau must be between 0 and 1."))

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

    validate(need(is.finite(input$p_wet) && input$p_wet > 0, "Wet season trap efficiency must be > 0%."))
    validate(need(is.finite(input$p_dry) && input$p_dry > 0, "Dry season trap efficiency must be > 0%."))
    eff_wet <- input$p_wet / 100
    eff_dry <- input$p_dry / 100

    dt[, trapcounts := get(abund_col)]
    dt[, abundance := ifelse(season == "D", trapcounts / eff_dry, trapcounts / eff_wet)]

    # Fig 4 time window
    dt <- dt[date >= as.Date("1995-01-01") & date <= as.Date("1997-12-31")]

    hurdle_dt(dt[, .(grid_id, lon, lat, date, abundance)])
    selected_grid(NA_integer_)

    dt <- dt[get(suit_col) >= tau]

    map_dt <- dt[
      is.finite(abundance) & abundance > 0,
      .(lon = lon[1], lat = lat[1], mean_abund = mean(abundance, na.rm = TRUE)),
      by = grid_id
    ]
    map_dt_rv(map_dt)

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

    hurdle_msg(paste0(
      "Done. Mapped ", nrow(map_dt), " grid cells. Click a cell for the time series.\n",
      "eff_wet=", sprintf("%.2f%%", input$p_wet), " | eff_dry=", sprintf("%.2f%%", input$p_dry)
    ))
  })

  # ---- Grid summary (your compact version) ----
  output$grid_summary <- renderPrint({
    gid <- selected_grid()
    validate(need(!is.na(gid), "Click a grid cell on the map."))

    map_dt <- map_dt_rv()
    dt_ts  <- hurdle_dt()
    dt_all <- centroids()

    validate(need(!is.null(map_dt), "Run 'Update map' first."))
    validate(need(!is.null(dt_ts),  "Run 'Update map' first."))
    validate(need(!is.null(dt_all), "Centroids data not loaded."))

    row <- map_dt[grid_id == gid]
    validate(need(nrow(row) == 1, "That grid isn’t in the mapped layer (maybe filtered out)."))

    ts <- dt_ts[grid_id == gid][order(date)]
    validate(need(nrow(ts) > 0, "No time-series data for that grid cell."))

    covs <- c("elev","water_occ","water_occ_99","mang_rf_5km","ppa21","tmaxm21","tminm21","rhm21")

    if ("date" %in% names(dt_all) && !inherits(dt_all$date, "Date")) {
      if (is.numeric(dt_all$date)) dt_all[, date := as.Date(date, origin = "1970-01-01")]
      else dt_all[, date := as.Date(date)]
    }

    dt_g <- dt_all[
      grid_id == gid &
        date >= as.Date("1995-01-01") & date <= as.Date("1997-12-31"),
      ..covs
    ]
    cov_means <- dt_g[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), .SDcols = covs]

    cat("lon/lat:", sprintf("%.4f, %.4f", row$lon[1], row$lat[1]), "\n")
    cat("mean predicted abundance (mapped):", sprintf("%.1f", row$mean_abund[1]), "\n\n")

    cat("Mean covariates (1995–1997):\n")
    cat("  elev:",        sprintf("%.2f", cov_means$elev), "\n")
    cat("  water_occ:",   sprintf("%.2f", cov_means$water_occ), "\n")
    cat("  water_occ_99:",sprintf("%.2f", cov_means$water_occ_99), "\n")
    cat("  mang_rf_5km:", sprintf("%.2f", cov_means$mang_rf_5km), "\n")
    cat("  ppa21:",       sprintf("%.3f", cov_means$ppa21), "\n")
    cat("  tmaxm21:",     sprintf("%.2f", cov_means$tmaxm21), "\n")
    cat("  tminm21:",     sprintf("%.2f", cov_means$tminm21), "\n")
    cat("  rhm21:",       sprintf("%.2f", cov_means$rhm21), "\n\n")

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

    if (!is.finite(th) || th <= 0) {
      return(
        ggplot(ts, aes(date, abundance)) +
          geom_line(linewidth = 0.35) +
          labs(x = NULL, y = "Abundance (trap-efficiency adjusted)") +
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
      labs(x = NULL, y = "Abundance (trap-efficiency adjusted)") +
      theme_bw(base_size = 11)
  })
}

shinyApp(ui, server)
