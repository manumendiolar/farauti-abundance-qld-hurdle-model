# -----------------------------------------------------------------------------
# Farauti dashboard — BASIC runnable version
# -----------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(data.table)
library(fst)
library(leaflet)
library(ggplot2)
library(MASS)   # safe to include; helps if estimate_theta uses NB fits

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

# ---- UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(tags$style(HTML("
  #hurdle_status { height: 220px; overflow-y: scroll; }
"))),

  fluidRow(
    column(
      width = 12,
      div(
        style = "padding: 16px; background-color: #f8f8f8;",
        h3("Mapping mozzie hotspots", style = "margin-top: 0;"),
        h4(em("Anopheles Farauti"), style = "margin: 0;"),
        h5(em("Modelling abundance in Queensland"), style = "margin: 0;"),
        tags$p(tags$code(paste0("pred_dir = ", pred_dir))),
        tags$p(tags$code(paste0("centroids_file exists = ", file.exists(centroids_file)))),
        tags$img(
          src = "anopheles_farauti_dontbugme.png",
          height = "70px",
          style = "float:right; margin-top: -70px;"
        )
      )
    )
  ),

  navbarPage(
    title = "Mozzie Dashboard",

    tabPanel(
      "Suitability",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "dist_model",
            "Distribution model",
            choices = c("BRT" = "brt", "RF" = "rf", "MAX" = "max", "GLM" = "glm", "GAM" = "gam", "Ensemble" = "ens"),
            selected = "brt"
          ),
          numericInput("tau", label = HTML("Threshold &tau;"), value = 0.25, min = 0, max = 1, step = 0.01),
          actionButton("update_suit", "Update map", class = "btn btn-primary"),
          hr(),
          verbatimTextOutput("status", placeholder = TRUE)
        ),
        mainPanel(
          leafletOutput("suit_map", height = "750px")
        )
      )
    ),

    tabPanel(
      "Abundance",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          selectInput(
            "dist_model_h",
            "Distribution model",
            choices = c("BRT"="brt","RF"="rf","MAX"="max","GLM"="glm","GAM"="gam","Ensemble"="ens"),
            selected = "brt"
          ),

          selectInput(
            "abund_model_h",
            "Abundance model (conditional on presence)",
            choices = c("RF"="rf","BRT"="brt","GLM"="glm","GAM"="gam","ENS"="ens"),
            selected = "rf"
          ),

          numericInput(
            "tau_h",
            label = HTML("Threshold &tau;"),
            value = 0.25, min = 0, max = 1, step = 0.01
          ),

          # --- NEW: interval controls ---
          selectInput(
            "conf",
            "Predictive interval",
            choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
            selected = 0.95
          ),
          numericInput(
            "theta",
            label = HTML("&theta; (NB dispersion)"),
            value = NA_real_,
            min = 0.001,
            step = 0.1
          ),

          actionButton("update_hurdle", "Update map", class = "btn btn-primary"),

          hr(),
          verbatimTextOutput("hurdle_status")
        ),

        mainPanel(
          leafletOutput("hurdle_map", height = "520px"),
          br(),
          plotOutput("ts_plot", height = "260px")
        )
      )
    ),

    tabPanel(
      "Data",
      fluidPage(
        h4("Loaded data preview"),
        tableOutput("head_tbl"),
        hr(),
        h4("Columns"),
        verbatimTextOutput("cols_txt")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  centroids <- reactiveVal(NULL)

  output$cols_txt <- renderPrint({
    dt <- centroids()
    req(dt)
    cat(paste(names(dt), collapse = "\n"))
  })

  hurdle_dt <- reactiveVal(NULL)
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

  output$head_tbl <- renderTable({
    dt <- centroids()
    req(dt)

    wanted <- c(
      "elev","water_occ","water_occ_99","mang_rf_5km","ppa21","tmaxm21","tminm21","rhm21"
    )

    missing <- setdiff(wanted, names(dt))
    validate(need(length(missing) == 0,
                  paste("Missing columns in centroids data:", paste(missing, collapse = ", "))))

    out <- dt[, c(
      .(lon = lon[1], lat = lat[1]),
      lapply(.SD, function(x) mean(x, na.rm = TRUE))
    ), by = grid_id, .SDcols = wanted]

    head(out[order(grid_id)], 30)
  })

  # ---- Suitability map ----
  output$suit_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
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

    pal <- colorNumeric("viridis", domain = map_dt$suit)

    leafletProxy("suit_map", data = map_dt) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 3, stroke = FALSE,
        fillOpacity = 0.85,
        color = ~pal(suit),
        label = ~sprintf("grid: %s | suit: %.3f | present(tau=%.2f): %s", grid_id, suit, tau, present)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~suit, title = "Suitability")
  })

  # ---- Abundance map ----
  output$hurdle_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
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
    eff_wet <- 1.231544 / 100
    eff_dry <- 1.660359 / 100

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
      addLegend("bottomright", pal = pal, values = ~mean_abund, title = "Mean abundance")

    hurdle_msg(paste0("Done. Mapped ", nrow(map_dt), " grid cells. Click a cell for the time series."))
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
        x = NULL,
        y = "Abundance (trap-efficiency adjusted)",
        title = paste0("Grid ", gid, " (", round(conf*100), "% PI)")
      ) +
      theme_bw(base_size = 11)
  })
}

shinyApp(ui, server)
