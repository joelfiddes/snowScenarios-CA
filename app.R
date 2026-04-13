library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(viridisLite)
library(sf)
library(leaflet)

# ============================================================
# Shared config
# ============================================================
scenarios <- c("ssp126", "ssp245", "ssp585")

# ============================================================
# Tab 1: Seasonal — data & config
# ============================================================
seasonal_data_list <- list(
  "Snow Water Equivalent (SWE)" = read_csv("./data/processed_SWE.csv", show_col_types = FALSE),
  "Snow Depth"                  = read_csv("./data/processed_snd.csv", show_col_types = FALSE),
  "Snowmelt Runoff"             = read_csv("./data/processed_Rof.csv", show_col_types = FALSE),
  "Surface Albedo"              = read_csv("./data/processed_alb.csv", show_col_types = FALSE),
  "Surface Temperature"         = read_csv("./data/processed_Tsf.csv", show_col_types = FALSE)
)

variable_units <- list(
  "Snow Water Equivalent (SWE)" = "[kg m\u207b\u00b2]",
  "Snow Depth" = "[m]",
  "Snowmelt Runoff" = "[kg m\u207b\u00b2]",
  "Surface Albedo" = "[-]",
  "Surface Temperature" = "[\u00b0C]"
)

# ============================================================
# Tab 2: Catchments — config
# ============================================================
zonal_palettes <- list(
  swe = colorNumeric(palette = c("#313695", "white", "#a50026"), domain = c(-150, 150), na.color = "transparent"),
  snd = colorNumeric(palette = c("#2c7bb6", "white", "#d7191c"), domain = c(-0.5, 0.5), na.color = "transparent"),
  rof = colorNumeric(palette = c("#8c510a", "white", "#01665e"), domain = c(-1, 1), na.color = "transparent"),
  gst = colorNumeric(palette = "plasma", domain = c(0, 8), na.color = "transparent"),
  alb = colorNumeric(palette = c("#762a83", "white", "#1b7837"), domain = c(-0.1, 0.1), na.color = "transparent")
)

zonal_palette_info <- list(
  swe = list(colors = c("#313695", "white", "#a50026"), domain = c(-150, 150)),
  snd = list(colors = c("#2c7bb6", "white", "#d7191c"), domain = c(-0.5, 0.5)),
  rof = list(colors = c("#8c510a", "white", "#01665e"), domain = c(-1, 1)),
  gst = list(colors = "plasma", domain = c(0, 8)),
  alb = list(colors = c("#762a83", "white", "#1b7837"), domain = c(-0.1, 0.1))
)

zonal_units <- c(swe = "mm", snd = "m", rof = "mm", gst = "°C", alb = "unitless")
zonal_variables <- c(
  "Snow Water Equivalent" = "swe",
  "Snow Depth" = "snd",
  "Snowmelt Runoff" = "rof",
  "Ground Surface Temperature" = "gst",
  "Surface Albedo" = "alb"
)

# Pre-load simplified geojson at startup for instant switching
message("Pre-loading catchment data...")
zonal_data_cache <- list()
for (v in zonal_variables) {
  for (s in scenarios) {
    key <- paste0(v, "_", s)
    fp <- file.path("data", "simplified", paste0(key, "_zonal_stats.geojson"))
    if (file.exists(fp)) {
      d <- st_read(fp, quiet = TRUE)
      if (any(!st_is_valid(d))) d <- st_make_valid(d)
      zonal_data_cache[[key]] <- d
    }
  }
}
message("Done. Loaded ", length(zonal_data_cache), " datasets.")

# ============================================================
# Methodology modal content
# ============================================================
methodology_modal <- modalDialog(
  title = NULL,
  size = "l",
  easyClose = TRUE,
  footer = modalButton("Close"),
  tags$div(style = "padding: 10px;",
    # Header with gradient
    tags$div(style = "background: linear-gradient(135deg, #1a5276 0%, #2e86c1 100%);
                      color: white; padding: 25px 30px; margin: -16px -16px 20px -16px;
                      border-radius: 4px 4px 0 0;",
      tags$h2(style = "margin: 0 0 8px 0; font-weight: 700;", "Methodology"),
      tags$p(style = "margin: 0; opacity: 0.9; font-size: 15px;",
             "High-Resolution Climate Change Impact Scenarios for Central Asian Snow Cover")
    ),

    tags$p(style = "font-size: 15px; line-height: 1.7; color: #333;",
           "This tool presents 21st-century snow climate scenarios for Central Asia,
            generated using the TopoCLIM model chain (Fiddes et al., 2022). The integrated
            approach combines climate downscaling, subgrid spatial modelling, and a
            physically-based snow model to produce high-resolution projections that capture
            the complex interactions between terrain and climate in mountainous regions."),

    # Steps as styled cards
    tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px; margin: 20px 0;",

      # Step 1
      tags$div(style = "background: #f0f7fb; border-left: 4px solid #2e86c1;
                        padding: 16px 18px; border-radius: 0 8px 8px 0;",
        tags$div(style = "color: #2e86c1; font-weight: 700; font-size: 13px;
                          text-transform: uppercase; letter-spacing: 1px; margin-bottom: 6px;",
                 "Step 1"),
        tags$h4(style = "margin: 0 0 8px 0; color: #1a5276;",
                "TopoSUB — Topographical Clustering"),
        tags$p(style = "margin: 0; font-size: 14px; line-height: 1.6; color: #444;",
               "The landscape is clustered into representative topographic units based on
                elevation, slope, aspect, and sky view factor (Fiddes & Gruber, 2012).
                This reduces computational load while retaining critical terrain variability
                across the Pamirs, Tien Shan, and Altai.")
      ),

      # Step 2
      tags$div(style = "background: #f0f7fb; border-left: 4px solid #2e86c1;
                        padding: 16px 18px; border-radius: 0 8px 8px 0;",
        tags$div(style = "color: #2e86c1; font-weight: 700; font-size: 13px;
                          text-transform: uppercase; letter-spacing: 1px; margin-bottom: 6px;",
                 "Step 2"),
        tags$h4(style = "margin: 0 0 8px 0; color: #1a5276;",
                "TopoSCALE — Downscaling Current Climate"),
        tags$p(style = "margin: 0; font-size: 14px; line-height: 1.6; color: #444;",
               "ERA5 reanalysis data is downscaled to the topographic clusters, accounting
                for altitude, exposure, and terrain-induced variations (Fiddes & Gruber, 2014).
                This provides the high-resolution baseline of current climate conditions.")
      ),

      # Step 3
      tags$div(style = "background: #fef5e7; border-left: 4px solid #e67e22;
                        padding: 16px 18px; border-radius: 0 8px 8px 0;",
        tags$div(style = "color: #e67e22; font-weight: 700; font-size: 13px;
                          text-transform: uppercase; letter-spacing: 1px; margin-bottom: 6px;",
                 "Step 3"),
        tags$h4(style = "margin: 0 0 8px 0; color: #784212;",
                "TopoCLIM — Bias Correction of Future Scenarios"),
        tags$p(style = "margin: 0; font-size: 14px; line-height: 1.6; color: #444;",
               "CMIP6 data is downscaled and bias-corrected using quantile mapping
                (Fiddes et al., 2022). Four GCMs (UKESM1.0-LL, IPSL-CM6A-LR, MRI-ESM2-0,
                GFDL-ESM4) span the uncertainty range. Scenarios: SSP1-2.6, SSP2-4.5,
                SSP3-7.0, and SSP5-8.5.")
      ),

      # Step 4
      tags$div(style = "background: #eafaf1; border-left: 4px solid #27ae60;
                        padding: 16px 18px; border-radius: 0 8px 8px 0;",
        tags$div(style = "color: #27ae60; font-weight: 700; font-size: 13px;
                          text-transform: uppercase; letter-spacing: 1px; margin-bottom: 6px;",
                 "Step 4"),
        tags$h4(style = "margin: 0 0 8px 0; color: #1e8449;",
                "FSM Snow Modelling"),
        tags$p(style = "margin: 0; font-size: 14px; line-height: 1.6; color: #444;",
               "The FSM snow model (Essery, 2015) simulates SWE, snow depth, runoff,
                surface temperature, and albedo across three periods: historical (1981-2010),
                near-future (2021-2040), and far-future (2081-2100).")
      )
    ),

    # Variables table
    tags$h4(style = "color: #1a5276; margin-top: 20px;", "Key Variables"),
    tags$table(style = "width: 100%; border-collapse: collapse; font-size: 14px;",
      tags$thead(style = "background: #2e86c1; color: white;",
        tags$tr(
          tags$th(style = "padding: 10px 14px; text-align: left;", "Variable"),
          tags$th(style = "padding: 10px 14px; text-align: left;", "Units"),
          tags$th(style = "padding: 10px 14px; text-align: left;", "Description")
        )
      ),
      tags$tbody(
        tags$tr(style = "background: #f8f9fa;",
          tags$td(style = "padding: 8px 14px; font-weight: 600;", "SWE"),
          tags$td(style = "padding: 8px 14px;", "kg m\u207b\u00b2"),
          tags$td(style = "padding: 8px 14px;", "Average snow water equivalent")),
        tags$tr(
          tags$td(style = "padding: 8px 14px; font-weight: 600;", "snd"),
          tags$td(style = "padding: 8px 14px;", "m"),
          tags$td(style = "padding: 8px 14px;", "Average snow depth")),
        tags$tr(style = "background: #f8f9fa;",
          tags$td(style = "padding: 8px 14px; font-weight: 600;", "Rof"),
          tags$td(style = "padding: 8px 14px;", "kg m\u207b\u00b2"),
          tags$td(style = "padding: 8px 14px;", "Cumulated runoff from snow (incl. rain)")),
        tags$tr(
          tags$td(style = "padding: 8px 14px; font-weight: 600;", "GST"),
          tags$td(style = "padding: 8px 14px;", "\u00b0C"),
          tags$td(style = "padding: 8px 14px;", "Average ground surface temperature")),
        tags$tr(style = "background: #f8f9fa;",
          tags$td(style = "padding: 8px 14px; font-weight: 600;", "alb"),
          tags$td(style = "padding: 8px 14px;", "\u2014"),
          tags$td(style = "padding: 8px 14px;", "Effective albedo"))
      )
    ),

    # References
    tags$h4(style = "color: #1a5276; margin-top: 20px;", "References"),
    tags$div(style = "font-size: 13px; line-height: 1.7; color: #555;",
      tags$p(style = "margin: 4px 0;", "Essery, R. (2015). A factorial snowpack model (FSM 1.0). ",
             tags$em("Geosci. Model Dev."), ", 8(12), 3867\u20133876."),
      tags$p(style = "margin: 4px 0;", "Fiddes, J. & Gruber, S. (2012). TopoSUB: efficient large area numerical modelling in complex topography. ",
             tags$em("Geosci. Model Dev."), ", 5, 1245\u20131257."),
      tags$p(style = "margin: 4px 0;", "Fiddes, J. & Gruber, S. (2014). TopoSCALE v.1.0: downscaling gridded climate data in complex terrain. ",
             tags$em("Geosci. Model Dev."), ", 7, 387\u2013405."),
      tags$p(style = "margin: 4px 0;", "Fiddes, J. et al. (2022). TopoCLIM: rapid topography-based downscaling of regional climate model output. ",
             tags$em("Geosci. Model Dev."), ", 15, 1753\u20131768.")
    )
  )
)

# ============================================================
# UI
# ============================================================
ui <- navbarPage(
  title = "21st Century Snow Climate Scenarios for Central Asia",
  id = "main_nav",
  theme = NULL,

  # --- Info button top-right of navbar ---
  tags$script(HTML("
    $(document).ready(function() {
      $('.navbar-right, .navbar-nav:last').after(
        '<ul class=\"nav navbar-nav navbar-right\"><li><a id=\"info_btn\" href=\"#\" style=\"font-size:20px; padding:15px 20px; cursor:pointer; line-height:20px;\"><i class=\"fa fa-info-circle\"></i></a></li></ul>'
      );
      $(document).on('click', '#info_btn', function(e) {
        e.preventDefault();
        Shiny.setInputValue('show_methodology', Math.random());
      });
    });
  ")),

  # Logos are placed inside each tab's sidebarPanel instead

  # ========== Tab 1: Seasonal ==========
  tabPanel(
    "Seasonal",
    icon = icon("chart-line"),
    fluidRow(
      column(4,
        wellPanel(
          selectInput("s_variable", "Variable",
                      choices = names(seasonal_data_list),
                      selected = "Snow Water Equivalent (SWE)"),
          selectInput("s_region", "River Basin", choices = NULL),
          selectInput("s_scenario", "Climate Scenario", choices = NULL),
          tags$hr(),
          tags$p(tags$b("Seasonal Snow Cycle")),
          tags$p("How snow variables change through the hydrological year
                  (September 1 \u2013 August 31) across elevation zones in Central
                  Asia\u2019s major river basins."),
          tags$p(tags$b("Reading the plot:"),
            tags$ul(style = "padding-left: 18px; margin-top: 4px;",
              tags$li(tags$span(style = "font-weight: 600;", "Solid lines"), " = historical baseline (1981\u20132010)"),
              tags$li(tags$span(style = "font-weight: 600;", "Dashed lines"), " = near-future projection (2021\u20132040)"),
              tags$li(tags$span(style = "font-weight: 600;", "Dotted lines"), " = far-future projection (2081\u20132100)"),
              tags$li(tags$span(style = "font-weight: 600;", "Colours"), " = elevation bands from 0 m (purple) to 6000 m (yellow)")
            )
          ),
          tags$p(style = "font-size: 12px; color: #666; margin-top: 8px;",
            "Projections are based on the TopoCLIM model chain (Fiddes et al., 2022)
             using bias-corrected CMIP6 climate scenarios and the FSM snow model.
             Four GCMs span the range of temperature and precipitation uncertainty.
             The gap between solid, dashed, and dotted lines reveals the projected
             near-term and long-term climate impact at each elevation.")
        ),
        tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 14px; padding: 12px 5px; justify-items: center; align-items: center;",
          tags$a(href = "https://www.unesco.org", target = "_blank",
                 tags$img(src = "unesco.png", style = "max-height: 100px; max-width: 200px;")),
          tags$a(href = "https://mountainfutures.ch/", target = "_blank",
                 tags$img(src = "mf.png", style = "max-height: 100px; max-width: 200px;")),
          tags$a(href = "https://www.thegef.org/", target = "_blank",
                 tags$img(src = "gef.png", style = "max-height: 100px; max-width: 200px;")),
          tags$a(href = "https://www.unifr.ch/", target = "_blank",
                 tags$img(src = "unifr2.svg", style = "max-height: 100px; max-width: 200px;"))
        )
      ),
      column(8,
        plotOutput("seasonalPlot", height = "600px")
      )
    )
  ),

  # ========== Tab 2: Catchments ==========
  tabPanel(
    "Catchments",
    icon = icon("map"),
    fluidRow(
      column(4,
        wellPanel(
          selectInput("z_variable", "Variable:",
                      choices = zonal_variables, selected = "swe"),
          selectInput("z_scenario", "Climate Scenario:",
                      choices = c(
                        "SSP1-2.6 (Sustainability)" = "ssp126",
                        "SSP2-4.5 (Middle of the road)" = "ssp245",
                        "SSP5-8.5 (Fossil-fueled development)" = "ssp585"
                      ), selected = "ssp126"),
          sliderInput("z_opacity", "Layer Opacity:",
                      min = 0, max = 1, value = 0.7, step = 0.05),
          tags$hr(),
          tags$p(tags$b("Catchment Anomaly Map")),
          tags$p("Projected change in snow and climate variables for 295 river
                  catchments across High Mountain Central Asia. Each catchment is
                  coloured by its mean anomaly: the difference between the far-future
                  period (2081\u20132100) and the historical baseline (1981\u20132010)."),
          tags$p(tags$b("Variables:"),
            tags$ul(style = "padding-left: 18px; margin-top: 4px;",
              tags$li(tags$span(style = "font-weight: 600;", "SWE"), " \u2013 snow water equivalent (mm). ",
                      "Negative = less water stored in snowpack"),
              tags$li(tags$span(style = "font-weight: 600;", "snd"), " \u2013 snow depth (m). ",
                      "Negative = shallower snowpack"),
              tags$li(tags$span(style = "font-weight: 600;", "Rof"), " \u2013 snowmelt runoff (mm). ",
                      "Changes affect downstream water supply timing"),
              tags$li(tags$span(style = "font-weight: 600;", "GST"), " \u2013 ground surface temperature (\u00b0C). ",
                      "Always positive = warming across all catchments"),
              tags$li(tags$span(style = "font-weight: 600;", "alb"), " \u2013 surface albedo. ",
                      "Negative = darker surface due to less snow cover")
            )
          ),
          tags$p(style = "font-size: 12px; color: #666; margin-top: 8px;",
            "Hover over a catchment to see its mean anomaly value.
             Higher SSP scenarios (SSP5-8.5) represent stronger warming and
             generally show larger anomalies, particularly at lower elevations
             where snow cover is most vulnerable.")
        ),
        tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 14px; padding: 12px 5px; justify-items: center; align-items: center;",
          tags$a(href = "https://www.unesco.org", target = "_blank",
                 tags$img(src = "unesco.png", style = "max-height: 100px; max-width: 200px;")),
          tags$a(href = "https://mountainfutures.ch/", target = "_blank",
                 tags$img(src = "mf.png", style = "max-height: 100px; max-width: 200px;")),
          tags$a(href = "https://www.thegef.org/", target = "_blank",
                 tags$img(src = "gef.png", style = "max-height: 100px; max-width: 200px;")),
          tags$a(href = "https://www.unifr.ch/", target = "_blank",
                 tags$img(src = "unifr2.svg", style = "max-height: 100px; max-width: 200px;"))
        )
      ),
      column(8,
        leafletOutput("mapPlot", height = "800px")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Methodology modal
  # ----------------------------------------------------------
  observeEvent(input$show_methodology, {
    showModal(methodology_modal)
  })

  # ----------------------------------------------------------
  # Tab 1: Seasonal
  # ----------------------------------------------------------
  df_long <- reactive({
    req(input$s_variable)
    seasonal_data_list[[input$s_variable]] %>%
      pivot_longer(cols = c("historical_mean", "near_future_mean", "future_mean"),
                   names_to = "period", values_to = "value") %>%
      mutate(elev_band = paste0(elev_low, "-", elev_high))
  })

  observe({
    req(df_long())
    regions <- unique(df_long()$region)
    region_labels <- c(
      "AMU_DARYA" = "Amu Darya",
      "SYR_DARYA" = "Syr Darya",
      "CHU_TALAS" = "Chu-Talas",
      "ISSYKUL" = "Issyk-Kul",
      "MURGHAB_HARIRUD" = "Murghab-Harirud"
    )
    regions_named <- setNames(regions, ifelse(regions %in% names(region_labels), region_labels[regions], regions))
    updateSelectInput(session, "s_region",
                      choices = regions_named,
                      selected = regions[1])
  })

  observe({
    req(df_long(), input$s_region)
    sc <- df_long() %>%
      filter(region == input$s_region) %>%
      pull(scenario) %>%
      unique()
    ssp_labels <- c(
      "ssp126" = "SSP1-2.6 (Sustainability)",
      "ssp245" = "SSP2-4.5 (Middle of the road)",
      "ssp585" = "SSP5-8.5 (Fossil-fueled development)"
    )
    sc_named <- setNames(sc, ifelse(sc %in% names(ssp_labels), ssp_labels[sc], sc))
    updateSelectInput(session, "s_scenario",
                      choices = sc_named, selected = sc[1])
  })

  output$seasonalPlot <- renderPlot({
    req(df_long(), input$s_region, input$s_scenario)

    df_filtered <- df_long() %>%
      filter(region == input$s_region, scenario == input$s_scenario) %>%
      mutate(hydro_date = as.Date("2000-09-01") + (hydro_day - 1))

    if (nrow(df_filtered) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available", size = 8) +
               theme_void())
    }

    unit_label <- variable_units[[input$s_variable]]

    region_labels <- c("AMU_DARYA"="Amu Darya", "SYR_DARYA"="Syr Darya",
                        "CHU_TALAS"="Chu-Talas", "ISSYKUL"="Issyk-Kul",
                        "MURGHAB_HARIRUD"="Murghab-Harirud")
    ssp_labels <- c("ssp126"="SSP1-2.6", "ssp245"="SSP2-4.5", "ssp585"="SSP5-8.5")
    nice_region <- ifelse(input$s_region %in% names(region_labels), region_labels[input$s_region], input$s_region)
    nice_ssp <- ifelse(input$s_scenario %in% names(ssp_labels), ssp_labels[input$s_scenario], input$s_scenario)

    ggplot(df_filtered, aes(x = hydro_date, y = value,
                            color = elev_band, linetype = period,
                            group = interaction(elev_band, period))) +
      geom_line(size = 1) +
      scale_color_viridis_d(name = "Elevation Band") +
      scale_linetype_manual(
        values = c("historical_mean" = "solid", "near_future_mean" = "dashed", "future_mean" = "dotted"),
        labels = c("historical_mean" = "Historical (1981-2010)",
                    "near_future_mean" = "Near Future (2021-2040)",
                    "future_mean" = "Far Future (2081-2100)"),
        guide = guide_legend(override.aes = list(size = 1.5))
      ) +
      scale_x_date(date_labels = "%m-%d", date_breaks = "1 month",
                   expand = expansion(add = c(0, 0))) +
      labs(
        title = paste0(input$s_variable, " \u2014 ", nice_region, " \u2014 ", nice_ssp),
        x = "Day of Year",
        y = paste0(input$s_variable, " ", unit_label),
        linetype = "Period"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
      )
  })

  # ----------------------------------------------------------
  # Tab 2: Catchments
  # ----------------------------------------------------------
  basins_data <- reactive({
    key <- paste0(input$z_variable, "_", input$z_scenario)
    if (is.null(zonal_data_cache[[key]])) {
      showNotification(paste("Data not found:", key), type = "error")
      return(NULL)
    }
    zonal_data_cache[[key]]
  })

  # Render base map ONCE — tiles and controls only
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("OpenTopoMap", group = "Topographic") %>%
      fitBounds(lng1 = 61, lat1 = 33.5, lng2 = 79.5, lat2 = 43.3) %>%
      addLayersControl(
        baseGroups = c("Light", "OpenStreetMap", "Satellite", "Topographic"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft"
      )
  })

  # Track whether base map has been rendered
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$mapPlot_bounds, { map_ready(TRUE) }, once = TRUE)

  # Update polygons + legend when variable, scenario, or opacity changes
  observe({
    req(map_ready())
    basins <- basins_data()
    req(basins)

    pal <- zonal_palettes[[input$z_variable]]
    opacity <- input$z_opacity

    leafletProxy("mapPlot", data = basins) %>%
      clearGroup("polygons") %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(mean),
        color = "black", weight = 1, opacity = 1,
        fillOpacity = opacity,
        label = ~paste0("Mean anomaly: ", round(mean, 2), " ",
                        zonal_units[[input$z_variable]]),
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        ),
        group = "polygons"
      ) %>%
      addLegend(
        pal = pal,
        values = zonal_palette_info[[input$z_variable]]$domain,
        title = paste0(toupper(input$z_variable), " Anomaly (",
                       zonal_units[[input$z_variable]], ")"),
        position = "topright"
      )
  })
}

shinyApp(ui, server)
