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
  "albedo"     = read_csv("./data/processed_alb.csv", show_col_types = FALSE),
  "ROF"        = read_csv("./data/processed_Rof.csv", show_col_types = FALSE),
  "snow depth" = read_csv("./data/processed_snd.csv", show_col_types = FALSE),
  "SWE"        = read_csv("./data/processed_SWE.csv", show_col_types = FALSE),
  "T_surf"     = read_csv("./data/processed_Tsf.csv", show_col_types = FALSE)
)

variable_units <- list(
  "albedo" = "[-]",
  "ROF" = "[mm/day]",
  "snow depth" = "[cm]",
  "SWE" = "[mm]",
  "T_surf" = "[°C]"
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
zonal_variables <- c("gst", "rof", "swe", "snd", "alb")

# ============================================================
# UI
# ============================================================
ui <- navbarPage(
  title = "21st Century Snow Climate Scenarios for Central Asia",
  id = "main_nav",
  theme = NULL,

  # --- Logos fixed bottom-right ---
  tags$div(
    style = "position: fixed; bottom: 10px; right: 10px; z-index: 1000; display: flex; gap: 15px; align-items: center;",
    tags$a(href = "https://www.unesco.org", target = "_blank",
           tags$img(src = "unesco.png", height = "30px")),
    tags$a(href = "https://mountainfutures.ch/", target = "_blank",
           tags$img(src = "mf.png", height = "60px")),
    tags$a(href = "https://www.thegef.org/", target = "_blank",
           tags$img(src = "gef.png", height = "70px"))
  ),

  # ========== Tab 1: Seasonal ==========
  tabPanel(
    "Seasonal",
    icon = icon("chart-line"),
    sidebarLayout(
      sidebarPanel(
        selectInput("s_variable", "Variable",
                    choices = names(seasonal_data_list),
                    selected = "SWE"),
        selectInput("s_region", "Region", choices = NULL),
        selectInput("s_scenario", "Scenario", choices = NULL),
        wellPanel(
          tags$p(tags$b("Information:")),
          tags$p("Seasonality of key snow variables for the hydrological year
                  (September 1 – August 31). Solid lines show the historical
                  period (1981–2010); dashed lines show the far-future period
                  (2081–2100). Colours represent elevation bands. Results are
                  based on the TopoCLIM model chain (Fiddes et al., 2022) with
                  bias-corrected CMIP6 projections and the FSM snow model.")
        )
      ),
      mainPanel(
        plotOutput("seasonalPlot", height = "600px")
      )
    )
  ),

  # ========== Tab 2: Catchments ==========
  tabPanel(
    "Catchments",
    icon = icon("map"),
    sidebarLayout(
      sidebarPanel(
        selectInput("z_variable", "Variable:",
                    choices = zonal_variables, selected = "swe"),
        selectInput("z_scenario", "Scenario:",
                    choices = scenarios, selected = "ssp126"),
        wellPanel(
          tags$p(tags$b("Basin Average Anomaly:")),
          tags$p("Anomaly maps aggregated by catchment for the far-future
                  period (2081–2100) relative to the historical baseline
                  (1981–2010). Variables shown: ground surface temperature (GST),
                  snowmelt runoff (ROF), snow water equivalent (SWE), snow
                  depth (SND), and albedo (ALB).")
        )
      ),
      mainPanel(
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
  # Tab 1: Seasonal
  # ----------------------------------------------------------
  df_long <- reactive({
    req(input$s_variable)
    seasonal_data_list[[input$s_variable]] %>%
      pivot_longer(cols = c("historical_mean", "future_mean"),
                   names_to = "period", values_to = "value") %>%
      mutate(elev_band = paste0(elev_low, "-", elev_high))
  })

  observe({
    req(df_long())
    updateSelectInput(session, "s_region",
                      choices = unique(df_long()$region),
                      selected = unique(df_long()$region)[1])
  })

  observe({
    req(df_long(), input$s_region)
    sc <- df_long() %>%
      filter(region == input$s_region) %>%
      pull(scenario) %>%
      unique()
    updateSelectInput(session, "s_scenario",
                      choices = sc, selected = sc[1])
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

    ggplot(df_filtered, aes(x = hydro_date, y = value,
                            color = elev_band, linetype = period,
                            group = interaction(elev_band, period))) +
      geom_line(size = 1) +
      scale_color_viridis_d(name = "Elevation Band") +
      scale_linetype_manual(
        values = c("historical_mean" = "solid", "future_mean" = "twodash"),
        guide = guide_legend(override.aes = list(size = 1.5))
      ) +
      scale_x_date(date_labels = "%m-%d", date_breaks = "1 month",
                   expand = expansion(add = c(0, 0))) +
      labs(
        title = paste("Variable:", input$s_variable,
                      "| Region:", input$s_region,
                      "| Scenario:", input$s_scenario),
        x = "Hydrological Day",
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
    file_path <- file.path("data",
                           paste0(input$z_variable, "_", input$z_scenario,
                                  "_zonal_stats.geojson"))
    if (!file.exists(file_path)) {
      showNotification(paste("File not found:", file_path), type = "error")
      return(NULL)
    }
    data <- st_read(file_path, quiet = TRUE)
    if (any(!st_is_valid(data))) {
      data <- st_make_valid(data)
    }
    data
  })

  output$mapPlot <- renderLeaflet({
    basins <- basins_data()
    req(basins)

    pal <- zonal_palettes[[input$z_variable]]

    leaflet(basins) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(mean),
        color = "black", weight = 1, opacity = 1, fillOpacity = 0.7,
        label = ~paste0("Mean anomaly: ", round(mean, 2), " ",
                        zonal_units[[input$z_variable]]),
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = zonal_palette_info[[input$z_variable]]$domain,
        title = paste0(toupper(input$z_variable), " Anomaly (",
                       zonal_units[[input$z_variable]], ")"),
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)
