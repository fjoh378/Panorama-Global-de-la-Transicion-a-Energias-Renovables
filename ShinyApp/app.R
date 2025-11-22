
library(shiny)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(bslib)
library(countrycode)
library(shinyWidgets)
library(ggplot2)
library(htmltools)
library(tidyr)
library(plotly)
library(RColorBrewer)


cols_pal <- list(
  Solar = "#FFC107", 
  Wind  = "#00BCD4", 
  Hydro = "#1565C0", 
  Bio   = "#2E7D32",
  Geo   = "#D84315"  
)

to_rgba <- function(hex, alpha = 0.6) {
  rgb_vals <- col2rgb(hex)
  paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
}


cols_def <- list(
  Solar = list(
    gen = "electricity_from_solar_t_wh__solar_energy_consumption",
    share = "solar_percent_equivalent_primary_energy__solar_share_energy",
    color = cols_pal$Solar,
    fill  = to_rgba(cols_pal$Solar),
    map_palette = "YlOrRd",
    icon = "sun",
    metric_label = "Generación solar"
  ),
  Wind = list(
    gen = "electricity_from_wind_t_wh__wind_generation",
    share = "wind_percent_equivalent_primary_energy__wind_share_energy",
    color = cols_pal$Wind,
    fill  = to_rgba(cols_pal$Wind),
    map_palette = "PuBu",
    icon = "wind",
    metric_label = "Generación eólica"
  ),
  Hydro = list(
    gen = "electricity_from_hydro_t_wh__hydropower_generation",
    share = "hydro_percent_equivalent_primary_energy__hydro_share_energy",
    color = cols_pal$Hydro,
    fill  = to_rgba(cols_pal$Hydro),
    map_palette = "GnBu",
    icon = "water",
    metric_label = "Generación hidroeléctrica"
  ),
  Bio = list(
    gen = "biofuels_production_t_wh__biofuel_production",
    share = NULL, 
    color = cols_pal$Bio,
    fill  = to_rgba(cols_pal$Bio),
    map_palette = "YlGn",
    icon = "leaf",
    metric_label = "Producción de biocombustibles"
  ),
  Geo = list(
    gen = "other_renewables_including_bioenergy_t_wh__modern_renewable_prod",
    share = NULL,
    color = cols_pal$Geo,
    fill  = to_rgba(cols_pal$Geo),
    map_palette = "OrRd",
    icon = "fire",
    metric_label = "Otros renovables modernos"
  )
)


global_share_col <- "renewables_percent_equivalent_primary_energy__renewable_share_energy"


er_data <- read.csv("cruce_er_cf.csv", na.strings = c("null", "NA", "")) 
pred_data <- read.csv("predicciones_world_fossil_renew_lstm_train_test.csv",
                      na.strings = c("null", "NA", ""))


pred_data$Real <- as.numeric(pred_data$Real)
pred_data$Pred_LSTM <- as.numeric(pred_data$Pred_LSTM)

er_clean <- er_data %>%
  mutate(
    iso3 = countrycode(entity, origin = "country.name", destination = "iso3c", warn = FALSE),
    Is_Country = !is.na(iso3)
  )


if ("World" %in% er_clean$entity) {
  er_world <- er_clean %>% filter(entity == "World")
} else {
  er_world <- er_clean %>%
    filter(Is_Country) %>%
    group_by(year) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    mutate(entity = "World")
}


cruce_er_por_anio <- er_data |>
  dplyr::filter(entity == "World") |>
  dplyr::mutate(
    total_renovables = rowSums(
      cbind(
        electricity_from_wind_t_wh__modern_renewable_prod,
        electricity_from_hydro_t_wh__modern_renewable_prod,
        electricity_from_solar_t_wh__modern_renewable_prod,
        other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ),
      na.rm = TRUE
    )
  ) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    total_renovables_twh = sum(total_renovables, na.rm = TRUE),
    .groups = "drop"
  )

primer_anio <- min(cruce_er_por_anio$year, na.rm = TRUE)

base_total_renovables <- cruce_er_por_anio |>
  dplyr::filter(year == primer_anio) |>
  dplyr::pull(total_renovables_twh)



world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::mutate(
    iso_a3 = dplyr::if_else(
      iso_a3 == "-99" & !is.na(adm0_a3),
      adm0_a3,   # usamos el código “real” del país
      iso_a3
    )
  ) %>%
  dplyr::select(iso_a3, geometry)


energy_tab_ui <- function(id, title, color, icon_name, description, has_share = TRUE) {
  ns <- NS(id)

  tagList(

    div(
      class = "card shadow-sm mb-4 border-0",
      div(
        class = "card-header bg-white py-3",
        style = paste0("border-left: 5px solid ", color, "; border-top: 0;"),
        h3(
          icon(icon_name),
          title,
          style = paste0("color:", color, "; font-weight: 800; margin: 0;")
        ),
        p(class = "text-muted mb-0 mt-2 small", description)
      )
    ),

   
    if (has_share) {
      tagList(
        # CARD 1: mapa + filtros
        div(
          class = "card shadow-sm border-0 mb-2 map-section-card",
          div(
            class = "card-header bg-white py-2",
            style = paste0("border-top: 3px solid ", color, ";"),
            h5("Participación en energía primaria (%)",
               style = "font-weight: 700; margin: 0;"),
            p(
              class = "text-muted mb-0 mt-1 small",
              "Mapa y series históricas de la cuota de esta fuente en la energía primaria o en la generación eléctrica."
            )
          ),
          div(
            class = "card-body bg-light",
            fluidRow(
              column(
                3,
                div(
                  class = "bg-white rounded shadow-sm p-3 mb-0 filters-panel",
                  h6("Filtros", class = "border-bottom pb-2 mb-3",
                     style = "font-weight: 700;"),
                  uiOutput(ns("slider_share_ui"))
                )
              ),
              column(
                9,
                div(
                  class = "bg-white rounded shadow-sm p-3 mb-0 map-panel",
                  leafletOutput(ns("map_share"), height = "420px")
                )
              )
            )
          )
        ),

     
        div(
          class = "card shadow-sm border-0 mb-4 charts-row-card",
          div(
            class = "card-body bg-light py-2",
            fluidRow(
              column(
                4,
                div(
                  class = "small-chart-card",
                  h6("Tendencia mundial (%)"),
                  plotlyOutput(ns("plot_world_share"), height = "260px")
                )
              ),
              column(
                4,
                div(
                  class = "small-chart-card",
                  h6("Top 5 (trayectoria histórica, %)"),
                  plotlyOutput(ns("plot_top5_share"), height = "260px")
                )
              ),
              column(
                4,
                div(
                  class = "small-chart-card",
                  h6(textOutput(ns("country_title_share"))),
                  tags$small(
                    "Da clic en un país del mapa para seleccionarlo.",
                    class = "text-muted d-block mb-2"
                  ),
                  plotlyOutput(ns("plot_country_share"), height = "260px")
                )
              )
            )
          )
        )
      )
    },

   
    div(
      class = "card shadow-sm border-0 mb-2 map-section-card",
      div(
        class = "card-header bg-white py-2",
        style = paste0("border-top: 3px solid ", color, ";"),
        h5("Generación / producción (TWh)",
           style = "font-weight: 700; margin: 0;"),
        p(
          class = "text-muted mb-0 mt-1 small",
          "Mapa y series históricas de energía generada o producida por esta fuente."
        )
      ),
      div(
        class = "card-body bg-light",
        fluidRow(
          column(
            3,
            div(
              class = "bg-white rounded shadow-sm p-3 mb-0 filters-panel",
              h6("Filtros", class = "border-bottom pb-2 mb-3",
                 style = "font-weight: 700;"),
              uiOutput(ns("slider_gen_ui"))
            )
          ),
          column(
            9,
            div(
              class = "bg-white rounded shadow-sm p-3 mb-0 map-panel",
              leafletOutput(ns("map_gen"), height = "420px")
            )
          )
        )
      )
    ),

  
    div(
      class = "card shadow-sm border-0 mb-4 charts-row-card",
      div(
        class = "card-body bg-light py-2",
        fluidRow(
          column(
            4,
            div(
              class = "small-chart-card",
              h6("Tendencia mundial (TWh)"),
              plotlyOutput(ns("plot_world_gen"), height = "260px")
            )
          ),
          column(
            4,
            div(
              class = "small-chart-card",
              h6("Top 5 (trayectoria histórica, TWh)"),
              plotlyOutput(ns("plot_top5_gen"), height = "260px")
            )
          ),
          column(
            4,
            div(
              class = "small-chart-card",
              h6(textOutput(ns("country_title_gen"))),
              tags$small(
                "Da clic en un país del mapa para seleccionarlo.",
                class = "text-muted d-block mb-2"
              ),
              plotlyOutput(ns("plot_country_gen"), height = "260px")
            )
          )
        )
      )
    )
  )
}




global_share_tab_ui <- function(id, color) {
  ns <- NS(id)

  div(
    class = "card shadow-sm mb-4 border-0",
    div(
      class = "card-header bg-white py-2",
      style = paste0("border-top: 3px solid ", color, ";"),
      h5("Participación de las renovables en la energía primaria (%)",
         style = "font-weight: 700; margin: 0;"),
      p(
        class = "text-muted mb-0 mt-1 small",
        "Porcentaje de la energía primaria equivalente que proviene de renovables modernas, calculado con el método de sustitución."
      )
    ),
    div(
      class = "card-body bg-light",
      
      fluidRow(
        column(
          3,
          div(
            class = "bg-white rounded shadow-sm p-3 mb-3",
            h6("Filtros", class = "border-bottom pb-2 mb-3",
               style = "font-weight: 700;"),
            uiOutput(ns("slider_ui"))
          )
        ),
        column(
          9,
          div(
            class = "bg-white rounded shadow-sm p-3 mb-3",
            leafletOutput(ns("map_share"), height = "440px")
          )
        )
      ),
   
      fluidRow(
        class = "mt-1",
        column(
          4,
          div(
            class = "bg-white rounded shadow-sm p-3 mb-3",
            h6("Tendencia mundial (%)",
               class = "small fw-bold mb-2"),
            plotlyOutput(ns("plot_world"), height = "260px")
          )
        ),
        column(
          4,
          div(
            class = "bg-white rounded shadow-sm p-3 mb-3",
            h6("Top 5 (trayectoria histórica, %)",
               class = "small fw-bold mb-2"),
            plotlyOutput(ns("plot_top5"), height = "260px")
          )
        ),
        column(
          4,
          div(
            class = "bg-white rounded shadow-sm p-3 mb-0",
            h6(
              textOutput(ns("country_title")),
              class = "small fw-bold mb-1"
            ),
            tags$small(
              "Da clic en un país del mapa para seleccionarlo.",
              class = "text-muted d-block mb-2"
            ),
            plotlyOutput(ns("plot_country"), height = "260px")
          )
        )
      )
    )
  )
}



energy_server_logic <- function(id, tech_key, data_full, world_geo, er_world) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- cols_def[[tech_key]]
    selected_iso <- reactiveVal("MEX") 
    
    # Selección de país por clic en mapa
    observeEvent(input$map_gen_shape_click, {
      if (!is.null(input$map_gen_shape_click$id)) selected_iso(input$map_gen_shape_click$id)
    })
    if (!is.null(conf$share)) {
      observeEvent(input$map_share_shape_click, {
        if (!is.null(input$map_share_shape_click$id)) selected_iso(input$map_share_shape_click$id)
      })
    }
    
    country_name <- reactive({
      req(selected_iso())
      nm <- data_full %>%
        dplyr::filter(iso3 == selected_iso()) %>%
        dplyr::pull(entity) %>%
        unique()
      if (length(nm) == 0) return(NULL)
      nm[1]
    })
    
    # Títulos dinámicos país seleccionado (share / gen)
    output$country_title_share <- renderText({
      nm <- country_name()
      if (is.null(nm)) "Evolución histórica del país seleccionado (%)"
      else paste("Evolución histórica (%):", nm)
    })
    
    output$country_title_gen <- renderText({
      nm <- country_name()
      if (is.null(nm)) "Evolución histórica del país seleccionado (TWh)"
      else paste("Evolución histórica (TWh):", nm)
    })
    
  
    if (!is.null(conf$share)) {
      output$slider_share_ui <- renderUI({
        sliderInput(
          ns("year_share_range"), "Periodo:",
          min   = min(data_full$year, na.rm = TRUE),
          max   = max(data_full$year, na.rm = TRUE),
          value = c(
            min(data_full$year, na.rm = TRUE),
            max(data_full$year, na.rm = TRUE)
          ),
          step = 1, sep = ""
        )
      })
    }
    
    output$slider_gen_ui <- renderUI({
      sliderInput(
        ns("year_gen_range"), "Periodo:",
        min   = min(data_full$year, na.rm = TRUE),
        max   = max(data_full$year, na.rm = TRUE),
        value = c(
          min(data_full$year, na.rm = TRUE),
          max(data_full$year, na.rm = TRUE)
        ),
        step = 1, sep = ""
      )
    })
    
  
    render_custom_map <- function(metric = c("gen", "share")) {
      metric <- match.arg(metric)
      renderLeaflet({
        col_name <- if (metric == "gen") conf$gen else conf$share
        req(col_name)
        
        if (metric == "gen") {
          year_range <- input$year_gen_range
        } else {
          year_range <- input$year_share_range
        }
        req(year_range)
        year_input <- year_range[2]  # usamos el año final del rango
        
        if (metric == "gen") {
          unit <- "TWh"
          bins <- c(0, 1, 10, 50, 100, 500, Inf)
        } else {
          unit <- "%"
          bins <- c(0, 1, 5, 10, 20, 50, 100)
        }
        
        map_dat <- data_full %>%
          dplyr::filter(year == year_input, Is_Country) %>%
          dplyr::select(iso3, entity, Val = dplyr::all_of(col_name))
        
        geo <- world_geo %>%
          dplyr::left_join(map_dat, by = c("iso_a3" = "iso3"))
        
        vals <- geo$Val
        pal <- colorBin(
          palette = conf$map_palette,
          domain = vals,
          bins = bins,
          na.color = "#ffffff"
        )
        
        leaflet(
          geo,
          options = leafletOptions(
            worldCopyJump = FALSE,
            minZoom = 1.5
          )
        ) %>%
          setMaxBounds(-180, -90, 180, 90) %>%
          addMapPane("fondo", zIndex = 400) %>%
          addMapPane("datos", zIndex = 450) %>%
          addMapPane("etiquetas", zIndex = 500) %>%
          addProviderTiles(
            "CartoDB.PositronNoLabels",
            options = providerTileOptions(pane = "fondo", noWrap = TRUE)
          ) %>%
          addPolygons(
            fillColor = ~pal(Val),
            weight = 1, color = "white", opacity = 1, fillOpacity = 0.85,
            layerId = ~iso_a3, options = pathOptions(pane = "datos"),
            highlightOptions = highlightOptions(
              weight = 2, color = "#444", bringToFront = TRUE
            ),
            label = sprintf(
              "<strong>%s</strong><br/>%s",
              geo$entity,
              ifelse(
                is.na(geo$Val),
                "Sin información",
                paste0(format(round(geo$Val, 2), big.mark = ","), " ", unit)
              )
            ) %>% lapply(HTML)
          ) %>%
          addProviderTiles(
            "CartoDB.PositronOnlyLabels",
            options = providerTileOptions(pane = "etiquetas", noWrap = TRUE)
          ) %>%
          setView(0, 20, 1.5) %>%
          addLegend(
            pal = pal, values = vals, title = unit,
            position = "bottomright", na.label = "Sin información"
          )
      })
    }
    
    if (!is.null(conf$share)) {
      output$map_share <- render_custom_map("share")
    }
    output$map_gen <- render_custom_map("gen")
    
   
    if (!is.null(conf$share)) {
      
     
      output$plot_world_share <- renderPlotly({
        req(input$year_share_range)
        yr <- input$year_share_range
        col_name <- conf$share
        
        w_dat <- er_world %>%
          dplyr::filter(year >= yr[1], year <= yr[2]) %>%
          dplyr::select(year, Val = dplyr::all_of(col_name)) %>%
          dplyr::arrange(year)
        
        plot_ly(
          w_dat,
          x = ~year,
          y = ~Val,
          type = "scatter",
          mode = "lines+markers",
          name = "Mundo",
          showlegend = FALSE,
          fill = "tozeroy",
          fillcolor = conf$fill,
          marker = list(size = 3, color = conf$color),
          line = list(color = conf$color, width = 2.5),
          hovertemplate = "%{y:,.2f} %<extra></extra>"
        ) %>%
          layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Participación (%)"),
            hovermode = "x unified"
          )
      })
      
      # Top 5
      output$plot_top5_share <- renderPlotly({
        req(input$year_share_range)
        yr <- input$year_share_range
        col_name <- conf$share
        unit <- "%"
        
        year_ref <- yr[2]  
        
        top_names <- data_full %>%
          dplyr::filter(year == year_ref, Is_Country) %>%
          dplyr::arrange(dplyr::desc(.data[[col_name]])) %>%
          dplyr::pull(entity) %>%
          unique() %>%
          head(5)
        
        if (length(top_names) == 0) return(plotly_empty())
        
        top_hist <- data_full %>%
          dplyr::filter(
            entity %in% top_names,
            year >= yr[1], year <= yr[2]
          ) %>%
          dplyr::select(year, entity, Val = dplyr::all_of(col_name))
        
        factor_order <- top_hist %>%
          dplyr::group_by(entity) %>%
          dplyr::summarise(max_val = max(Val, na.rm = TRUE)) %>%
          dplyr::arrange(dplyr::desc(max_val)) %>%
          dplyr::pull(entity)
        
        top_hist$entity <- factor(top_hist$entity, levels = factor_order)
        top_hist <- top_hist %>% dplyr::arrange(entity, year)
        
        plot_ly(
          top_hist,
          x = ~year, y = ~Val, color = ~entity,
          colors = "Set1",
          type = "scatter", mode = "lines+markers",
          marker = list(size = 3), line = list(width = 2), fill = "none",
          hovertemplate = paste0("<b>%{y:,.2f}</b> ", unit)
        ) %>%
          layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = unit),
            legend = list(
              orientation = "h", xanchor = "center", x = 0.5, y = -0.6
            ),
            margin = list(b = 80, l = 50, r = 20, t = 20),
            hovermode = "x unified"
          )
      })
      
 
      output$plot_country_share <- renderPlotly({
        req(selected_iso(), input$year_share_range)
        yr <- input$year_share_range
        col_name <- conf$share
        unit <- "%"
        
        c_dat <- data_full %>%
          dplyr::filter(
            iso3 == selected_iso(),
            year >= yr[1], year <= yr[2]
          ) %>%
          dplyr::arrange(year)
        
        if (nrow(c_dat) == 0) return(plotly_empty())
        
        nm <- country_name()
        if (is.null(nm)) nm <- "País seleccionado"
        
        plot_ly(
          c_dat,
          x = ~year,
          y = ~get(col_name),
          type = "scatter",
          mode = "lines+markers",
          name = nm,
          showlegend = FALSE,
          fill = "tozeroy",
          fillcolor = conf$fill,
          line = list(color = conf$color, width = 2.5),
          marker = list(size = 4, color = conf$color),
          hovertemplate = paste0("%{y:,.2f} ", unit, "<extra></extra>")
        ) %>%
          layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = unit),
            hovermode = "x unified"
          )
      })
    }
    
 
    output$plot_world_gen <- renderPlotly({
      req(input$year_gen_range)
      yr <- input$year_gen_range
      col_name <- conf$gen
      unit <- "TWh"
      
      w_dat <- er_world %>%
        dplyr::filter(year >= yr[1], year <= yr[2]) %>%
        dplyr::select(year, Val = dplyr::all_of(col_name)) %>%
        dplyr::arrange(year)
      
      plot_ly(
        w_dat,
        x = ~year,
        y = ~Val,
        type = "scatter",
        mode = "lines+markers",
        name = "Mundo",
        showlegend = FALSE,
        fill = "tozeroy",
        fillcolor = conf$fill,
        marker = list(size = 3, color = conf$color),
        line = list(color = conf$color, width = 2.5),
        hovertemplate = paste0("%{y:,.1f} ", unit, "<extra></extra>")
      ) %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = unit),
          hovermode = "x unified"
        )
    })
    
 
    output$plot_top5_gen <- renderPlotly({
      req(input$year_gen_range)
      yr <- input$year_gen_range
      col_name <- conf$gen
      unit <- "TWh"
      
      year_ref <- yr[2]
      
      top_names <- data_full %>%
        dplyr::filter(year == year_ref, Is_Country) %>%
        dplyr::arrange(dplyr::desc(.data[[col_name]])) %>%
        dplyr::pull(entity) %>%
        unique() %>%
        head(5)
      
      if (length(top_names) == 0) return(plotly_empty())
      
      top_hist <- data_full %>%
        dplyr::filter(
          entity %in% top_names,
          year >= yr[1], year <= yr[2]
        ) %>%
        dplyr::select(year, entity, Val = dplyr::all_of(col_name))
      
      factor_order <- top_hist %>%
        dplyr::group_by(entity) %>%
        dplyr::summarise(max_val = max(Val, na.rm = TRUE)) %>%
        dplyr::arrange(dplyr::desc(max_val)) %>%
        dplyr::pull(entity)
      
      top_hist$entity <- factor(top_hist$entity, levels = factor_order)
      top_hist <- top_hist %>% dplyr::arrange(entity, year)
      
      plot_ly(
        top_hist,
        x = ~year, y = ~Val, color = ~entity,
        colors = "Set1",
        type = "scatter", mode = "lines+markers",
        marker = list(size = 3), line = list(width = 2), fill = "none",
        hovertemplate = paste0("<b>%{y:,.1f}</b> ", unit)
      ) %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = unit),
          legend = list(
            orientation = "h", xanchor = "center", x = 0.5, y = -0.6
          ),
          margin = list(b = 80, l = 50, r = 20, t = 20),
          hovermode = "x unified"
        )
    })
    
 
    output$plot_country_gen <- renderPlotly({
      req(selected_iso(), input$year_gen_range)
      yr <- input$year_gen_range
      col_name <- conf$gen
      unit <- "TWh"
      
      c_dat <- data_full %>%
        dplyr::filter(
          iso3 == selected_iso(),
          year >= yr[1], year <= yr[2]
        ) %>%
        dplyr::arrange(year)
      
      if (nrow(c_dat) == 0) return(plotly_empty())
      
      nm <- country_name()
      if (is.null(nm)) nm <- "País seleccionado"
      
      plot_ly(
        c_dat,
        x = ~year,
        y = ~get(col_name),
        type = "scatter",
        mode = "lines+markers",
        name = nm,
        showlegend = FALSE,
        fill = "tozeroy",
        fillcolor = conf$fill,
        line = list(color = conf$color, width = 2.5),
        marker = list(size = 4, color = conf$color),
        hovertemplate = paste0("%{y:,.2f} ", unit, "<extra></extra>")
      ) %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = unit),
          hovermode = "x unified"
        )
    })
  })
}


global_share_server <- function(id, data_full, world_geo, er_world, col_name, color) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_iso <- reactiveVal("MEX")
    
    observeEvent(input$map_share_shape_click, {
      if (!is.null(input$map_share_shape_click$id))
        selected_iso(input$map_share_shape_click$id)
    })
    
    output$country_title <- renderText({
      req(selected_iso())
      nm <- data_full %>%
        dplyr::filter(iso3 == selected_iso()) %>%
        dplyr::pull(entity) %>%
        unique()
      if (length(nm) == 0) return("Evolución histórica del país seleccionado (%)")
      paste("Evolución histórica (%):", nm[1])
    })
    
    # Slider de rango
    output$slider_ui <- renderUI({
      sliderInput(
        ns("year_range"), "Periodo:",
        min   = min(data_full$year, na.rm = TRUE),
        max   = max(data_full$year, na.rm = TRUE),
        value = c(
          min(data_full$year, na.rm = TRUE),
          max(data_full$year, na.rm = TRUE)
        ),
        step = 1, sep = ""
      )
    })
    

    output$map_share <- renderLeaflet({
      req(input$year_range)
      yr <- input$year_range
      year_map <- yr[2]
      
      unit <- "%"
      bins <- c(0, 1, 5, 10, 20, 50, 100)
      
      map_dat <- data_full %>%
        dplyr::filter(year == year_map, Is_Country) %>%
        dplyr::select(iso3, entity, Val = dplyr::all_of(col_name))
      
      geo <- world_geo %>%
        dplyr::left_join(map_dat, by = c("iso_a3" = "iso3"))
      
      vals <- geo$Val
      pal <- colorBin(
        palette = "YlGnBu",
        domain = vals,
        bins = bins,
        na.color = "#ffffff"
      )
      
      leaflet(
        geo,
        options = leafletOptions(
          worldCopyJump = FALSE,
          minZoom = 1.5
        )
      ) %>%
        setMaxBounds(-180, -90, 180, 90) %>%
        addMapPane("fondo", zIndex = 400) %>%
        addMapPane("datos", zIndex = 450) %>%
        addMapPane("etiquetas", zIndex = 500) %>%
        addProviderTiles(
          "CartoDB.PositronNoLabels",
          options = providerTileOptions(pane = "fondo", noWrap = TRUE)
        ) %>%
        addPolygons(
          fillColor = ~pal(Val),
          weight = 1, color = "white", opacity = 1, fillOpacity = 0.85,
          layerId = ~iso_a3, options = pathOptions(pane = "datos"),
          highlightOptions = highlightOptions(
            weight = 2, color = "#444", bringToFront = TRUE
          ),
          label = sprintf(
            "<strong>%s</strong><br/>%s",
            geo$entity,
            ifelse(
              is.na(geo$Val),
              "Sin información",
              paste0(format(round(geo$Val, 2), big.mark = ","), " ", unit)
            )
          ) %>% lapply(HTML)
        ) %>%
        addProviderTiles(
          "CartoDB.PositronOnlyLabels",
          options = providerTileOptions(pane = "etiquetas", noWrap = TRUE)
        ) %>%
        setView(0, 20, 1.5) %>%
        addLegend(
          pal = pal, values = vals, title = unit,
          position = "bottomright", na.label = "Sin información"
        )
    })
    
  
    output$plot_world <- renderPlotly({
      req(input$year_range)
      yr <- input$year_range
      
      w_dat <- er_world %>%
        dplyr::filter(year >= yr[1], year <= yr[2]) %>%
        dplyr::select(year, Val = dplyr::all_of(col_name)) %>%
        dplyr::arrange(year)
      
      fill_rgba <- to_rgba(color)
      
      plot_ly(
        w_dat,
        x = ~year,
        y = ~Val,
        type = "scatter",
        mode = "lines+markers",
        fill = "tozeroy",
        fillcolor = fill_rgba,
        marker = list(size = 3, color = color),
        line = list(color = color, width = 2.5),
        hovertemplate = "%{y:,.2f} %<extra></extra>"
      ) %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "Participación (%)"),
          hovermode = "x unified"
        )
    })
    
  
    output$plot_top5 <- renderPlotly({
      req(input$year_range)
      yr <- input$year_range
      year_ref <- yr[2]
      
      top_names <- data_full %>%
        dplyr::filter(year == year_ref, Is_Country) %>%
        dplyr::arrange(dplyr::desc(.data[[col_name]])) %>%
        dplyr::pull(entity) %>%
        unique() %>%
        head(5)
      
      if (length(top_names) == 0) return(plotly_empty())
      
      top_hist <- data_full %>%
        dplyr::filter(
          entity %in% top_names,
          year >= yr[1], year <= yr[2]
        ) %>%
        dplyr::select(year, entity, Val = dplyr::all_of(col_name))
      
      factor_order <- top_hist %>%
        dplyr::group_by(entity) %>%
        dplyr::summarise(max_val = max(Val, na.rm = TRUE)) %>%
        dplyr::arrange(dplyr::desc(max_val)) %>%
        dplyr::pull(entity)
      
      top_hist$entity <- factor(top_hist$entity, levels = factor_order)
      top_hist <- top_hist %>% dplyr::arrange(entity, year)
      
      plot_ly(
        top_hist,
        x = ~year,
        y = ~Val,
        color = ~entity,
        colors = "Set1",
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 3),
        line = list(width = 2),
        fill = "none",
        hovertemplate = "<b>%{y:,.2f}</b> %<extra></extra>"
      ) %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "Participación (%)"),
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.7
          ),
          margin = list(b = 80, l = 50, r = 20, t = 20),
          hovermode = "x unified"
        )
    })
    
   
    output$plot_country <- renderPlotly({
      req(selected_iso(), input$year_range)
      yr <- input$year_range
      
      c_dat <- data_full %>%
        dplyr::filter(
          iso3 == selected_iso(),
          year >= yr[1], year <= yr[2]
        ) %>%
        dplyr::arrange(year)
      
      if (nrow(c_dat) == 0) return(plotly_empty())
      
      plot_ly(
        c_dat,
        x = ~year,
        y = ~get(col_name),
        type = "scatter",
        mode = "lines+markers",
        fill = "tozeroy",
        fillcolor = to_rgba(color),
        line = list(color = color, width = 2.5),
        marker = list(size = 4, color = color),
        hovertemplate = "%{y:,.2f} %<extra></extra>"
      ) %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "Participación (%)"),
          hovermode = "x unified"
        )
    })
  })
}


ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  tags$head(
  tags$style(HTML("
      body { background-color: #f3f5f9; }
      .leaflet-container { background: #f0f2f5; } 
      .legend {
        font-size: 11px;
        padding: 8px;
        background: rgba(255,255,255,0.95);
        border-radius: 4px;
        border: 1px solid #eee;
      }

      /* Layout del navlist lateral */
      .navlayout .col-sm-3 {
        background-color: #e9edf5;
        border-right: 1px solid #d1d7e5;
        padding-top: 15px;
      }
      .navlayout .col-sm-9 {
        background-color: #f9fafc;
      }

      .nav-pills > li > a {
        border-radius: 8px;
        font-weight: 600;
        color: #5f6b7a;
        margin-bottom: 6px;
      }
      .nav-pills > li.active > a,
      .nav-pills > li > a.active {
        background-color: #2c3e50 !important;
        color: #ffffff !important;
      }
      .nav-stacked > li > a {
        padding-top: 10px;
        padding-bottom: 10px;
      }

      /* Cards generales un poco más compactas */
      .card-body.bg-light {
        padding-top: 8px;
        padding-bottom: 8px;
      }

      /* Los card internos dentro de fondos grises sin tanto aire */
      .card-body.bg-light .card {
        margin-top: 4px !important;
        margin-bottom: 4px !important;
      }

      /* ---------------- INDICADORES (4 cajitas) ---------------- */

      /* El wrapper ya no es una card grande; solo un row compacto */
      .indicators-row {
        margin-bottom: 0 !important;
      }

      .ind-card {
        background-color: #ffffff;
        border-radius: 0.75rem;
        box-shadow: 0 0.25rem 0.5rem rgba(0,0,0,0.08);
        padding: 0.9rem 1.1rem;
        margin-bottom: 0;      /* SIN espacio extra debajo */
        text-align: left;
        border-left: 5px solid #0d6efd;
      }
      .ind-card h6 {
        font-size: 0.8rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: #6c757d;
        margin-bottom: 0.25rem;
        display: flex;
        align-items: center;
      }
      .ind-main-value {
        font-size: 1.6rem;
        font-weight: 700;
        margin: 0;
      }
      .ind-sub-value {
        font-size: 0.85rem;
        color: #6c757d;
        margin-top: 0.1rem;
        margin-bottom: 0;      /* clave para que no se vea aire abajo */
      }
      .ind-icon {
        font-size: 1.1rem;
        margin-right: 0.4rem;
        opacity: 0.9;
      }
      .ind-primary  { border-left-color: #0d6efd; }
      .ind-success  { border-left-color: #198754; }
      .ind-warning  { border-left-color: #ffc107; }
      .ind-info     { border-left-color: #0dcaf0; }

      /* ---------------- MAPA + 3 GRÁFICAS ---------------- */

      /* Card del mapa (share y generación) */
      .map-section-card .card-body {
        padding-top: 10px;
        padding-bottom: 10px;
      }
      .map-section-card .filters-panel,
      .map-section-card .map-panel {
        margin-bottom: 0;   /* evita aire raro dentro del card del mapa */
      }

      /* Card de las 3 gráficas debajo del mapa */
      .charts-row-card .card-body {
        padding-top: 8px;
        padding-bottom: 8px;
      }

      /* Las tarjetitas de cada gráfica */
      .small-chart-card {
        background: #ffffff;
        border-radius: 0.75rem;
        box-shadow: 0 0.25rem 0.5rem rgba(0,0,0,0.05);
        padding: 0.75rem;
        height: 100%;
      }
      .small-chart-card h6 {
        font-weight: 700;
        font-size: 0.9rem;
        margin-bottom: 0.4rem;
      }
      .small-chart-card small {
        font-size: 0.75rem;
      }

      /* Elimina posibles márgenes fantasma debajo de htmlwidgets */
      .leaflet.html-widget, .plotly.html-widget {
        margin-bottom: 0 !important;
      }
  "))
),
  
  div(
    class = "container-fluid py-4 text-center shadow-sm mb-4",
    style = "background: linear-gradient(90deg,#1b2838,#283c52); color:#ffffff;",
    h1(
      "Monitor global de transición energética",
      style = "font-weight: 900; margin-bottom: 8px;"
    ),
    p(
      "Análisis histórico y proyecciones de la matriz energética mundial basados en Our World in Data y el Energy Institute (2025).",
      class = "small mb-0",
      style = "opacity: 0.9;"
    )
  ),
  
 
  div(
    class = "container-fluid navlayout",
    navlistPanel(
      id = "main_nav",
      widths = c(2, 10),
      well = FALSE,
      
     
      tabPanel(
        "Panorama global y análisis",
        div(
          class = "card shadow-sm mb-4 border-0",
          div(
            class = "card-header bg-white py-3",
            style = "border-top: 5px solid #2c3e50;",
            h3(
              icon("globe"),
              "Panorama global de la transición energética",
              style = "color:#2c3e50; font-weight: 800; margin: 0;"
            )
          ),
          div(
            class = "card-body",
            p(
              class = "text-muted small mb-2",
              "Esta vista resume la evolución de las energías renovables modernas en el sistema energético mundial y el cambio en el equilibrio entre combustibles fósiles y renovables."
            ),
            div(
              class = "alert alert-light border mb-0 p-3",
              tags$strong("Nota metodológica – método de sustitución: "),
              span(
                class = "small text-muted",
                "las métricas de participación en energía primaria siguen la metodología de Our World in Data. La electricidad renovable se convierte a energía primaria equivalente suponiendo la cantidad de combustibles fósiles que habría sido necesaria en centrales térmicas típicas."
              )
            )
          )
        ),
        
       
div(
  class = "card shadow-sm mb-3 border-0",
  div(
    class = "card-header bg-white py-2",
    style = "border-top: 3px solid #2c3e50;",
    h5(
      icon("layer-group"),
      "Composición del mix renovable (TWh)",
      style = "font-weight: 700; margin: 0;"
    ),
    p(
      class = "text-muted small mb-0 mt-1",
      "Generación renovable mundial desagregada por fuente (hidro, solar, eólica, biocombustibles y otros renovables modernos)."
    )
  ),
  div(
    class = "card-body bg-light",
    div(
      class = "d-flex align-items-center bg-white p-3 rounded shadow-sm mb-3",
      strong("Línea de tiempo:", class = "me-3 small"),
      div(style = "flex-grow: 1;", uiOutput("slider_global_ui"))
    ),
    div(
      class = "bg-white rounded shadow-sm p-3",
      plotlyOutput("global_plot", height = "420px")
    )
  )
),


div(
  class = "mb-3",
  fluidRow(
    class = "indicators-row",
    column(
      width = 3,
      div(
        class = "ind-card ind-primary",
        h6(icon("leaf", class = "ind-icon"), "Total renovables (TWh) por año"),
        p(class = "ind-main-value", textOutput("ind_total_renovables")),
        p(
          class = "ind-sub-value",
          "Incluye hidro, eólica, solar y otros renovables modernos"
        )
      )
    ),
    column(
      width = 3,
      div(
        class = "ind-card ind-success",
        h6(icon("chart-line", class = "ind-icon"), "Crecimiento EN EL PERIODO"),
        p(class = "ind-main-value", textOutput("ind_crecimiento_vs_inicio")),
        p(
          class = "ind-sub-value",
          "Variación del año más reciente con respecto al primer año del periodo seleccionado"
        )
      )
    ),
    column(
      width = 3,
      div(
        class = "ind-card ind-warning",
        h6(icon("layer-group", class = "ind-icon"), "Total renovables acumulado"),
        p(class = "ind-main-value", textOutput("ind_total_renovablesPorAnio")),
        p(
          class = "ind-sub-value",
          "Suma histórica hasta el año seleccionado"
        )
      )
    ),
    column(
      width = 3,
      div(
        class = "ind-card ind-info",
        h6(icon("percent", class = "ind-icon"), "Crecimiento vs año anterior"),
        p(class = "ind-main-value", textOutput("comparacion_anios")),
        p(
          class = "ind-sub-value",
          "Tasa de crecimiento interanual global"
        )
      )
    )
  )
),
      
        div(
          class = "card shadow-sm mb-4 border-0",
          div(
            class = "card-header bg-white py-2",
            style = "border-top: 3px solid #2c3e50;",
            h5(
              icon("robot"),
              "Proyección IA: fósiles vs renovables",
              style = "font-weight: 700; margin: 0;"
            ),
            p(
              class = "text-muted small mb-0 mt-1",
              "Modelo LSTM aplicado a la participación de combustibles fósiles y renovables en la energía primaria."
            )
          ),
          div(
            class = "card-body bg-light",
            div(
              class = "bg-white rounded shadow-sm p-3",
              plotlyOutput("forecast_plot", height = "480px")
            )
          )
        ),
        
                
        div(
          class = "card shadow-sm mb-4 border-0",
          div(
            class = "card-header bg-white py-2",
            style = "border-top: 3px solid #2c3e50;",
            h5(
              icon("globe-americas"),
              "Patrones globales por país, región y nivel de ingreso",
              style = "font-weight: 700; margin: 0;"
            ),
            p(
              class = "text-muted small mb-0 mt-1",
              "Vista interactiva que combina el ranking de países por participación renovable, el ritmo de crecimiento por continente, la evolución por nivel de ingreso y la dispersión regional en el porcentaje de energías renovables."
            )
          ),
          div(
            class = "card-body bg-light",
            fluidRow(
              # --- Columna de filtros ---
              column(
                3,
                div(
                  class = "bg-white rounded shadow-sm p-3 mb-3",
                  h6(
                    "Filtros",
                    class = "border-bottom pb-2 mb-2",
                    style = "font-weight: 700;"
                  ),
                  p(
                    class = "small text-muted",
                    "El año de referencia se usa para calcular el top 10 de países y el periodo de crecimiento anual por continente."
                  ),
                  uiOutput("global_year_ui")
                )
              ),
              
            
              column(
                9,
               
                fluidRow(
                  column(
                    6,
                    div(
                      class = "bg-white rounded shadow-sm p-3 mb-3",
                      h6("Top 10 países por % renovable", class = "small fw-bold mb-2"),
                      plotlyOutput("global_top10_plot", height = "300px")
                    )
                  ),
                  column(
                    6,
                    div(
                      class = "bg-white rounded shadow-sm p-3 mb-3",
                      h6("Cambio anual por continente", class = "small fw-bold mb-2"),
                      p(
                        class = "text-muted small mb-1",
                        "Pendiente de una regresión lineal del % renovable sobre el tiempo, en el periodo seleccionado."
                      ),
                      plotlyOutput("global_cont_growth_plot", height = "300px")
                    )
                  )
                ),
                
             
                fluidRow(
                  column(
                    6,
                    div(
                      class = "bg-white rounded shadow-sm p-3 mb-3",
                      h6("Tendencia por nivel de ingreso", class = "small fw-bold mb-2"),
                      plotlyOutput("global_income_trend_plot", height = "300px")
                    )
                  ),
                  column(
                    6,
                    div(
                      class = "bg-white rounded shadow-sm p-3 mb-0",
                      h6("Dispersión regional del % renovable", class = "small fw-bold mb-2"),
                      plotlyOutput("global_region_disp_plot", height = "300px")
                    )
                  )
                )
              )
            )
          )
        ),

               
        div(
          class = "card shadow-sm mb-4 border-0",
          div(
            class = "card-header bg-white py-2",
            style = "border-top: 3px solid #2c3e50;",
            h5(
              icon("balance-scale"),
              "Modelo explicativo y desarrollo económico en la transición renovable",
              style = "font-weight: 700; margin: 0;"
            )
          ),
          div(
            class = "card-body bg-light",
            
         
            div(
              class = "bg-white rounded shadow-sm p-3 mb-3",
              p(
                class = "text-muted small mb-2",
                "Se estimó un modelo de panel dinámico para explicar la participación de energías renovables en la energía primaria equivalente por país y año."
              ),
              tags$ul(
                class = "small text-muted",
                tags$li(
                  strong("Variable dependiente: "), 
                  tags$code("Renewables"),
                  " (% de energía primaria equivalente proveniente de renovables modernas por país-año)."
                ),
                tags$li(
                  strong("Estructura de panel: "),
                  "se usan países con trayectoria anual suficiente en ",
                  tags$code("Renewables"),
                  "; se excluyeron los países sin información en toda la serie."
                ),
                tags$li(
                  strong("Componente dinámico (persistencia): "),
                  "se incluye el rezago de la propia variable, ",
                  tags$code("Renewables_lag"),
                  " (t–1), que captura la tendencia de cada país a mantener su nivel previo de participación renovable."
                ),
                tags$li(
                  strong("Variables económicas: "),
                  "participación de combustibles fósiles en la energía primaria (",
                  tags$code("Fossil_Fuels"),
                  ") y grupo de ingreso del país (",
                  tags$code("Income_Group"),
                  ") como aproximación al nivel de desarrollo económico."
                ),
                tags$li(
                  strong("Variable de política energética: "),
                  "distancia al año objetivo de emisiones netas cero (meta ",
                  tags$i("net-zero"),
                  "), transformada como término cuadrado en décadas, ",
                  tags$code("(YearsToTarget / 10)^2"),
                  ", a partir del archivo de metas climáticas por país."
                ),
                tags$li(
                  strong("País y año en el modelo: "),
                  "el modelo incluye efectos fijos por país y por año: para cada país se estima un intercepto propio (",
                  tags$code("α_pais"),
                  ") que recoge sus características estructurales constantes en el tiempo, y para cada año un intercepto global (",
                  tags$code("γ_año"),
                  ") que captura shocks comunes (cambios tecnológicos, precios de la energía, etc.)."
                )
              ),
              tags$hr(class = "my-2"),
              p(
                class = "small text-muted mb-1",
                strong("Esquema de la ecuación: ")
              ),
              p(
                class = "small text-muted mb-0",
                HTML(
                  "Renewables<sub>pais,t</sub> = &beta;<sub>1</sub> Renewables<sub>pais,t-1</sub> 
                   + &beta;<sub>2</sub> Fossil_Fuels<sub>pais,t</sub>
                   + &beta;<sub>3</sub> (YearsToTarget<sub>pais,t</sub>/10)<sup>2</sup>
                   + &beta;<sub>4</sub> Income_Group<sub>pais</sub>
                   + &alpha;<sub>pais</sub> + &gamma;<sub>t</sub> + &epsilon;<sub>pais,t</sub>"
                )
              )
            ),
            
           
            div(
              class = "bg-white rounded shadow-sm p-3 mb-3",
              h6("Indicadores del modelo y relación con el desarrollo económico", 
                 class = "fw-bold mb-2"),
              p(
                class = "small text-muted",
                "La primera gráfica muestra los coeficientes principales del modelo (con intervalos de confianza al 95%). 
                 La segunda gráfica combina el efecto fijo estimado para cada país con su participación renovable promedio, 
                 coloreando por grupo de ingreso, para explorar la relación entre desarrollo económico y uso de energías limpias."
              ),
              br(),
              fluidRow(
                column(
                  6,
                  h6("Coeficientes del modelo", class = "small fw-bold"),
                  plotlyOutput("coef_plot_modelo_panel", height = "320px")
                ),
                column(
                  6,
                  h6("Países: estructura renovable y nivel de ingreso", 
                     class = "small fw-bold"),
                  plotlyOutput("dev_renew_plot", height = "320px")
                )
              )
            ),
          )
        ),

      
        global_share_tab_ui("global_share", color = "#2c3e50")
      ),
      
      
      tabPanel(
        "Energía solar",
        energy_tab_ui(
          "solar",
          title = "Energía solar",
          color = cols_def$Solar$color,
          icon_name = "sun",
          description = "La energía solar es una de las tecnologías que más rápido ha crecido, impulsando el aumento de la generación renovable en numerosos países.",
          has_share = TRUE
        )
      ),
      
     
      tabPanel(
        "Energía eólica",
        energy_tab_ui(
          "wind",
          title = "Energía eólica",
          color = cols_def$Wind$color,
          icon_name = "wind",
          description = "La energía eólica, terrestre y marina, se ha consolidado como pilar del crecimiento renovable, con un peso creciente en la generación eléctrica.",
          has_share = TRUE
        )
      ),
      
     
      tabPanel(
        "Energía hidroeléctrica",
        energy_tab_ui(
          "hydro",
          title = "Energía hidroeléctrica",
          color = cols_def$Hydro$color,
          icon_name = "water",
          description = "La hidroeléctrica sigue siendo la mayor fuente renovable moderna, clave en muchos sistemas eléctricos de baja emisión.",
          has_share = TRUE
        )
      ),
      
     
      tabPanel(
        "Biocombustibles",
        energy_tab_ui(
          "bio",
          title = "Biocombustibles y bioenergía moderna",
          color = cols_def$Bio$color,
          icon_name = "leaf",
          description = "Los biocombustibles líquidos y la bioenergía moderna complementan a otras renovables, especialmente en el sector transporte.",
          has_share = FALSE
        )
      )
    )
  ),
  
  
  div(
    class = "container-fluid py-3 mt-4 bg-light text-center border-top",
    p(
      class = "mb-1 text-muted small",
      style = "font-weight: 600;",
      "Fuente de datos:"
    ),
    p(
      class = "text-muted small",
      style = "font-style: italic;",
      "Ember (2025); Energy Institute - Statistical Review of World Energy (2025) – con procesamiento de Our World in Data."
    )
  )
)



server <- function(input, output, session) {

  global_long <- reactive({
    df <- er_world %>%
      select(
        year,
        Solar = cols_def$Solar$gen,
        Eólica = cols_def$Wind$gen,
        Hidro  = cols_def$Hydro$gen,
        Bio    = cols_def$Bio$gen,
        Geo    = cols_def$Geo$gen
      ) %>%
      pivot_longer(
        cols = -year,
        names_to = "Fuente",
        values_to = "TWh"
      ) %>%
      filter(TWh != 0)
    
    total_por_fuente <- df %>%
      group_by(Fuente) %>%
      summarise(Total = max(TWh, na.rm = TRUE)) %>%
      arrange(desc(Total))
    
    df$Fuente <- factor(df$Fuente, levels = total_por_fuente$Fuente)
    df %>% arrange(year)
  })
  
output$slider_global_ui <- renderUI({
  dat <- global_long()
  sliderInput(
    "anim_year_range", "Periodo:",
    min   = min(dat$year, na.rm = TRUE),
    max   = max(dat$year, na.rm = TRUE),
    value = c(
      min(dat$year, na.rm = TRUE),
      max(dat$year, na.rm = TRUE)
    ),
    step = 1, sep = "",
    width = "100%"
  )
})
  
output$global_plot <- renderPlotly({
  req(input$anim_year_range)
  yr <- input$anim_year_range
  
  dat <- global_long() %>%
    dplyr::filter(year >= yr[1], year <= yr[2])
  
  cols_g <- c(
    "Solar" = cols_def$Solar$color,
    "Eólica" = cols_def$Wind$color,
    "Hidro"  = cols_def$Hydro$color,
    "Bio"    = cols_def$Bio$color,
    "Geo"    = cols_def$Geo$color
  )
  
  plot_ly(
    dat, x = ~year, y = ~TWh,
    color = ~Fuente, colors = cols_g,
    type = "scatter", mode = "lines+markers",
    fill = "tozeroy",
    hovertemplate = "<b>%{y:,.0f} TWh</b>"
  ) %>%
    layout(
      yaxis = list(title = "Generación (TWh)"),
      xaxis = list(title = ""),
      legend = list(orientation = "h", y = 1.1),
      hovermode = "x unified"
    )
})

  
  datos_filtrados <- reactive({
    req(input$anim_year_range)
    yr <- input$anim_year_range
    cruce_er_por_anio |>
      dplyr::filter(year == yr[2])
  })

  
  output$ind_total_renovables <- renderText({
    df <- datos_filtrados()
    if (nrow(df) == 0) return("Sin datos")
    
    valor <- df$total_renovables_twh[1]
    valor_formateado <- format(round(valor, 1), big.mark = ",")
    paste0(valor_formateado, " TWh")
  })

  output$ind_crecimiento_vs_inicio <- renderText({
    req(input$anim_year_range)
    yr <- input$anim_year_range
    year_ini <- yr[1]
    year_fin <- yr[2]

    df_ini <- cruce_er_por_anio |>
      dplyr::filter(year == year_ini)

    df_fin <- cruce_er_por_anio |>
      dplyr::filter(year == year_fin)

    if (nrow(df_ini) == 0 || nrow(df_fin) == 0) return("Sin datos")

    base_rango <- df_ini$total_renovables_twh[1]
    valor_actual <- df_fin$total_renovables_twh[1]

    if (is.na(base_rango) || base_rango == 0) return("Sin datos")

    dif_abs <- valor_actual - base_rango
    dif_pct <- (valor_actual - base_rango) / base_rango * 100

    dif_abs_fmt <- format(round(dif_abs, 1), big.mark = ",")
    dif_pct_fmt <- format(round(dif_pct, 1), big.mark = ",")

    signo <- ifelse(dif_abs >= 0, "+", "")

    paste0(signo, dif_abs_fmt, " TWh (", signo, dif_pct_fmt, " %)")
  })


  output$ind_total_renovablesPorAnio <- renderText({
    req(input$anim_year_range)
    yr <- input$anim_year_range
    
    resultadoAño <- er_data |>
      dplyr::filter(
        entity == "World",
        year >= yr[1], year <= yr[2]
      ) |>
      dplyr::mutate(
        total_renovables_anio =
          electricity_from_wind_t_wh__modern_renewable_prod +
          electricity_from_hydro_t_wh__modern_renewable_prod +
          electricity_from_solar_t_wh__modern_renewable_prod +
          other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ) |>
      dplyr::summarise(
        total_renovables_anio = sum(total_renovables_anio, na.rm = TRUE)
      )
    
    val <- resultadoAño[["total_renovables_anio"]]
    paste0(format(round(val, 1), big.mark = ","), " TWh")
  })


  output$comparacion_anios <- renderText({
    req(input$anim_year_range)
    yr <- input$anim_year_range
    year_now <- yr[2]
    
    if (year_now <= min(cruce_er_por_anio$year, na.rm = TRUE)) {
      return("Sin datos")
    }
    
    actual <- er_data |>
      dplyr::filter(entity == "World", year == year_now) |>
      dplyr::mutate(
        total_renovables_anio =
          electricity_from_wind_t_wh__modern_renewable_prod +
          electricity_from_hydro_t_wh__modern_renewable_prod +
          electricity_from_solar_t_wh__modern_renewable_prod +
          other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ) |>
      dplyr::summarise(
        total_renovables_anio = sum(total_renovables_anio, na.rm = TRUE)
      )
    
    anterior <- er_data |>
      dplyr::filter(entity == "World", year == year_now - 1) |>
      dplyr::mutate(
        total_renovables_anio =
          electricity_from_wind_t_wh__modern_renewable_prod +
          electricity_from_hydro_t_wh__modern_renewable_prod +
          electricity_from_solar_t_wh__modern_renewable_prod +
          other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ) |>
      dplyr::summarise(
        total_renovables_anio = sum(total_renovables_anio, na.rm = TRUE)
      )
    
    val_act <- actual[["total_renovables_anio"]]
    val_ant <- anterior[["total_renovables_anio"]]
    
    if (is.na(val_ant) || val_ant == 0) return("Sin datos")
    
    tasa <- (val_act - val_ant) / val_ant * 100
    paste0(round(tasa, 1), " %")
  })


  output$forecast_plot <- renderPlotly({
    p_dat <- pred_data %>%
      mutate(Energy_Type = ifelse(Type == "Fossil", "Fósiles", "Renovables"))
    
    hist_dat <- p_dat %>% filter(!is.na(Real))
    pred_dat_raw <- p_dat %>% filter(!is.na(Pred_LSTM))
    
    last_real_year <- max(hist_dat$Year, na.rm = TRUE)
    
    stitch_points <- hist_dat %>%
      filter(Year == last_real_year) %>%
      mutate(Pred_LSTM = Real) %>%
      select(Year, Type, Energy_Type, Pred_LSTM)
    
    pred_dat_stitched <- bind_rows(stitch_points, pred_dat_raw) %>%
      arrange(Year)
    
    cols_pred <- c(
      "Fósiles"    = "#37474F",
      "Renovables" = "#2E7D32"
    )
    
    plot_ly() %>%
      add_trace(
        data = hist_dat,
        x = ~Year, y = ~Real,
        color = ~Energy_Type, colors = cols_pred,
        type = "scatter", mode = "lines",
        line = list(width = 3),
        name = ~paste(Energy_Type, "(histórico)")
      ) %>%
      add_trace(
        data = pred_dat_stitched,
        x = ~Year, y = ~Pred_LSTM,
        color = ~Energy_Type, colors = cols_pred,
        type = "scatter", mode = "lines",
        line = list(width = 3, dash = "dot"),
        name = ~paste(Energy_Type, "(proyección)"),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = "Participación (%)"),
        xaxis = list(title = "Año"),
        hovermode = "x unified",
        shapes = list(list(
          type = "line",
          x0 = last_real_year, x1 = last_real_year,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "gray", width = 1, dash = "dash")
        )),
        annotations = list(list(
          x = last_real_year, y = 0.05,
          text = "Inicio de la proyección",
          showarrow = FALSE, xanchor = "right",
          yref = "paper",
          font = list(size = 10, color = "gray")
        ))
      )
  })

  
  coef_modelo_panel <- reactive({
    path_coef <- "modelo_panel_coeficientes_con_target_cuadrado.csv"
    if (!file.exists(path_coef)) return(NULL)
    
    df <- read.csv(path_coef, stringsAsFactors = FALSE)
    
    # Filtramos términos clave del modelo
    df <- df %>%
      dplyr::filter(
        grepl("Renewables_lag|Fossil_Fuels|YearsToTarget_dec_sq|Income_Group::", term)
      ) %>%
      dplyr::mutate(
        term_label = dplyr::case_when(
          term == "Renewables_lag" ~ "Rezago Renewables (t-1)",
          term == "Fossil_Fuels" ~ "Participación fósiles",
          term == "YearsToTarget_dec_sq" ~ "(YearsToTarget/10)^2",
          grepl("Income_Group::Low-income", term) ~ "Ingreso: Low-income",
          grepl("Income_Group::Lower-middle", term) ~ "Ingreso: Lower-middle",
          grepl("Income_Group::Upper-middle", term) ~ "Ingreso: Upper-middle",
          TRUE ~ term
        ),
        ci_low  = estimate - 1.96 * std.error,
        ci_high = estimate + 1.96 * std.error
      )
    
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  output$coef_plot_modelo_panel <- renderPlotly({
    df <- coef_modelo_panel()
    if (is.null(df)) {
      return(plotly::plotly_empty())
    }
    
    df <- df %>% dplyr::arrange(estimate)
    
    plotly::plot_ly(
      df,
      x = ~estimate,
      y = ~term_label,
      type = "bar",
      orientation = "h",
      error_x = list(
        type = "data",
        array = df$ci_high - df$estimate,
        arrayminus = df$estimate - df$ci_low
      ),
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Coeficiente: %{x:.3f}<br>",
        "IC 95%: [", round(df$ci_low, 3), ", ", round(df$ci_high, 3), "]",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        xaxis = list(title = "Coeficiente estimado"),
        yaxis = list(title = ""),
        margin = list(l = 140, r = 20, t = 10, b = 40)
      )
  })
  

  dev_renew_data <- reactive({

    path_fe_country <- "modelo_panel_efectos_fijos_pais_con_target_cuadrado.csv"
    if (!file.exists(path_fe_country)) return(NULL)
    
    fe_country <- read.csv(path_fe_country, stringsAsFactors = FALSE)
    
    if (!all(c("Entity", "effect_country") %in% names(fe_country))) {
      message("Columnas en efectos fijos país:")
      print(names(fe_country))
      return(NULL)
    }
    

    path_data <- "data.csv"
    if (!file.exists(path_data)) return(NULL)
    
    data_panel <- readr::read_csv(path_data, show_col_types = FALSE)
    if ("...1" %in% names(data_panel)) {
      data_panel <- data_panel %>% dplyr::select(-`...1`)
    }
    
    panel_avg <- data_panel %>%
      dplyr::group_by(Entity, Income_Group) %>%
      dplyr::summarise(
        mean_renewables = mean(Renewables, na.rm = TRUE),
        n_obs = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(mean_renewables))
    
    dev_df <- fe_country %>%
      dplyr::inner_join(panel_avg, by = "Entity") %>%
      dplyr::filter(!is.na(effect_country))
    
    if (nrow(dev_df) == 0) return(NULL)
    
    dev_df
  })
  
  output$dev_renew_plot <- renderPlotly({
    df <- dev_renew_data()
    if (is.null(df)) {
      return(plotly::plotly_empty())
    }
    
    df$Income_Group <- factor(
      df$Income_Group,
      levels = c(
        "Low-income countries",
        "Lower-middle-income countries",
        "Upper-middle-income countries",
        "High-income countries"
      )
    )
    
    plotly::plot_ly(
      df,
      x = ~mean_renewables,
      y = ~effect_country,
      color = ~Income_Group,
      type = "scatter",
      mode = "markers",
      marker = list(size = 9, opacity = 0.8),
      text = ~Entity,
      hovertemplate = paste0(
        "<b>%{text}</b><br>",
        "Renovables promedio: %{x:.1f} %<br>",
        "Efecto fijo del país: %{y:.2f}<br>",
        "Grupo de ingreso: %{marker.color}<extra></extra>"
      )
    ) %>%
  plotly::layout(
    xaxis = list(
      title = "Participación renovable promedio del país (%)",
      titlefont = list(size = 11)
    ),
    yaxis = list(
      title = "Efecto fijo del país en el modelo",
      titlefont = list(size = 8)
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.9   
    ),
    margin = list(
      l = 110,
      r = 20,
      t = 20,
      b = 120   
    )
  )
  })

  
  global_panel_data <- reactive({
  path_data <- "data.csv"
  if (!file.exists(path_data)) return(NULL)
  
  df <- read.csv(path_data, stringsAsFactors = FALSE)
  if ("...1" %in% names(df)) {
    df <- df %>% dplyr::select(-`...1`)
  }
  
  df %>%
    dplyr::mutate(
      Continent = countrycode(
        Entity,
        origin      = "country.name",
        destination = "continent",
        warn        = FALSE
      ),
      
      Continent = dplyr::recode(
        Continent,
        "Africa"   = "África",
        "Americas" = "América",
        "Asia"     = "Asia",
        "Europe"   = "Europa",
        "Oceania"  = "Oceanía",
        .default   = Continent
      )
    ) %>%
    dplyr::filter(nchar(Code) == 3)
})

  

output$global_year_ui <- renderUI({
  df <- global_panel_data()
  if (is.null(df)) return(NULL)
  
  sliderInput(
    "global_top_year_range",
    label = "Periodo para ranking y pendiente",
    min   = min(df$Year, na.rm = TRUE),
    max   = max(df$Year, na.rm = TRUE),
    value = c(
      max(min(df$Year, na.rm = TRUE), min(df$Year, na.rm = TRUE)),
      max(df$Year, na.rm = TRUE)
    ),
    step  = 1,
    sep   = "",
    width = "100%"
  )
})

output$global_top10_plot <- renderPlotly({
  df <- global_panel_data()
  req(df, input$global_top_year_range)
  
  yr <- input$global_top_year_range
  year_ref <- yr[2]
  
  top_df <- df %>%
    dplyr::filter(Year == year_ref) %>%
    dplyr::filter(!is.na(Renewables)) %>%
    dplyr::arrange(dplyr::desc(Renewables)) %>%
    dplyr::slice_head(n = 10)
  
  if (nrow(top_df) == 0) return(plotly::plotly_empty())
  
  top_df <- top_df %>%
    dplyr::mutate(
      Entity = factor(Entity, levels = rev(Entity))
    )
  
  plotly::plot_ly(
    top_df,
    x = ~Renewables,
    y = ~Entity,
    type = "bar",
    orientation = "h",
    marker = list(
      color = ~Renewables,
      colorscale = list(
        c(0, "#a6cee3"),
        c(1, "#1f78b4")
      ),
      showscale = FALSE
    ),
    text = ~paste0(round(Renewables, 1), " %"),
    textposition = "inside",
    insidetextanchor = "middle",
    textfont = list(color = "white", size = 11),
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "Renovables: %{x:.1f} %<br>",
      "Fósiles: ", round(top_df$Fossil_Fuels, 1), " %<br>",
      "Continente: ", top_df$Continent, "<br>",
      "Grupo de ingreso: ", top_df$Income_Group,
      "<extra></extra>"
    )
  ) %>%
    plotly::layout(
      xaxis = list(
        title = "% de energías renovables",
        tickformat = ".0f"
      ),
      yaxis = list(title = ""),
      margin = list(l = 120, r = 20, t = 10, b = 40)
    )
})

  
 
output$global_cont_growth_plot <- renderPlotly({
  df <- global_panel_data()
  req(df, input$global_top_year_range)
  
  yr <- input$global_top_year_range
  
  cont_series <- df %>%
    dplyr::filter(
      Year >= yr[1], Year <= yr[2],
      !is.na(Continent),
      !is.na(Renewables)
    ) %>%
    dplyr::group_by(Continent, Year) %>%
    dplyr::summarise(
      mean_renew = mean(Renewables, na.rm = TRUE),
      .groups    = "drop"
    )
  
  if (nrow(cont_series) == 0) return(plotly::plotly_empty())
  
  growth_df <- cont_series %>%
    dplyr::group_by(Continent) %>%
    dplyr::summarise(
      slope = {
        m <- lm(mean_renew ~ Year)
        unname(coef(m)[2])
      },
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(slope)) %>%
    dplyr::arrange(slope) %>%
    dplyr::mutate(
      Continent = factor(Continent, levels = Continent),
      sign_pos  = slope > 0
    )
  
  if (nrow(growth_df) == 0) return(plotly::plotly_empty())
  
  colors_sign <- c("TRUE" = "#2E7D32", "FALSE" = "#C62828")
  
  plotly::plot_ly(
    growth_df,
    x = ~slope,
    y = ~Continent,
    type = "bar",
    orientation = "h",
    marker = list(color = ~colors_sign[as.character(sign_pos)]),
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "Cambio anual promedio: %{x:.2f} puntos porcentuales/año",
      "<extra></extra>"
    )
  ) %>%
    plotly::layout(
      xaxis = list(
        title = "Cambio anual promedio en % de renovables\n(puntos porcentuales por año)",
        zeroline = TRUE
      ),
      yaxis = list(title = ""),
      margin = list(l = 140, r = 20, t = 10, b = 40)
    )
})


output$global_income_trend_plot <- renderPlotly({
  df <- global_panel_data()
  if (is.null(df)) return(plotly::plotly_empty())
  
  req(input$global_top_year_range)
  yr <- input$global_top_year_range
  
  trend <- df %>%
    dplyr::filter(
      !is.na(Income_Group),
      Year >= yr[1],
      Year <= yr[2]
    ) %>%
    dplyr::group_by(Year, Income_Group) %>%
    dplyr::summarise(
      mean_renew = mean(Renewables, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      Income_Short = dplyr::case_when(
        Income_Group == "Low-income countries"            ~ "Low-income",
        Income_Group == "Lower-middle-income countries"   ~ "Lower-middle-income",
        Income_Group == "Upper-middle-income countries"   ~ "Upper-middle-income",
        Income_Group == "High-income countries"           ~ "High-income",
        TRUE                                              ~ Income_Group
      )
    )
  
  if (nrow(trend) == 0) return(plotly::plotly_empty())
  
  col_income <- c(
    "Low-income"           = "#66C2A5",
    "Lower-middle-income"  = "#FC8D62",
    "Upper-middle-income"  = "#8DA0CB",
    "High-income"          = "#E78AC3"
  )
  
  plotly::plot_ly(
    trend,
    x          = ~Year,
    y          = ~mean_renew,
    color      = ~Income_Short,
    colors     = col_income,
    type       = "scatter",
    mode       = "lines+markers",
    line       = list(width = 2),
    marker     = list(size = 4),
    customdata = ~Income_Short,        # para el hover (nombre del grupo)
    hovertemplate = paste0(
      "<b>%{customdata}</b><br>",
      "Año: %{x}<br>",
      "Media renovables: %{y:.1f} %<extra></extra>"
    )
  ) %>%
    plotly::layout(
      xaxis = list(
        title = "",
        dtick = 4
      ),
      yaxis = list(
        title     = "% de energía renovable (media anual)",
        rangemode = "tozero",
        titlefont = list(size = 9),
        tickfont  = list(size = 9)
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.5
      ),
      margin = list(l = 90, r = 20, t = 10, b = 70)
    )
})



  
output$global_region_disp_plot <- renderPlotly({
  df <- global_panel_data()
  if (is.null(df)) return(plotly::plotly_empty())
  
  req(input$global_top_year_range)
  yr <- input$global_top_year_range
  
  cont_year <- df %>%
    dplyr::filter(
      !is.na(Continent),
      Year >= yr[1],
      Year <= yr[2]
    ) %>%
    dplyr::group_by(Continent, Year) %>%
    dplyr::summarise(
      mean_renew   = mean(Renewables, na.rm = TRUE),
      n_countries  = dplyr::n(),
      .groups      = "drop"
    ) %>%
    dplyr::filter(n_countries >= 3)
  
  if (nrow(cont_year) == 0) return(plotly::plotly_empty())
  
  plotly::plot_ly(
    cont_year,
    x = ~Continent,
    y = ~mean_renew,
    color = ~Continent,
    type = "box",
    boxpoints = "outliers",
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "Media anual de renovables: %{y:.1f} %<extra></extra>"
    )
  ) %>%
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(
        title = "% renovable (promedio anual por continente)",
        titlefont = list(size = 8)
      ),
      showlegend = FALSE,
      margin = list(l = 80, r = 20, t = 10, b = 60)
    )
})

  

  energy_server_logic("solar", "Solar", er_clean, world_sf, er_world)
  energy_server_logic("wind",  "Wind",  er_clean, world_sf, er_world)
  energy_server_logic("hydro", "Hydro", er_clean, world_sf, er_world)
  energy_server_logic("bio",   "Bio",   er_clean, world_sf, er_world)
  
  # Bloque global de share renovables
  global_share_server(
    "global_share",
    data_full = er_clean,
    world_geo = world_sf,
    er_world  = er_world,
    col_name  = global_share_col,
    color     = "#2c3e50"
  )
}

shinyApp(ui, server)