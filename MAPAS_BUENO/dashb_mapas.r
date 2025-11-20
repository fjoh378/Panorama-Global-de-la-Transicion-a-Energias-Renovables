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

# ==============================================================================
# 1. CONFIGURACIÓN TÉCNICA, COLORES Y PALETAS
# ==============================================================================

# ==============================================================================
source("funcion_main.R")
datos <- read.csv("data.csv")


# =============================================================================

# Colores principales (Líneas y Gráficas)
cols_pal <- list(
  Solar = "#FFC107", # Amber
  Wind  = "#00BCD4", # Cyan
  Hydro = "#1565C0", # Blue Dark
  Bio   = "#2E7D32", # Green Dark
  Geo   = "#D84315"  # Deep Orange
)

# AJUSTE DE VISIBILIDAD: Subimos la opacidad de 0.2 a 0.6 para colores vibrantes
to_rgba <- function(hex, alpha = 0.6) {
  rgb_vals <- col2rgb(hex)
  paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
}

# CONFIGURACIÓN MAESTRA
cols_def <- list(
  Solar = list(
    gen = "Solar.generation...TWh..modern.renewable.energy.consumption.",
    share = "Solar.....electricity..share.electricity.solar.",
    color = cols_pal$Solar,
    fill  = to_rgba(cols_pal$Solar), # Ahora usa 0.6 por defecto
    map_palette = "YlOrRd",
    icon = "sun",
    metric_label = "Generación Eléctrica Solar",
    desc = "Electricidad producida mediante tecnología fotovoltaica (PV). El dato corresponde a la generación eléctrica neta (output), no a la energía primaria equivalente."
  ),
  Wind = list(
    gen = "Wind.generation...TWh..modern.renewable.energy.consumption.",
    share = "Wind.....electricity..share.electricity.wind.",
    color = cols_pal$Wind,
    fill  = to_rgba(cols_pal$Wind),
    map_palette = "PuBu",
    icon = "wind",
    metric_label = "Generación Eléctrica Eólica",
    desc = "Electricidad generada por el viento. Incluye la producción de parques eólicos tanto en tierra (onshore) como en mar (offshore)."
  ),
  Hydro = list(
    gen = "Hydro.generation...TWh..modern.renewable.energy.consumption.",
    share = "Hydro.....electricity..share.electricity.hydro.",
    color = cols_pal$Hydro,
    fill  = to_rgba(cols_pal$Hydro),
    map_palette = "GnBu",
    icon = "water",
    metric_label = "Generación Hidroeléctrica",
    desc = "Electricidad generada por centrales hidroeléctricas (de embalse y de pasada). Excluye la energía hidroeléctrica de bombeo puro."
  ),
  Bio = list(
    gen = "Biofuels.production...TWh..biofuel.production.",
    share = NULL, 
    color = cols_pal$Bio,
    fill  = to_rgba(cols_pal$Bio),
    map_palette = "YlGn",
    icon = "leaf",
    metric_label = "Producción de Biocombustibles",
    desc = "Producción de combustibles líquidos (bioetanol y biodiesel). Nota: La unidad TWh aquí representa el contenido energético del combustible, no electricidad generada."
  ),
  Geo = list(
    gen = "Other.renewables..including.geothermal.and.biomass..electricity.generation...TWh..modern.renewable.energy.consumption.",
    share = NULL, 
    color = cols_pal$Geo,
    fill  = to_rgba(cols_pal$Geo),
    map_palette = "OrRd",
    icon = "fire",
    metric_label = "Geotérmica, Biomasa y Otros",
    desc = "Electricidad generada a partir de biomasa sólida (madera/residuos), energía geotérmica y tecnologías marinas (mareomotriz/undimotriz)."
  )
)

# ==============================================================================
# 2. CARGA Y PROCESAMIENTO DE DATOS
# ==============================================================================

er_data <- read.csv("cruce_er_cf.csv")
pred_data <- read.csv("predicciones_world_fossil_renew_lstm_train_test.csv")



# Limpieza y Geocodificación
er_clean <- er_data %>%
  mutate(
    iso3 = countrycode(Entity, origin = "country.name", destination = "iso3c", warn = FALSE),
    Is_Country = !is.na(iso3)
  )

# Datos Mundiales
if ("World" %in% er_clean$Entity) {
  er_world <- er_clean %>% filter(Entity == "World")
} else {
  er_world <- er_clean %>%
    filter(Is_Country) %>%
    group_by(Year) %>%
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>%
    mutate(Entity = "World")
}

# Geometría del Mundo
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% select(iso_a3, geometry)

# ==============================================================================
# 3. INTERFAZ DE USUARIO (UI)
# ==============================================================================

energy_block_ui <- function(id, title, color, icon_name, description, has_share, metric_label_text) {
  ns <- NS(id)
  
  div(class = "card shadow-sm mb-5 border-0",
      div(class = "card-header bg-white py-3", style=paste0("border-top: 5px solid ", color, ";"),
          h3(icon(icon_name), title, style = paste0("color:", color, "; font-weight: 800; margin: 0;")),
          p(class="text-muted mb-0 mt-2 small", style="text-align: justify;", description)
      ),
      div(class = "card-body bg-light",
          layout_columns(
            col_widths = c(3, 9),
            # Columna Izquierda: Filtros
            div(class="p-3 bg-white rounded shadow-sm h-100",
                h5("Filtros", class="border-bottom pb-2"),
                uiOutput(ns("slider_ui"))
            ),
            # Columna Derecha: Mapa
            div(class="bg-white rounded shadow-sm p-3",
                tabsetPanel(id = ns("tabs_metric"),
                            tabPanel(paste(metric_label_text, "(TWh)"), value = "gen",
                                     br(), leafletOutput(ns("map_gen"), height = "450px")),
                            if(has_share) {
                              tabPanel("Participación (%)", value = "share",
                                       br(), leafletOutput(ns("map_share"), height = "450px"))
                            } else NULL
                )
            )
          ),
          br(),
          # Fila de Gráficas
          layout_columns(
            col_widths = c(4, 4, 4),
            card(card_header("Tendencia Mundial"), plotlyOutput(ns("plot_world"), height = "250px")),
            card(card_header("Top 5 (Trayectoria Histórica)"), plotlyOutput(ns("plot_top5"), height = "250px")),
            card(card_header(textOutput(ns("country_title"))), plotlyOutput(ns("plot_country"), height = "250px"))
          )
      )
  )
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  tags$head(tags$style(HTML("
    .leaflet-container { background: #f0f2f5; } 
    .legend { font-size: 11px; padding: 8px; background: rgba(255,255,255,0.95); border-radius: 4px; border: 1px solid #eee; }
    .card-header { font-weight: bold; font-size: 1rem; color: #2c3e50; background-color: #fff; }
    .leaflet-tooltip { font-family: sans-serif; font-size: 12px; font-weight: bold; color: #333; background: rgba(255,255,255,0.9); border: 1px solid #ccc; border-radius: 4px;}
  "))),
  
  div(class = "container-fluid py-5 text-center bg-white shadow-sm mb-4",
      h1("Monitor Global de Transición Energética", style="font-weight: 900; color: #2c3e50;"),
      p("Análisis integral de la matriz energética mundial: Historia y Proyecciones (LSTM).", class="text-muted")
  ),
  
  # --- BLOQUE 1: PANORAMA GLOBAL ---
  div(class = "container-fluid px-4 mb-5",
      div(class = "card shadow-sm border-0",
          div(class = "card-header bg-white py-3", style="border-top: 5px solid #2c3e50;",
              h3(icon("chart-line"), "Panorama Global y Proyecciones", style = "color: #2c3e50; font-weight: 800; margin: 0;")
          ),
          div(class = "card-body",
              tabsetPanel(
                tabPanel("Historia: Comparativa por Fuente (TWh)", br(),
                         sidebarLayout(
                           sidebarPanel(width = 3, h5("Línea de Tiempo"), uiOutput("slider_global_ui")),
                           mainPanel(width = 9, plotlyOutput("global_plot", height = "450px"))
                         )
                ),
                tabPanel("Proyección IA: Fósiles vs Renovables", br(),
                         div(class="alert alert-light border", icon("robot"), 
                             strong("Modelo LSTM:"), " Tendencia proyectada de la participación en energía primaria."),
                         plotlyOutput("forecast_plot", height = "500px")
                )
              )
          )
      )
  ),
  
  # --- BLOQUES TEMÁTICOS ---
  div(class = "container-fluid px-4",
      energy_block_ui("solar", "Energía Solar", cols_def$Solar$color, "sun", cols_def$Solar$desc, TRUE, cols_def$Solar$metric_label),
      energy_block_ui("wind", "Energía Eólica", cols_def$Wind$color, "wind", cols_def$Wind$desc, TRUE, cols_def$Wind$metric_label),
      energy_block_ui("hydro", "Energía Hidroeléctrica", cols_def$Hydro$color, "water", cols_def$Hydro$desc, TRUE, cols_def$Hydro$metric_label),
      energy_block_ui("bio", "Biocombustibles", cols_def$Bio$color, "leaf", cols_def$Bio$desc, FALSE, cols_def$Bio$metric_label),
      energy_block_ui("geo", "Geotérmica y Otros", cols_def$Geo$color, "fire", cols_def$Geo$desc, FALSE, cols_def$Geo$metric_label)
  ),
  
  # --- CITA OFICIAL ---
  div(class="container-fluid py-4 mt-5 bg-light text-center border-top",
      p(class="mb-1 text-muted small", style="font-weight: 600;", 
        "Información obtenida de:"),
      p(class="text-muted small", style="font-style: italic;", 
        "Ember (2025); Energy Institute - Statistical Review of World Energy (2025) – with major processing by Our World in Data")
  ),
  # ----------------------------------------------------------------------------------------------------------------------------
  # Añadiendo la parte del otro dash
  # ----------------------------------------------------------------------------------------------------------------------------
   tags$h3("Vision global de las energias renovables"),
  sidebarLayout(
    sidebarPanel(
        h3("Filtros"),
        #Para seleccionar el año
        numericInput( "anio", "Año", value = 2024, min = 2000, max = 2024 ),
        #Para seleccionar que medida
        selectizeInput( "medida", "Selecciona medidas descriptivas", list("Media" = "mean", "Desviacion" = "sd"), multiple = FALSE )
    ),

    mainPanel(
      #Graficas que hiciero Diego y Brayan
      fluidRow(
        column(6, plotOutput("graf1")),
        column(6, plotOutput("graf2"))
    ),

    fluidRow(
        column(6, plotOutput("graf3")),
        column(6, plotOutput("graf4"))
    )
    
  )


)
 # ----------------------------------------------------------------------------------------------------------------------------

)

# ==============================================================================
# 4. SERVIDOR (LÓGICA)
# ==============================================================================

energy_server_logic <- function(id, tech_key, data_full, world_geo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conf <- cols_def[[tech_key]]
    selected_iso <- reactiveVal("MEX") 
    
    # Actualizar selección de país
    observeEvent(input$map_gen_shape_click, { if(!is.null(input$map_gen_shape_click$id)) selected_iso(input$map_gen_shape_click$id) })
    observeEvent(input$map_share_shape_click, { if(!is.null(input$map_share_shape_click$id)) selected_iso(input$map_share_shape_click$id) })
    
    output$country_title <- renderText({
      req(selected_iso())
      nm <- data_full %>% filter(iso3 == selected_iso()) %>% pull(Entity) %>% unique()
      if(length(nm) == 0) return("Seleccione un País")
      paste("Evolución Histórica:", nm[1])
    })
    
    # Slider con Play
    output$slider_ui <- renderUI({
      sliderInput(ns("year"), "Año:", min = min(data_full$Year), max = max(data_full$Year), 
                  value = max(data_full$Year), step = 1, sep = "", 
                  animate = animationOptions(interval = 800, loop = FALSE))
    })
    
    # MAPA (PANELES Y PALETAS)
    render_custom_map <- function(type) {
      renderLeaflet({
        req(input$year)
        col_name <- if(type == "gen") conf$gen else conf$share
        unit <- if(type == "gen") "TWh" else "%"
        
        map_dat <- data_full %>% filter(Year == input$year, Is_Country) %>% select(iso3, Entity, Val = all_of(col_name))
        geo <- world_geo %>% left_join(map_dat, by = c("iso_a3" = "iso3"))
        vals <- geo$Val
        
        # Paleta Temática Dinámica
        if(type == "gen") {
          bins <- c(0, 1, 10, 50, 100, 500, Inf)
          pal <- colorBin(conf$map_palette, domain = vals, bins = bins, na.color = "#ffffff") 
        } else {
          bins <- c(0, 1, 5, 10, 20, 50, 100)
          pal <- colorBin(conf$map_palette, domain = vals, bins = bins, na.color = "#ffffff")
        }
        
        tooltip_content <- sprintf("<strong>%s</strong><br/>%s", geo$Entity,
                                   ifelse(is.na(geo$Val), "Sin Información", paste0(format(round(geo$Val, 2), big.mark=","), " ", unit))) %>% lapply(HTML)
        
        leaflet(geo) %>%
          setMaxBounds(-180, -90, 180, 90) %>%
          addMapPane("fondo", zIndex = 400) %>%
          addMapPane("datos", zIndex = 450) %>%
          addMapPane("etiquetas", zIndex = 500) %>%
          
          addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(noWrap = TRUE, pane = "fondo")) %>%
          
          addPolygons(
            fillColor = ~pal(Val), weight = 1, color = "white", opacity = 1, fillOpacity = 0.85,
            layerId = ~iso_a3,
            options = pathOptions(pane = "datos"),
            highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = TRUE),
            label = tooltip_content,
            labelOptions = labelOptions(direction = "auto")
          ) %>%
          
          addProviderTiles("CartoDB.PositronOnlyLabels", options = providerTileOptions(noWrap = TRUE, pane = "etiquetas")) %>%
          setView(0, 20, 1.5) %>%
          addLegend(pal = pal, values = vals, title = unit, position = "bottomright", na.label = "Sin Info")
      })
    }
    
    output$map_gen <- render_custom_map("gen")
    output$map_share <- render_custom_map("share")
    
    # GRÁFICA MUNDIAL (COLORES VIBRANTES 0.6 ALPHA)
    output$plot_world <- renderPlotly({
      active_tab <- if(is.null(input$tabs_metric)) "gen" else input$tabs_metric
      col_name <- if(active_tab == "gen") conf$gen else conf$share
      unit <- if(active_tab == "gen") "TWh" else "%"
      
      w_dat <- er_world %>% select(Year, Val = all_of(col_name)) %>% arrange(Year)
      
      plot_ly(w_dat, x = ~Year, y = ~Val, type = 'scatter', mode = 'lines+markers', 
              fill = 'tozeroy', fillcolor = conf$fill, 
              marker = list(size = 3, color = conf$color), line = list(color = conf$color, width = 2.5),
              hovertemplate = paste0("<b>%{x}</b>: %{y:,.1f} ", unit, "<extra></extra>")) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit))
    })
    
    # TOP 5
    output$plot_top5 <- renderPlotly({
      req(input$year)
      active_tab <- if(is.null(input$tabs_metric)) "gen" else input$tabs_metric
      col_name <- if(active_tab == "gen") conf$gen else conf$share
      unit <- if(active_tab == "gen") "TWh" else "%"
      
      top_iso <- data_full %>% filter(Year == input$year, Is_Country) %>% arrange(desc(.data[[col_name]])) %>% head(5) %>% pull(iso3)
      if(length(top_iso) == 0) return(plotly_empty())
      
      top_hist <- data_full %>% filter(iso3 %in% top_iso) %>% select(Year, Entity, Val = all_of(col_name)) %>% arrange(Year)
      
      plot_ly(top_hist, x = ~Year, y = ~Val, color = ~Entity, colors = "Set1",
              type = 'scatter', mode = 'lines+markers', marker = list(size=3), line = list(width=2),
              hovertemplate = paste0("<b>%{x}</b><br>%{y:,.1f} ", unit, "<extra></extra>")) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit), legend = list(orientation="h", y=-0.2))
    })
    
    # PAÍS (COLORES VIBRANTES)
    output$plot_country <- renderPlotly({
      req(selected_iso())
      active_tab <- if(is.null(input$tabs_metric)) "gen" else input$tabs_metric
      col_name <- if(active_tab == "gen") conf$gen else conf$share
      unit <- if(active_tab == "gen") "TWh" else "%"
      
      c_dat <- data_full %>% filter(iso3 == selected_iso()) %>% arrange(Year)
      if(nrow(c_dat) == 0) return(plotly_empty())
      
      plot_ly(c_dat, x = ~Year, y = ~get(col_name), type = 'scatter', mode = 'lines+markers',
              fill = 'tozeroy', fillcolor = conf$fill,
              line = list(color = conf$color, width = 2.5), marker = list(size = 4, color = conf$color),
              hovertemplate = paste0("<b>%{x}</b>: %{y:,.2f} ", unit, "<extra></extra>")) %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = unit))
    })
  })
}

server <- function(input, output, session) {
  
  # PREDICCIÓN
  output$forecast_plot <- renderPlotly({
    p_dat <- pred_data %>% mutate(Energy_Type = ifelse(Type == "Fossil", "Fósiles", "Renovables"))
    last_real_f <- p_dat %>% filter(Energy_Type=="Fósiles", !is.na(Real)) %>% slice_max(Year, n=1)
    last_real_r <- p_dat %>% filter(Energy_Type=="Renovables", !is.na(Real)) %>% slice_max(Year, n=1)
    cut_year <- last_real_f$Year
    
    stitch_f <- data.frame(Year=last_real_f$Year, Type="Fossil", Real=NA, Pred_LSTM=last_real_f$Real, Energy_Type="Fósiles")
    stitch_r <- data.frame(Year=last_real_r$Year, Type="Renewable", Real=NA, Pred_LSTM=last_real_r$Real, Energy_Type="Renovables")
    
    final_pred <- bind_rows(p_dat, stitch_f, stitch_r) %>% arrange(Year)
    cols_pred <- c("Fósiles" = "#37474F", "Renovables" = "#2E7D32")
    
    plot_ly(final_pred) %>%
      add_trace(x = ~Year, y = ~Real, color = ~Energy_Type, colors = cols_pred,
                type = 'scatter', mode = 'lines+markers', marker = list(size=4), line = list(width=3),
                name = ~paste(Energy_Type, "(Hist)")) %>%
      add_trace(x = ~Year, y = ~Pred_LSTM, color = ~Energy_Type, colors = cols_pred,
                type = 'scatter', mode = 'lines+markers', marker = list(size=4), 
                line = list(width=3, dash='dot'),
                name = ~paste(Energy_Type, "(Pred)"), showlegend = FALSE) %>%
      layout(
        yaxis = list(title = "Participación (%)"), xaxis = list(title = "Año"), hovermode = "x unified",
        shapes = list(list(type = "line", x0 = cut_year, x1 = cut_year, y0 = 0, y1 = 1, yref = "paper",
                           line = list(color = "gray", width = 1, dash = "dash"))),
        annotations = list(list(x = cut_year, y = 0.05, text = "Inicio Proyección", showarrow = FALSE, xanchor = "right", yref="paper", font=list(size=10, color="gray")))
      )
  })
  
  # HISTORIA GLOBAL (FILTRO TWh != 0)
  global_long <- reactive({
    er_world %>%
      select(Year, Solar = cols_def$Solar$gen, Eólica = cols_def$Wind$gen, Hidro = cols_def$Hydro$gen, 
             Bio = cols_def$Bio$gen, Geo = cols_def$Geo$gen) %>%
      pivot_longer(cols = -Year, names_to = "Fuente", values_to = "TWh") %>%
      filter(TWh != 0) %>% arrange(Year)
  })
  
  # AJUSTE: Slider Global con valor por defecto al MÁXIMO
  output$slider_global_ui <- renderUI({
    dat <- global_long()
    sliderInput("anim_year", "Año:", min = min(dat$Year), max = max(dat$Year), 
                value = max(dat$Year), # <--- POR DEFECTO EL MÁS RECIENTE
                step = 1, sep = "", animate = animationOptions(interval = 600, loop = FALSE))
  })
  
  output$global_plot <- renderPlotly({
    req(input$anim_year)
    dat <- global_long() %>% filter(Year <= input$anim_year) %>% arrange(Year)
    
    cols_g <- c("Solar"=cols_def$Solar$color, "Eólica"=cols_def$Wind$color, "Hidro"=cols_def$Hydro$color, "Bio"=cols_def$Bio$color, "Geo"=cols_def$Geo$color)
    fills_g <- c("Solar"=cols_def$Solar$fill, "Eólica"=cols_def$Wind$fill, "Hidro"=cols_def$Hydro$fill, "Bio"=cols_def$Bio$fill, "Geo"=cols_def$Geo$fill)
    
    p <- plot_ly()
    for(src in names(cols_g)) {
      d_src <- dat %>% filter(Fuente == src)
      if(nrow(d_src) > 0) {
        p <- p %>% add_trace(data = d_src, x = ~Year, y = ~TWh, type = 'scatter', mode = 'lines+markers',
                             name = src, line = list(color = cols_g[[src]], width = 2.5),
                             fill = 'tozeroy', fillcolor = fills_g[[src]], # Relleno vibrante (0.6)
                             marker = list(size = 3, color = cols_g[[src]]),
                             hovertemplate = "%{y:,.0f} TWh<extra></extra>")
      }
    }
    p %>% layout(yaxis = list(title = "TWh"), legend = list(orientation = "h", y = 1.1))
  })
  
  energy_server_logic("solar", "Solar", er_clean, world_sf)
  energy_server_logic("wind", "Wind", er_clean, world_sf)
  energy_server_logic("hydro", "Hydro", er_clean, world_sf)
  energy_server_logic("bio", "Bio", er_clean, world_sf)
  energy_server_logic("geo", "Geo", er_clean, world_sf)


     # Llama a tu funcion_main usando los inputs
  graficas <- reactive({
    funcion_main(
      datos,
      año1   = input$anio,     # viene del numericInput
      medida = input$medida    # viene del selectInput
    )
  })

  #Haciendo la primera grafica:
  output$graf1 <- renderPlot({
    graficas()[[1]]            
  })

  # Y la segunda:
  output$graf2 <- renderPlot({
    graficas()[[2]]            
  })

  output$graf3 <- renderPlot({
    graficas()[[3]]            
  })
  
  output$graf4 <- renderPlot({
    graficas()[[4]]            
  })




}

runApp(shinyApp(ui, server), launch.browser = TRUE)
