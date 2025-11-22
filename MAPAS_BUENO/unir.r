

# Pega esto afuera ---------------------- lo uso para calcular 2 indicadores los otros dos indicarores filtro los datos dentro del server 
# ------------  INDICADORES -----------------------------------------------------------------------------------------------------------------------------------

# Cálculo de total renovables por AÑO (sumando todos los países)
cruce_er_por_anio <- er_data |>
   dplyr::filter(entity == "World") |>
  mutate(
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
  group_by(year) |>
  summarise(total_renovables_twh = sum(total_renovables, na.rm = TRUE))

# Año inicial y valor base (referencia) #Segundo indicador
primer_anio <- min(cruce_er_por_anio$year, na.rm = TRUE)

base_total_renovables <- cruce_er_por_anio |>
  filter(year == primer_anio) |>
  pull(total_renovables_twh)
# Tercer indicador 

# -------------------------------------------------------------------------------------------------------------------------------------------------------------














# ------------------------------------------------  UI para los indicadores  ----------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .ind-card {
        background-color: #ffffff;
        border-radius: 0.75rem;
        box-shadow: 0 0.25rem 0.5rem rgba(0,0,0,0.08);
        padding: 1rem 1.25rem;
        margin-bottom: 1rem;
        text-align: left;
        border-left: 5px solid #0d6efd;
      }
      .ind-card h6 {
        font-size: 0.8rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: #6c757d;
        margin-bottom: 0.35rem;
      }
      .ind-main-value {
        font-size: 1.9rem;
        font-weight: 700;
        margin: 0;
      }
      .ind-sub-value {
        font-size: 0.95rem;
        color: #6c757d;
        margin-top: 0.15rem;
      }
      .ind-icon {
        font-size: 1.4rem;
        margin-right: 0.5rem;
        opacity: 0.85;
      }
      /* Colores distintos por tarjeta */
      .ind-primary  { border-left-color: #0d6efd; }
      .ind-success  { border-left-color: #198754; }
      .ind-warning  { border-left-color: #ffc107; }
      .ind-info     { border-left-color: #0dcaf0; }
    "))
  ),

















# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Aqqui va toda la  estruycutra para desplejar los indicadores
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
   # ====== INDICADORES DEBAJO DE LA GRÁFICA ======
          fluidRow(
            column(
              width = 3,
              div(class = "ind-card ind-primary",
                  h6(icon("leaf", class = "ind-icon"), "Total renovables (TWh) por año"),
                  p(class = "ind-main-value", textOutput("ind_total_renovables")),
                  p(class = "ind-sub-value", "Incluye hidro, eólica, solar y geotérmica")
              )
            ),
            column(
              width = 3,
              div(class = "ind-card ind-success",
                  h6(icon("chart-line", class = "ind-icon"), "Crecimiento actual vs inicio"),
                  p(class = "ind-main-value", textOutput("ind_crecimiento_vs_inicio")),
                  p(class = "ind-sub-value", "Variación respecto al primer año de la serie")
              )
            ),
            column(
              width = 3,
              div(class = "ind-card ind-warning",
                  h6(icon("layer-group", class = "ind-icon"), "Total renovables (TWh) acumulado"),
                  p(class = "ind-main-value", textOutput("ind_total_renovablesPorAnio")),
                  p(class = "ind-sub-value", "Suma histórica hasta el año seleccionado")
              )
            ),
            column(
              width = 3,
              div(class = "ind-card ind-info",
                  h6(icon("percent", class = "ind-icon"), "Crecimiento vs año anterior"),
                  p(class = "ind-main-value", textOutput("comparacion_anios")),
                  p(class = "ind-sub-value", "Tasa anual de crecimiento interanual")
              )
            )
          )



# -------------------------------------------------------------------------------------------------------------------------------------------------------------
















####-----Indicadora para la grafica -----------------------------------------------------------------------------------------------------
# Logica que va en el server
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Filtrar por año seleccionado
  datos_filtrados <- reactive({
    req(input$anim_year)

    cruce_er_por_anio |>
      filter(year == input$anim_year)
  })

  # ===== INDICADOR: Total renovables globales (TWh) =====
  output$ind_total_renovables <- renderText({
    df <- datos_filtrados()

    if (nrow(df) == 0) {
      return("Sin datos")
    }

    valor <- df$total_renovables_twh[1]

    # Formato final
    valor_formateado <- format(round(valor, 1), big.mark = ",")
    paste0(valor_formateado, " TWh")
  })


# Segundo indicador:
 output$ind_crecimiento_vs_inicio <- renderText({
    df <- datos_filtrados()

    # Validaciones básicas
    if (nrow(df) == 0 || is.na(base_total_renovables) || base_total_renovables == 0) {
      return("Sin datos")
    }

    valor_actual <- df$total_renovables_twh[1]

    # Diferencia absoluta y porcentaje vs año inicial
    dif_abs <- valor_actual - base_total_renovables
    dif_pct <- (valor_actual - base_total_renovables) / base_total_renovables * 100

    dif_abs_fmt <- format(round(dif_abs, 1), big.mark = ",")
    dif_pct_fmt <- format(round(dif_pct, 1), big.mark = ",")

    # Ejemplo: "+10,045.6 TWh (+22.3 %)"
    signo <- ifelse(dif_abs >= 0, "+", "")

    paste0(signo, dif_abs_fmt, " TWh (", signo, dif_pct_fmt, " %)")
  })

  # Tercer indicador:
   # Filtramos solo el año seleccionado


  # INDICADOR: producción renovable total SOLO de ese año
output$ind_total_renovablesPorAnio <- renderText({

    resultadoAño <- er_data |>
      filter(entity == "World" & year <= input$anim_year) |>
       mutate(
          # SOLO producción renovable DE ESE AÑO
          total_renovables_anio =
            electricity_from_wind_t_wh__modern_renewable_prod +
            electricity_from_hydro_t_wh__modern_renewable_prod +
            electricity_from_solar_t_wh__modern_renewable_prod +
            other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ) |>
      select( total_renovables_anio) |>
      summarise(total_renovables_anio = sum(total_renovables_anio, na.rm = TRUE))






  # Aseguramos que sea UN solo string
  paste0(round(resultadoAño[["total_renovables_anio"]], 2), " TWh")
})

output$comparacion_anios <- renderText({
    # Sacando la suma del primer año actual
    primerfiltro <- er_data |>
      filter(entity == "World" & year == input$anim_year ) |>
      mutate(
        total_renovables_anio_inicio =
          electricity_from_wind_t_wh__modern_renewable_prod +
          electricity_from_hydro_t_wh__modern_renewable_prod +
          electricity_from_solar_t_wh__modern_renewable_prod +
          other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ) |>
      select( total_renovables_anio_inicio) 
  # Sacando la suma del año anterior al seleccionado 
    segundofiltro <- er_data |>
      filter(entity == "World" &   year == input$anim_year - 1 ) |>
      mutate(
        total_renovables_anio_final =
          electricity_from_wind_t_wh__modern_renewable_prod +
          electricity_from_hydro_t_wh__modern_renewable_prod +
          electricity_from_solar_t_wh__modern_renewable_prod +
          other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ) |>
      select( total_renovables_anio_final)

    # Aplicando la suma
    total =   ((primerfiltro[["total_renovables_anio_inicio"]] - segundofiltro[["total_renovables_anio_final"]]) / segundofiltro[["total_renovables_anio_final"]]) * 100

    print(paste0(primerfiltro[["total_renovables_anio_inicio"]] , "  primer filtro"))
    print(paste0(segundofiltro[["total_renovables_anio_final"]] , "  segundo filtro"))

    print(total)
    paste0(round(total, 2), " %")

})


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#-----------------------------------------------------------------------------------------------------------------



