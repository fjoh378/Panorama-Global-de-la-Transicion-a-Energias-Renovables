
#Donda cargo mis datos y mi funcion main
# ==============================================================================
source("funcion_main.R")
datos <- read.csv("data.csv")


# =============================================================================



# Div que muestra todo lo principal 

  ##-------------------------------------------------------------------------------------------------------------------------------------------------------
  ##-------------------------------------------------------------------------------------------------------------------------------------------------------
  ##-------------------------------------------------------------------------------------------------------------------------------------------------------
div(class = "container-fluid px-4 mb-5",
    div(class = "card shadow-sm border-0",

        # ---------------------- TÍTULO QUE TÚ TENÍAS ----------------------
        div(class = "card-header bg-white py-3",
            style="border-top: 5px solid #2c3e50;",
            h3(icon("chart-line"),
               "Panorama Global y Proyecciones",
               style = "color: #2c3e50; font-weight: 800; margin: 0;")
        ),
        # ------------------------------------------------------------------

        div(class = "card-body",

            tabsetPanel(

                # ==========================================================
                # TAB 1 - HISTORIA GLOBAL
                # ==========================================================
                tabPanel("Historia: Comparativa por Fuente (TWh)", br(),

                    fluidRow(
                        column(12,
                               div(class="d-flex align-items-center bg-light p-3 rounded mb-3",
                                   strong("Línea de Tiempo: ", class="me-3"),
                                   div(style="flex-grow: 1;",
                                       uiOutput("slider_global_ui")
                                   )
                               )
                        )
                    ),

                    fluidRow(
                        column(12,
                               plotlyOutput("global_plot", height = "500px")
                        )
                    )
                ),

                # ==========================================================
                # TAB 2 - PROYECCIÓN IA
                # ==========================================================
                tabPanel("Proyección IA: Fósiles vs Renovables", br(),

                    div(class="alert alert-light border",
                        icon("robot"),
                        strong("Modelo LSTM:"),
                        " Tendencia proyectada de la participación en energía primaria."
                    ),

                    fluidRow(
                        column(12,
                               plotlyOutput("forecast_plot", height = "500px")
                        )
                    )
                ),

                # ==========================================================
                # TAB 3 - VISIÓN GLOBAL + SIDEBAR
                # ==========================================================
                tabPanel("Resumen Global y Tendencias por Región",

                    fluidRow(

                        # ---- SIDEBAR ----
                        column(3,
                               h3("Filtros"),
                               numericInput("anio", "Año",
                                            value = 2024,
                                            min = 2000,
                                            max = 2024),
                               selectizeInput("medida",
                                              "Selecciona medidas descriptivas",
                                              list("Media" = "mean",
                                                   "Desviación" = "sd"),
                                              multiple = FALSE)
                        ),

                        # ---- GRÁFICAS ----
                        column(9,
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
                )

            ) # cierre tabsetPanel
        ) # cierre card-body
    ) # cierre card
) # cierre container

##-------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------------------------------------------------------------





# Esto va en la parte del server


### ------------------------------------------------------------------------------------------------------------
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

### ------------------------------------------------------------------------------------------------------------