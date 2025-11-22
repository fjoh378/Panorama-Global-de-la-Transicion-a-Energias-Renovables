library(readr)
library(shiny)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(sf)
library(bslib)
library(countrycode)
library(shinyWidgets)


##############################################################
#                                                            #
#Consumo de energía primaria procedente de fuentes renovables#
#                                                            #
##############################################################

rse_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/renewable-share-energy.csv")
rse_data = rse_data %>% select(-Code)
names(rse_data)[c(3)] = c("Renewables")

rse_data$iso3 = countrycode(rse_data$Entity, origin = "country.name", destination = "iso3c")
rse_data = rse_data[!is.na(rse_data$iso3), ]
rse_data = st_drop_geometry(rse_data)

world = ne_countries(scale = "medium", returnclass = "sf")

world_data = left_join(world, rse_data, by = c("iso_a3_eh" = "iso3"))
world_data = world_data[!is.na(world_data$Renewables), ]
world_data = st_drop_geometry(world_data)

ui <- fluidPage(
  titlePanel("Mapa de Energía Renovable por País"),
  tagList(
    leafletOutput("mapa", height = 600),
    sliderInput("anio", "Selecciona el año:",
                min = min(world_data$Year),
                max = max(world_data$Year),
                value = max(world_data$Year),
                step = 1,
                sep = "",
                animate = FALSE)
  )
)

server = function(input, output, session) {
  datos_filtrados = reactive({
    world_data %>%
      filter(Year == input$anio) %>%
      mutate(iso3 = countrycode(Entity, origin = "country.name", destination = "iso3c"))
  })
  
  mapa_datos = reactive({
    world = ne_countries(scale = "medium", returnclass = "sf")
    left_join(world, datos_filtrados(), by = c("iso_a3_eh" = "iso3"))
  })
  
  output$mapa <- renderLeaflet({
    datos <- mapa_datos()
    pal <- colorNumeric("YlGnBu", domain = datos$Renewables, na.color = "transparent")
    
    leaflet(datos) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Renewables),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~paste(Entity, ": ", round(Renewables, 1), "%"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Renewables, title = paste("Renovables (%) -", input$anio))
  })
}


shinyApp(ui, server)


############################################################
#                                                          #
#Producción de electricidad a partir de energías renovables#
#                                                          #
############################################################

ser_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/share-electricity-renewables.csv")
ser_data = ser_data %>% select(-Code)
names(ser_data)[c(3)] = c("Ren_elec")

ser_data$iso3 = countrycode(ser_data$Entity, origin = "country.name", destination = "iso3c")
ser_data = ser_data[!is.na(ser_data$iso3), ]
ser_data = st_drop_geometry(ser_data)

world = ne_countries(scale = "medium", returnclass = "sf")

world_data = left_join(world, ser_data, by = c("iso_a3_eh" = "iso3"))
world_data = world_data[!is.na(world_data$Ren_elec), ]
world_data = st_drop_geometry(world_data)

ui <- fluidPage(
  titlePanel("Mapa de Producción de Electricidad a partir de Energía Renovable por País"),
  tagList(
    leafletOutput("mapa", height = 600),
    sliderInput("anio", "Selecciona el año:",
                min = min(world_data$Year),
                max = max(world_data$Year),
                value = max(world_data$Year),
                step = 1,
                sep = "",
                animate = FALSE)
  )
)

server = function(input, output, session) {
  datos_filtrados = reactive({
    world_data %>%
      filter(Year == input$anio) %>%
      mutate(iso3 = countrycode(Entity, origin = "country.name", destination = "iso3c"))
  })
  
  mapa_datos = reactive({
    world = ne_countries(scale = "medium", returnclass = "sf")
    left_join(world, datos_filtrados(), by = c("iso_a3_eh" = "iso3"))
  })
  
  output$mapa <- renderLeaflet({
    datos <- mapa_datos()
    pal <- colorNumeric("Blues", domain = datos$Ren_elec, na.color = "transparent")
    
    leaflet(datos) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Ren_elec),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~paste(Entity, ": ", round(Ren_elec, 1), "%"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Ren_elec, title = paste("Energía Eléctrica (%) -", input$anio))
  })
}
  
shinyApp(ui, server)  


######################################
#                                    #
#Generación de Energía Hidroeléctrica#
#                                    #
######################################

hg_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/hydropower-generation.csv")
hg_data = hg_data %>% select(-Code)
names(hg_data)[c(3)] = c("Hydro_gen")


#############################################################################
#                                                                           #
#Porcentaje de Producción de Electricidad a partir de Energía Hidroeléctrica#
#                                                                           #
#############################################################################

hse_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/hydro-share-energy.csv")
hse_data = hse_data %>% select(-Code)
names(hse_data)[c(3)] = c("Hydro_share")


##############################
#                            #
#Generación de Energía Eólica#
#                            #
##############################

wg_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/wind-generation.csv")
wg_data = wg_data %>% select(-Code)
names(wg_data)[c(3)] = c("Wind_gen")


############################################################################
#                                                                          #
#Porcentaje del Consumo de Energía Primaria Procedente de la Energía Eólica#
#                                                                          #
############################################################################

wse_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/wind-share-energy.csv")
wse_data = wse_data %>% select(-Code)
names(wse_data)[c(3)] = c("Wind_share")


##############################################################
#                                                            #
#Porcentaje de Producción de Electricidad a partir del Viento#
#                                                            #
##############################################################

sew_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/share-electricity-wind.csv")
sew_data = sew_data %>% select(-Code)
names(sew_data)[c(3)] = c("Wind_Elec_share")


#############################
#                           #
#Generación de Energía Solar#
#                           #
#############################

sg_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/installed-solar-pv-capacity.csv")
sg_data = sg_data %>% select(-Code)
names(sg_data)[c(3)] = c("Solar_gen")


############################################################################
#                                                                          #
#Porcentaje del Consumo de Energía Primaria Procedente de la Energía Solar #
#                                                                          #
############################################################################

sse_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/solar-share-energy.csv")
sse_data = sse_data %>% select(-Code)
names(sse_data)[c(3)] = c("Solar_share")


####################################################################
#                                                                  #
#Porcentaje de Producción de Electricidad a partir de Energía Solar#
#                                                                  #
####################################################################

ses_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/share-electricity-solar.csv")
ses_data = ses_data %>% select(-Code)
names(ses_data)[c(3)] = c("Solar_Elec_share")


#####################################################
#                                                  #
#Producción de Energía a partir de Biocuombistibles#
#                                                  #
####################################################

sse_data = read.csv("~/Diplomado/Módulos/Módulo 8/Proyecto/solar-share-energy.csv")
sse_data = sse_data %>% select(-Code)
names(sse_data)[c(3)] = c("Solar_share")

