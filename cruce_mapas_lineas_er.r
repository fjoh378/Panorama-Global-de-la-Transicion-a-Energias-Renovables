library(dplyr)
library(readr)
library(purrr)
library(tools)


archivos <- c(
  "share-electricity-solar.csv",
  "modern-renewable-energy-consumption.csv",
  "cumulative-installed-wind-energy-capacity-gigawatts.csv",
  "share-electricity-renewables.csv",
  "installed-geothermal-capacity.csv",
  "renewable-share-energy.csv",
  "biofuel-production.csv",
  "solar-share-energy.csv",
  "hydro-share-energy.csv",
  "installed-solar-pv-capacity.csv",
  "wind-generation.csv",
  "fossil-fuel-primary-energy.csv",
  "fossil-fuel-consumption-by-type.csv",
  "share-electricity-wind.csv",
  "wind-share-energy.csv",
  "fossil-fuels-share-energy.csv",
  "modern-renewable-prod.csv",
  "hydropower-generation.csv",
  "share-electricity-fossil-fuels.csv",
  "share-electricity-hydro.csv",
  "solar-energy-consumption.csv"
)

# Función para leer y preparar cada archivo
leer_archivo <- function(nombre) {
  df <- read_csv(nombre, show_col_types = FALSE)
  
#Quitamos Code
  if ("Code" %in% names(df)) {
    df <- df %>% select(-Code)
  }
  

  base <- file_path_sans_ext(nombre)
  

  claves <- intersect(c("Entity", "Year"), names(df))
  

  otras <- setdiff(names(df), claves)
  names(df)[names(df) %in% otras] <- paste0(otras, " (", base, ")")
  
  df
}


lista_df <- archivos %>%
  set_names() %>% 
  map(leer_archivo)


panel_energia <- reduce(
  lista_df,
  .f = ~ full_join(.x, .y, by = c("Entity", "Year"))
)


write_csv(panel_energia, "CRUCE_ER_CF.csv", na = "")


