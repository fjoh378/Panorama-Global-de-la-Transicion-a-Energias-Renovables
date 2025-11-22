

library(dplyr)
library(readr)

er_data <- read.csv("CRUCE_ER_CF.csv", na.strings = c("null", "NA", "")) 





# Crear columna de TOTAL RENOVABLES (TWh)
indice1 <- er_data |>
  mutate(
    total_renovables_twh = rowSums(
      cbind(
        electricity_from_wind_t_wh__modern_renewable_prod,
        electricity_from_hydro_t_wh__modern_renewable_prod,
        electricity_from_solar_t_wh__modern_renewable_prod,
        other_renewables_including_bioenergy_t_wh__modern_renewable_prod
      ),
      na.rm = TRUE
    )
  )

