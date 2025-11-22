CATÁLOGO DE VARIABLES
Archivo resultante: cruce_er_cf.csv.csv

==================================================
1. DESCRIPCIÓN GENERAL
==================================================

El archivo panel_energia_21archivos.csv integra 21 bases de datos de Our World in Data (OWID) sobre energía fósil y renovable. Cada fila corresponde a una combinación de:

- Entity  (país, región o agregado geográfico)
- Year    (año calendario)

El cruce se realizó mediante un FULL OUTER JOIN por:

- Entity
- Year

Es decir, se incluyen todas las combinaciones de Entity–Year que aparezcan en al menos uno de los archivos.

La columna Code de los archivos originales NO se utilizó para el cruce y se eliminó del panel final.

Además, se excluyó completamente el archivo:
- global-fossil-fuel-consumption.csv

Para evitar ambigüedad con columnas que tienen el mismo nombre en distintos archivos, todas las columnas (excepto Entity y Year) fueron renombradas añadiendo el nombre del archivo de origen entre paréntesis.

Ejemplo:
- Columna original en share-electricity-solar.csv:
    Solar - % electricity
- En el panel final:
    Solar - % electricity (share-electricity-solar)

Esto permite saber siempre de qué CSV de OWID proviene cada serie.

Los datos proceden de las páginas de energía, combustibles fósiles y renovables de Our World in Data, que a su vez se basan principalmente en Ember y en el Energy Institute – Statistical Review of World Energy (BP en versiones anteriores).

==================================================
2. ESTRUCTURA DEL ARCHIVO
==================================================

Columnas clave (llaves):

- Entity
    Nombre del país, región o agregado geográfico.
    Ejemplos: "World", "Mexico", "European Union (27)", etc.

- Year
    Año calendario al que corresponde la observación.

Todas las demás columnas son indicadores de energía (porcentaje, TWh, GW, etc.) y fueron renombradas como:

    <nombre_original> (<nombre-del-archivo-sin-.csv>)

Ejemplo:
    Renewables - % electricity (share-electricity-renewables)

==================================================
3. VARIABLES POR ARCHIVO DE ORIGEN
==================================================

--------------------------------------------------
3.1. share-electricity-solar.csv
--------------------------------------------------

Solar - % electricity (share-electricity-solar)
    Porcentaje de la generación total de electricidad que proviene de
    energía solar en el país/entidad y año considerados.
    Unidad: %
    Fuente principal: Ember / Energy Institute (procesado por OWID).

--------------------------------------------------
3.2. share-electricity-renewables.csv
--------------------------------------------------

Renewables - % electricity (share-electricity-renewables)
    Porcentaje de la electricidad total generada a partir de todas las
    fuentes renovables (solar, eólica, hidro, bioenergía, geotermia,
    mareas y otras).
    Unidad: %
    Fuente principal: Ember + procesamiento OWID.

--------------------------------------------------
3.3. share-electricity-wind.csv
--------------------------------------------------

Wind - % electricity (share-electricity-wind)
    Porcentaje de la generación eléctrica que proviene de energía eólica.
    Unidad: %.

--------------------------------------------------
3.4. share-electricity-hydro.csv
--------------------------------------------------

Hydro - % electricity (share-electricity-hydro)
    Porcentaje de la generación eléctrica que proviene de energía
    hidroeléctrica.
    Unidad: %.

--------------------------------------------------
3.5. share-electricity-fossil-fuels.csv
--------------------------------------------------

Fossil fuels - % electricity (share-electricity-fossil-fuels)
    Porcentaje de la generación eléctrica que proviene de combustibles
    fósiles (carbón, petróleo y gas).
    Unidad: %.

==================================================
4. PARTICIPACIÓN EN ENERGÍA PRIMARIA
(% EQUIVALENT PRIMARY ENERGY)
==================================================

Estas variables se expresan como porcentaje de la energía primaria equivalente utilizando el “método de sustitución” (substitution method), es decir, cuánta energía fósil sería necesaria para generar la misma cantidad de electricidad renovable.

--------------------------------------------------
4.1. renewable-share-energy.csv
--------------------------------------------------

Renewables (% equivalent primary energy) (renewable-share-energy)
    Porcentaje del consumo de energía primaria equivalente que proviene
    de fuentes renovables.
    Unidad: %.

--------------------------------------------------
4.2. solar-share-energy.csv
--------------------------------------------------

Solar (% equivalent primary energy) (solar-share-energy)
    Porcentaje del consumo de energía primaria equivalente que proviene
    de energía solar.
    Unidad: %.

--------------------------------------------------
4.3. hydro-share-energy.csv
--------------------------------------------------

Hydro (% equivalent primary energy) (hydro-share-energy)
    Porcentaje del consumo de energía primaria equivalente que proviene
    de energía hidroeléctrica.
    Unidad: %.

--------------------------------------------------
4.4. wind-share-energy.csv
--------------------------------------------------

Wind (% equivalent primary energy) (wind-share-energy)
    Porcentaje del consumo de energía primaria equivalente que proviene
    de energía eólica.
    Unidad: %.

--------------------------------------------------
4.5. fossil-fuels-share-energy.csv
--------------------------------------------------

Fossil fuels (% equivalent primary energy) (fossil-fuels-share-energy)
    Porcentaje del consumo de energía primaria equivalente que proviene
    de combustibles fósiles.
    Unidad: %.

==================================================
5. CAPACIDAD INSTALADA (GW U OTRAS UNIDADES)
==================================================

--------------------------------------------------
5.1. cumulative-installed-wind-energy-capacity-gigawatts.csv
--------------------------------------------------

Wind capacity (total) (GW) (cumulative-installed-wind-energy-capacity-gigawatts)
    Capacidad instalada total de energía eólica, en gigawatts (GW).
    Incluye normalmente tanto eólica onshore como offshore.

--------------------------------------------------
5.2. installed-solar-pv-capacity.csv
--------------------------------------------------

Solar capacity (total) (GW) (installed-solar-pv-capacity)
    Capacidad instalada total de energía solar fotovoltaica (PV), en GW.

--------------------------------------------------
5.3. installed-geothermal-capacity.csv
--------------------------------------------------

Geothermal capacity (total) (installed-geothermal-capacity)
    Capacidad instalada total de energía geotérmica.
    La unidad exacta en la fuente OWID suele ser GW o MW estandarizados.

==================================================
6. GENERACIÓN ELÉCTRICA POR FUENTE (TWh)
==================================================

Gran parte de estas series utilizan TWh (terawatt-horas) como unidad de
electricidad generada o consumida.

--------------------------------------------------
6.1. modern-renewable-energy-consumption.csv
--------------------------------------------------

Other renewables (including geothermal and biomass) electricity generation - TWh (modern-renewable-energy-consumption)
    Generación eléctrica a partir de “otras renovables”, incluyendo
    geotermia y biomasa, en TWh.

Solar generation - TWh (modern-renewable-energy-consumption)
    Generación eléctrica anual a partir de energía solar, en TWh.

Wind generation - TWh (modern-renewable-energy-consumption)
    Generación eléctrica anual a partir de energía eólica, en TWh.

Hydro generation - TWh (modern-renewable-energy-consumption)
    Generación eléctrica anual a partir de energía hidroeléctrica, en TWh.

--------------------------------------------------
6.2. modern-renewable-prod.csv
    (Modern renewable electricity generation by source)
--------------------------------------------------

Electricity from wind - TWh (modern-renewable-prod)
    Electricidad generada a partir de viento, en TWh, dentro del conjunto
    de “modern renewable electricity generation”.

Electricity from hydro - TWh (modern-renewable-prod)
    Electricidad generada a partir de energía hidroeléctrica, en TWh.

Electricity from solar - TWh (modern-renewable-prod)
    Electricidad generada a partir de energía solar, en TWh.

Other renewables including bioenergy - TWh (modern-renewable-prod)
    Electricidad generada a partir de otras renovables, incluyendo
    bioenergía, en TWh.

NOTA: Estas variables miden la misma magnitud física que otras series
de generación por fuente, pero se mantienen separadas porque provienen
de este archivo agrupador específico de OWID.

--------------------------------------------------
6.3. wind-generation.csv
--------------------------------------------------

Electricity from wind - TWh (wind-generation)
    Electricidad generada a partir de viento, en TWh, según la serie
    específica wind-generation de OWID.

--------------------------------------------------
6.4. hydropower-generation.csv
--------------------------------------------------

Electricity from hydro - TWh (hydropower-generation)
    Electricidad generada a partir de hidroelectricidad, en TWh, según
    la serie específica hydropower-generation.

--------------------------------------------------
6.5. solar-energy-consumption.csv
--------------------------------------------------

Electricity from solar - TWh (solar-energy-consumption)
    Electricidad generada a partir de energía solar, en TWh, según la
    serie solar-energy-consumption.

==================================================
7. ENERGÍA FÓSIL – ENERGÍA PRIMARIA Y CONSUMO POR TIPO
==================================================

--------------------------------------------------
7.1. fossil-fuel-primary-energy.csv
--------------------------------------------------

Fossil fuels (TWh) (fossil-fuel-primary-energy)
    Energía primaria total procedente de combustibles fósiles (carbón,
    petróleo y gas), expresada en TWh.
    Es la serie de “primary energy consumption from fossil fuels”.

--------------------------------------------------
7.2. fossil-fuel-consumption-by-type.csv
--------------------------------------------------

Oil consumption - TWh (fossil-fuel-consumption-by-type)
    Consumo de energía proveniente de petróleo (oil), en TWh.

Gas consumption - TWh (fossil-fuel-consumption-by-type)
    Consumo de energía proveniente de gas, en TWh.

Coal consumption - TWh (fossil-fuel-consumption-by-type)
    Consumo de energía proveniente de carbón, en TWh.

==================================================
8. BIOENERGÍA
==================================================

--------------------------------------------------
8.1. biofuel-production.csv
--------------------------------------------------

Biofuels production - TWh (biofuel-production)
    Producción de biocombustibles (biofuels) convertida a energía en TWh.
    Representa la energía contenida en biocombustibles producidos.

==================================================
9. RESUMEN DE VARIABLES “DUPLICADAS” POR FUENTE
==================================================

Existen varias variables que, físicamente, miden la misma magnitud
(electricidad generada por viento, hidro o solar) pero provienen de
archivos distintos:

ELECTRICIDAD DESDE VIENTO (TWh)
    - Wind generation - TWh (modern-renewable-energy-consumption)
    - Electricity from wind - TWh (wind-generation)
    - Electricity from wind - TWh (modern-renewable-prod)

ELECTRICIDAD DESDE HIDRO (TWh)
    - Hydro generation - TWh (modern-renewable-energy-consumption)
    - Electricity from hydro - TWh (hydropower-generation)
    - Electricity from hydro - TWh (modern-renewable-prod)

ELECTRICIDAD DESDE SOLAR (TWh)
    - Solar generation - TWh (modern-renewable-energy-consumption)
    - Electricity from solar - TWh (solar-energy-consumption)
    - Electricity from solar - TWh (modern-renewable-prod)

Para análisis y modelos, suele ser recomendable escoger UNA sola serie
por fuente (por ejemplo, siempre la de modern-renewable-prod o siempre
la grapher específica) para evitar duplicar información y contar dos
veces la misma magnitud.

==================================================
10. NOTAS DE USO DEL PANEL
==================================================

- Los NA en R se escribieron en el CSV como celdas vacías (na = "").
- No todas las entidades tienen datos completos en todos los años.
- Las series de participación (% electricity y % equivalent primary energy)
  suelen estar disponibles desde 1965–1970 en adelante, pero la cobertura
  varía por país.
- Para análisis comparativos (por ejemplo, entre países o por décadas),
  es recomendable filtrar por:
    * Un subconjunto de países, y
    * Un rango de años donde las variables clave no sean NA.

Este panel está pensado como una base maestra para análisis de transición
energética (evolución de renovables, fósiles, bioenergía, etc.) por país
y año.
