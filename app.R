# Paquetes
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(shiny)
library(shinydashboard)

# Lectura de una capa vectorial (GeoJSON) de distritos de Montes de Oca
distritos <-
  st_read(
    "https://raw.githubusercontent.com/MontserratJB/Proyecto-2021/master/Distritos.geojson",
    quiet = TRUE
  )

# Lectura de un archivo CSV con datos de Presupuestos participativos en Montes de Oca
presupuesto <-
  st_read(
    "/vsicurl/https://raw.githubusercontent.com/MontserratJB/tarea03-shinyapp/master/Pres_part.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

#Lectura de vias primarias
viaprimaria <-
  st_read(
    "https://raw.githubusercontent.com/MontserratJB/proyecto02_shiny/master/viasprimarias.geojson",
    quiet = TRUE
  )


# Asignación de un CRS al objeto presupuesto
st_crs(presupuesto) <- 4326

