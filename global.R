# load libraries
library(sf)
library(leaflet)
library(tidyverse)
library(DT)
library(bslib)

# import data
#bn_data <- st_read("data/establecimientos_bahia_negra.shp")
cad_data <- st_read("data/lup.gpkg",
                   layer= "lup")
lup_data <- st_read("data/lup.gpkg",
                   layer= "lup_limit")
