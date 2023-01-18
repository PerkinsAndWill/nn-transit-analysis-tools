library(tidyverse)
library(sf)
library(rgdal)
library(leaflet)

file.copy("G:/Current/CLACKAMAS_Walk_Bike_2021.0308/Analysis/KML/fromGIS/Wikimap_mask_.kmz",
          "data/input/misc/wikimap-mask.kmz")

unzip("data/input/misc/wikimap-mask.kmz")

st_layers("data/input/misc/wikimap-mask.kmz")
rgdal::ogrListLayers("data/input/misc/wikimap-mask.kmz")
raw <- read_sf("data/input/misc/wikimap-mask - Copy/doc.kml") %>%
  st_zm()


leaflet() %>%
  addTiles() %>%
  addPolygons(data = raw)
