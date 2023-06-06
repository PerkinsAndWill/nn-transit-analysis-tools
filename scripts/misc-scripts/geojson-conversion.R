library(tidyverse)
library(geojsonsf)
library(jsonlite)
library(leaflet)
library(sf)

# GeoJSON to Shape- -------

city_limits_raw = geojson_sf("data/input/misc/milwaukeewi-spin-milwaukee_city_limits_export (2).geojson")

trail_network_raw = geojson_sf("data/input/misc/milwaukeewi-spin-full_trail_network_export (1).geojson")

leaflet() %>%
  addTiles() %>%
  addPolylines(data = trail_network_raw) %>%
  addPolylines(data = city_limits_raw, color="red")

export_path <- "G:/Current/CONFIDENTIAL_Project_Naraja/Data/z.Original/2023.03.15_Milwaukee/converted-shapefiles"

write_sf(city_limits_raw,paste0(export_path,"/city-limits.shp"))
write_sf(trail_network_raw,paste0(export_path,"/trail-network.shp"))


# Shape to geojson --------------

gdb_path <- "G:/Current/CONFIDENTIAL_Project_Naraja/Data/GDBs/SanDiego2022"

feature_raw = read_sf(gdb_path, layer = "LoganHeightsBlocks")

write_sf(feature_raw,"G:/Current/CONFIDENTIAL_Project_Naraja/Data/converted-geojson/LoganHeightsBlocks.geojson")

feature_wgs <- feature_raw %>%
  st_transform(4236)

write_sf(feature_wgs,"G:/Current/CONFIDENTIAL_Project_Naraja/Data/converted-geojson/LoganHeightsBlocks_wgs84.geojson")
