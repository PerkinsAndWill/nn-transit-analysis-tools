library(tidyverse)
library(sf)
library(janitor)
library(tidytransit)
library(leaflet)
library(rgeos)
library(openxlsx)
library(readxl)
library(scales)
library(rstudioapi)
library(tigris)
library(tidycensus)
library(lehdr)
library(osmdata)
library(furrr)
library(sfnetworks)
library(DBI)
library(nntools)
library(broom)
library(progressr)
library(nngeo)
library(lwgeom)
library(httr)
library(jsonlite)
library(valhallr)
library(geosphere)
library(htmlwidgets)

bus_gtfs <- read_gtfs("data/input/gtfs/dart/2023-04-20.zip")

coord_global = 4326
coord_local = 2276 #see here: https://nelsonnygaard.shinyapps.io/coord-system-reference/

trips            <- bus_gtfs$trips  
stop_times       <- bus_gtfs$stop_times 
shapes           <- bus_gtfs$shapes 
routes           <- bus_gtfs$routes 
stops            <- bus_gtfs$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

#Stop Orders
stop_dirs <- stop_times %>% 
  left_join(trips) %>%
  distinct(route_id, direction_id, shape_id, stop_id, stop_sequence) %>% 
  left_join(routes %>% select(route_id,route_short_name)) %>% 
  left_join(bus_gtfs$stops %>% select(stop_id, stop_name,stop_lon,stop_lat)) %>%
  arrange(route_id,direction_id,shape_id,stop_sequence)

uq_rt_stops <- stop_dirs %>%
  distinct(stop_id,route_id,route_short_name) %>%
  arrange(stop_id,route_short_name)

clipr::write_clip(uq_rt_stops)

stop_rt_summary <- uq_rt_stops %>%
  group_by(stop_id) %>%
  summarise(num_routes = n(),
            route_string = str_flatten(route_short_name,collapse = ", ")) 

clipr::write_clip(stop_rt_summary)  
