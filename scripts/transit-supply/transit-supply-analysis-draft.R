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

# devtools::install_github("bpb824/valhallr")
# devtools::install_github("PerkinsAndWill/nntools", ref = "master", auth_token = "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")

future::plan(multisession)

# USER INPUTS ---------------------

#Buffer distance
buff_dist <- 5280/4 # Quarter mile

#Folder for saving outputs to
folder_name = "trimet"

#Coordinate systems
coord_global = 4326
coord_local = 2269 #see here: https://nelsonnygaard.shinyapps.io/coord-system-reference/

#Path to GTFS feed you will use
feed_path = "data/input/gtfs/trimet/2022-09-22.zip"

#A character string to identify the time period of the GTFS feed you are using
app_time_period = "Fall 2022"
file_ending = str_replace_all(app_time_period," ","_")

# Project Setup -----------

if(!dir.exists("data/output")){dir.create("data/output")}
if(!dir.exists("data/output/transit-supply-analysis")){dir.create("data/output/transit-supply-analysis")}
if(!dir.exists(paste0("data/output/transit-supply-analysis/",folder_name))){dir.create(paste0("data/output/transit-supply-analysis/",folder_name))}

full_output_folder = paste0("data/output/transit-supply-analysis/",folder_name)

# NN's Valhalla instance, no need to change
nn_valhalla_hostname <- "128.199.8.29"

# GTFS Setup ------------
bus_gtfs = read_gtfs(feed_path)

trips            <- bus_gtfs$trips  
stop_times       <- bus_gtfs$stop_times 
shapes           <- bus_gtfs$shapes 
routes           <- bus_gtfs$routes 
stops            <- bus_gtfs$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

stop_dirs <- stop_times %>% 
  left_join(trips) %>%
  distinct(route_id, direction_id, shape_id, stop_id, stop_sequence) %>% 
  left_join(routes %>% select(route_id,route_short_name)) %>% 
  left_join(bus_gtfs$stops %>% select(stop_id, stop_name,stop_lon,stop_lat)) %>%
  arrange(route_id,direction_id,shape_id,stop_sequence)

shape_geom = shapes %>%
  nest(data = any_of(c('shape_pt_lon','shape_pt_lat','shape_pt_sequence','shape_dist_traveled'))) %>%
  mutate(geometry = map(data, function(df){
    df %>% 
      arrange(shape_pt_sequence) %>%
      select(shape_pt_lon,shape_pt_lat) %>%
      as.matrix() %>%
      st_linestring()
  }) %>%
    st_sfc(crs=coord_global)) %>%
  st_as_sf() %>%
  select(-data) %>%
  st_transform(coord_local) %>%
  mutate(time_period = app_time_period)

gtfs_table_names <- names(bus_gtfs)

if("calendar" %in% gtfs_table_names & "calendar_dates" %in% gtfs_table_names){
  calendar_reg<- bus_gtfs$calendar %>% 
    mutate(date_range = map2(start_date,end_date,function(sd,ed){
      seq.Date(sd,ed,by='1 day')
    })) %>%
    unnest(date_range) %>%
    rename(date = date_range) %>%
    pivot_longer(cols = monday:sunday) %>%
    rename(weekday_operating = name) %>%
    mutate(weekday_operating = str_to_title(weekday_operating)) %>%
    mutate(weekday = weekdays(date)) %>%
    filter(value == 1, weekday==weekday_operating) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
  
  calendar_exceptions <- bus_gtfs$calendar_dates %>%
    filter(exception_type == 1) %>%
    mutate(weekday = weekdays(date)) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
  
  calendar = bind_rows(calendar_reg,calendar_exceptions) %>%
    distinct() %>%
    arrange(service_id,date)
}else if ("calendar" %in% gtfs_table_names & !("calendar_dates" %in% gtfs_table_names)){
  calendar <- bus_gtfs$calendar %>% 
    mutate(date_range = map2(start_date,end_date,function(sd,ed){
      seq.Date(sd,ed,by='1 day')
    })) %>%
    unnest(date_range) %>%
    rename(date = date_range) %>%
    pivot_longer(cols = monday:sunday) %>%
    rename(weekday_operating = name) %>%
    mutate(weekday_operating = str_to_title(weekday_operating)) %>%
    mutate(weekday = weekdays(date)) %>%
    filter(value == 1, weekday==weekday_operating) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
}else if (!("calendar" %in% gtfs_table_names) & "calendar_dates" %in% gtfs_table_names){
  calendar <- bus_gtfs$calendar_dates %>%
    filter(exception_type == 1) %>%
    mutate(weekday = weekdays(date)) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
}

dt_trip_counts <- trips %>%
  filter(service_id %in% calendar$service_id) %>%
  left_join(calendar) %>%
  group_by(route_id,direction_id,shape_id,day_cat,date) %>%
  summarise(num_trips = n_distinct(trip_id)) %>%
  group_by(route_id,direction_id,shape_id,day_cat) %>%
  summarise(total_trips_observed = sum(num_trips),
            num_uq_service_days = n_distinct(date)) %>%
  ungroup() %>%
  group_by(day_cat) %>%
  mutate(ref_service_days = max(num_uq_service_days)) %>%
  mutate(avg_trips_per_day = total_trips_observed/ref_service_days) %>%
  select(route_id:day_cat,avg_trips_per_day) %>%
  pivot_wider(names_from = day_cat,values_from = avg_trips_per_day,
              names_prefix = 'num_trips_') %>%
  mutate(across(contains("num_trips"),replace_na,0)) %>%
  select(route_id:shape_id,num_trips_Weekday,num_trips_Saturday,num_trips_Sunday)

shape_pattern_ref = trips %>%
  filter(route_id %in% sub_routes$route_id) %>%
  distinct(route_id,direction_id,shape_id) %>%
  left_join(sub_routes %>% select(route_id,route_short_name,route_long_name)) %>%
  left_join(stop_dirs %>% 
              group_by(route_id,direction_id,shape_id) %>%
              summarise(num_stops = n())) %>%
  left_join(dt_trip_counts) %>%
  mutate(total_weekly_trips = num_trips_Weekday*5 + num_trips_Saturday + num_trips_Sunday) %>%
  group_by(route_id,direction_id) %>%
  arrange(route_id,direction_id,desc(total_weekly_trips)) %>%
  mutate(trip_rank = 1:n()) %>%
  ungroup() %>%
  group_by(shape_id) %>%
  mutate(pattern_alpha = ifelse(
    trip_rank<27,LETTERS[trip_rank],str_c("Z",LETTERS[(trip_rank-26)])
  )
  ) %>%
  ungroup() %>%
  mutate(primary_shape = trip_rank==1) %>%
  mutate(pattern_label = paste0(
    pattern_alpha, " (",shape_id,"): ",
    case_when(
      num_trips_Weekday>0 & num_trips_Saturday>0 & num_trips_Sunday>0 ~ "Daily Service",
      num_trips_Weekday>0 & num_trips_Saturday==0 & num_trips_Sunday==0 ~ "Monday - Friday Service",
      num_trips_Weekday>0 & num_trips_Saturday>0 & num_trips_Sunday==0 ~ "Monday - Saturday Service",
      num_trips_Weekday>0 & num_trips_Saturday==0 & num_trips_Sunday>0 ~ "Sunday - Friday Service",
      num_trips_Weekday==0 & num_trips_Saturday>0 & num_trips_Sunday>0 ~ "Saturday & Sunday Service",
      num_trips_Weekday==0 & num_trips_Saturday>0 & num_trips_Sunday==0 ~ "Saturday Only Service",
      num_trips_Weekday==0 & num_trips_Saturday==0 & num_trips_Sunday>0 ~ "Sunday Only Service",
      TRUE ~ "Daily Service"
    )
  )) 

stop_shp_ref <- stop_dirs %>%
  distinct(shape_id,stop_id)


# Defining extent of census geography --------------

shape_geom_hull <- shape_geom %>%
  st_buffer(buff_dist) %>%
  st_union() 

us_counties <- counties(year = 2020)

sub_us_counties <- us_counties %>%
  st_transform(coord_local) %>%
  filter(st_intersects(., shape_geom_hull, sparse = FALSE))

state_county_pairs <- sub_us_counties %>%
  st_drop_geometry() %>%
  distinct(STATEFP,COUNTYFP)

sub_block_groups <- state_county_pairs %>%
  mutate(bg_geoms = pmap(.l = list(STATEFP, COUNTYFP),
                         .f = function(state_fips, county_fips){
                           
                           block_groups(state = state_fips, county = county_fips, year = 2020)
                           
                         })) %>%
  select(bg_geoms) %>%
  unnest(bg_geoms) %>%
  st_as_sf() %>%
  st_transform(coord_local)

# Generating Walksheds per Stop ----------

buff_dist_km <- buff_dist*0.0003048

#This step will take a few minutes depending on the number of stops
with_progress({
  
  p <- progressor(steps = nrow(stops))
  
  stop_walksheds_queried <- stops %>%
    select(stop_id,stop_name,geometry) %>%
    st_transform(coord_global) %>%
    mutate(walkshed_geom = future_pmap(.l = list(geometry),
                                       .f = function(geom, p){
                                         
                                         p()
                                         
                                         pt_geom <- geom %>% st_coordinates() %>%
                                           as_tibble() %>%
                                           rename(lon = X, lat = Y)
                                         
                                         iso_result <- isochrone(from = pt_geom,
                                                                 costing = "pedestrian",
                                                                 contours = c(buff_dist_km),
                                                                 metric = "km",
                                                                 hostname = nn_valhalla_hostname)
                                         
                                         return(iso_result$geometry)
                                         
                                       }, p = p))
})

stop_walksheds <- stop_walksheds_queried %>%
  st_drop_geometry() %>%
  mutate(geometry = map(walkshed_geom,~.x[[1]]) %>% st_sfc(crs=coord_global)) %>%
  select(-walkshed_geom) %>%
  st_as_sf() %>%
  st_transform(coord_local)

# leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data = stop_walksheds)

# Calculating Transit Supply ----------------

with_progress({
  
  p <- progressor(steps = nrow(sub_block_groups))
  
  bgs_with_transit_supply <- sub_block_groups %>%
    mutate(transit_supply_raw = future_pmap(
      .l = list(geometry),
      .f = function(geom, p){
        
        p()
        
        # x=500
        # geom <- sub_block_groups$geometry[[x]]
        
        bg_geom <- geom %>% st_sfc(crs=coord_local)
        
        sub_walksheds <- stop_walksheds %>%
          filter(st_intersects(.,bg_geom,sparse = FALSE))
        
        if(nrow(sub_walksheds)>0){
          
          sub_shape_ids <- stop_shp_ref %>%
            filter(stop_id %in% sub_walksheds$stop_id) %>%
            distinct(shape_id) %>%
            pull(shape_id)
          
          sub_shape_pattern_ref <- shape_pattern_ref %>%
            filter(shape_id %in% sub_shape_ids)
          
          sub_route_trips <- sub_shape_pattern_ref %>%
            group_by(route_id,direction_id) %>%
            summarise(across(contains("num_trips"),sum)) %>%
            group_by(route_id) %>%
            summarise(across(contains("num_trips"),mean))
          
          unioned_walkshed <- sub_walksheds %>%
            st_union()
          
          cut_walkshed <- unioned_walkshed %>%
            st_intersection(bg_geom)
          
          cut_walkshed_area <- as.numeric(st_area(cut_walkshed))*2.29568e-5
          
          whole_bg_area <- as.numeric(st_area(bg_geom))*2.29568e-5
          
          transit_supply_raw_calc <- sub_route_trips %>%
            pivot_longer(contains("num_trips")) %>%
            mutate(day_cat = str_replace(name,"num_trips_","")) %>%
            mutate(transit_supply_raw = value * cut_walkshed_area) %>%
            group_by(day_cat) %>%
            summarise(transit_supply_raw = sum(transit_supply_raw)/whole_bg_area) 
          
          return(transit_supply_raw_calc)
          
        }else{
          return(tibble())
        }
        
      }, p = p))
  
})

wash_co_bgs <- sub_block_groups %>%
  filter(COUNTYFP == "067")

transit_supply_results <- bgs_with_transit_supply %>%
  st_drop_geometry() %>%
  filter(GEOID %in% wash_co_bgs$GEOID) %>%
  select(GEOID,transit_supply_raw) %>%
  unnest(transit_supply_raw) %>%
  group_by(day_cat) %>%
  mutate(transit_supply_score_by_day_cat = percent_rank(transit_supply_raw)) %>%
  ungroup() %>%
  mutate(transit_supply_score_across_day_cat = percent_rank(transit_supply_raw))

weekday_transit_supply_geom <- wash_co_bgs %>%
  left_join(transit_supply_results %>% filter(day_cat == "Weekday")) %>%
  mutate(across(contains("transit_supply"),replace_na,0)) %>%
  st_transform(coord_global)

supply_score_pal = colorNumeric(
  palette = viridis::magma(10,direction = -1),
  domain = c(0,1)
)

map_obj <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = weekday_transit_supply_geom, color="white", weight=2,
              opacity = 0.8,
              fillColor =~ supply_score_pal(transit_supply_score_across_day_cat),
              fillOpacity = 0.5, label =~ comma(transit_supply_score_across_day_cat,accuracy = 0.001),
              highlightOptions = highlightOptions(weight = 4, fillOpacity = 0.75)) %>%
  addLegend(position = "topright", pal = supply_score_pal, values = c(0,0.5,1),
            title = "Weekday<br>Transit<br>Supply<br>Score")

htmlwidgets::saveWidget(map_obj,file = "docs/transit-supply-example-maps/wash-co-example.html")

