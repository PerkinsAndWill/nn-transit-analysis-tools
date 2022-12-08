library(tidyverse)
library(sf)
library(janitor)
library(tidytransit)
library(furrr)
library(viridis)
library(nntools)
library(clipr)

future::plan(multisession)

# USER INPUTS ---------------------

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
if(!dir.exists("viz/frequency-span-charts")){dir.create("viz/frequency-span-charts")}
if(!dir.exists(paste0("viz/frequency-span-charts/",folder_name))){
  dir.create(paste0("viz/frequency-span-charts/",folder_name))
}

full_output_folder = paste0("viz/frequency-span-charts/",folder_name)

# GTFS Table Setup ------------
bus_gtfs = read_gtfs(feed_path)

trips            <- bus_gtfs$trips  
stop_times       <- bus_gtfs$stop_times 
shapes           <- bus_gtfs$shapes 
routes           <- bus_gtfs$routes %>%
  mutate(route_label = case_when(
    str_length(route_short_name) == 0 ~ route_long_name,
    TRUE ~ paste0(str_pad(route_short_name,width=3,side="left",pad="0"),": ",route_long_name)
  ))
stops            <- bus_gtfs$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

gtfs_table_names <- names(bus_gtfs)

#This if statement will build a cross reference of date and service ID via calendar files
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

most_freq_stop_screen = stop_times %>%
  left_join(trips %>% select(trip_id,route_id,direction_id,shape_id,service_id)) %>%
  left_join(calendar %>% distinct(service_id,day_cat)) %>%
  group_by(service_id,day_cat,route_id,direction_id,stop_id) %>%
  summarise(stop_count = n()) %>%
  arrange(service_id,day_cat,route_id,direction_id,desc(stop_count)) %>%
  do(head(.,n=1)) %>%
  ungroup() %>%
  select(-stop_count)

#Subset to most frequent stop times
most_freq_stop_times = stop_times %>%
  left_join(trips %>% select(trip_id,route_id,direction_id,shape_id,service_id)) %>%
  right_join(most_freq_stop_screen) %>%
  group_by(service_id,day_cat,route_id,direction_id,trip_id) %>%
  arrange(service_id,day_cat,route_id,direction_id,trip_id,stop_sequence) %>%
  do(tail(.,n=1)) %>%
  ungroup() %>%
  mutate(departure_time = str_pad(departure_time,width=8,side='left',pad='0'),
         arrival_time = str_pad(arrival_time,width=8,side='left',pad='0')) %>%
  mutate(trip_depart_hour = as.numeric(str_sub(departure_time,1,2))+
           as.numeric(str_sub(departure_time,4,5))/60 +
           as.numeric(str_sub(departure_time,7,8))/3600) %>%
  mutate(floor_hour = floor(trip_depart_hour)) %>%
  group_by(service_id,day_cat,route_id,direction_id,stop_id) %>%
  arrange(service_id,day_cat,route_id,direction_id,stop_id,trip_depart_hour) %>%
  mutate(headway_observed = (trip_depart_hour-lag(trip_depart_hour))*60) %>%
  ungroup() 


# Plot Assembly -------------------------------------------------------------------------

headway_summary <- most_freq_stop_times %>%
  select(service_id,day_cat,route_id,direction_id,floor_hour,
         headway_observed,trip_id, trip_depart_hour) %>%
  filter(!is.na(headway_observed)) %>%
  left_join(calendar) %>%
  group_by(day_cat,route_id,floor_hour) %>%
  summarise(mean_headway_observed = mean(headway_observed, na.rm=TRUE),
            num_dates_observed = n_distinct(date),
            num_uq_trip_ids = n_distinct(trip_id),
            min_trip_depart_hour = min(trip_depart_hour),
            max_trip_depart_hour = max(trip_depart_hour)) %>%
  ungroup() %>%
  left_join(routes %>% select(route_id,route_type,route_label)) %>%
  mutate(span_depart_hour = max_trip_depart_hour - min_trip_depart_hour) %>%
  filter(span_depart_hour > mean_headway_observed/60)

wash_co_route_ref <- read_rds("data/output/transit-supply-analysis/trimet/wash_co_route_reference.rds")

sub_headway_summary <- headway_summary %>%
  filter(route_id %in% wash_co_route_ref$route_id) %>%
  filter(day_cat == "Weekday") %>%
  group_by(route_id) %>%
  mutate(sum_uq_trip_ids = sum(num_uq_trip_ids)) %>%
  ungroup() %>%
  arrange(desc(route_type),sum_uq_trip_ids,route_label) %>%
  mutate(route_label = factor(route_label, ordered = TRUE, levels = unique(route_label)))

num_uq_routes <- length(unique(sub_headway_summary$route_label))
route_levels = levels(sub_headway_summary$route_label)

ggplot(sub_headway_summary, aes(xmin=floor_hour, xmax = floor_hour +1, 
                                ymin = as.numeric(route_label)-1,
                                ymax = as.numeric(route_label), 
                                fill = mean_headway_observed)) +
  geom_rect()+
  scale_fill_viridis(option = "F", limits = c(0,60),alpha = 0.7,
                     name = "Avg.\nHeadway")+
  theme_light() +
  scale_x_continuous(breaks = seq(0,26,2),
                     labels = c("12 am","2 am","4 am","6 am",
                                "8 am","10 am","12 pm","2 pm",
                                "4 pm","6 pm","8 pm","10 pm",
                                "12 am","2 am"),
                     name = "Hour of Day")+
  scale_y_continuous(breaks = seq(0.5,num_uq_routes-0.5),
                     limits = c(0,num_uq_routes),
                     labels = route_levels,
                     expand = expansion(0))+
  labs(
    y = "Route",
    title = "Frequency and Span for Washington County, OR Routes",
    subtitle = "Weekday Service, Fall 2022"
  )+
  theme(panel.grid.major.y = element_blank())
