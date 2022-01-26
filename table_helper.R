
#### Table Set 4 - Player Page Tournaments-------------------------------
athlete_results <- function(results, athlete) {
  
  #Output table names
  header <- c( "Date" = "date",  "Race" = "event",  "Dist (km)" = "dist_km" , "Time" = "time" , "Speed (Kph)" = "speed" , "Place" = "position")
  
  tbl_oi <- results %>%
    dplyr::filter(rider_name == athlete) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::select(date, event, dist_km, time, position, speed) %>%
    mutate(date = format(date, "%B %e, %Y")) %>% 
    dplyr::rename(!!header) 
  
  return(tbl_oi)
}

event_results <- function(results, date) {
  
  #Output table names
  header <- c( "Dist (km)" = "dist_km" , "Time" = "time" , "Speed (Kph)" = "speed" , "Place" = "position", "Athlete" = "rider_name")
  
  date_filter <- ymd(str_sub(date,1,10))
  
  tbl_oi <- results %>%
    dplyr::filter(date == date_filter) %>%
    dplyr::arrange(desc(dist_km), position) %>%
    dplyr::select(position, rider_name, dist_km, time, speed) %>%
    dplyr::rename(!!header) 
  
  return(tbl_oi)
}

weather_results <- function(weather, date) {
  
  #Output table names
  header <- c( "Temp" = "tair_c" , "Humidity" = "rh_percent" , "Pressure" = "pressure" , "Wind Spd" = "speed_km_hr", "Wind Dir" = "wd_cardinal" , "Density" = "density")
  
  date_filter <- ymd(str_sub(date,1,10))
  
  tbl_oi <- weather %>%
    dplyr::filter(date == date_filter) %>%
    select(-date , -dir_deg_t) %>% 
    mutate(
      tair_c = paste0(tair_c,"C"),
      rh_percent = paste0(rh_percent,"%"),
      pressure = paste0(pressure,"kpa"),
      speed_km_hr = paste0(speed_km_hr,"kph")) %>% 
      relocate(wd_cardinal, .after = rh_percent) %>% 
      dplyr::rename(!!header)

  return(tbl_oi)
}


# 
# date_temp <- dmy(23112021)
# 
# 
# #Output table names
# header <- c( "Temp" = "tair_c" , "Humidity" = "rh_percent" , "Pressure" = "pressure" , "Wind Spd" = "speed_km_hr", "Wind Dir" = "wd_cardinal" , "Density" = "density")
# 
# date_filter <- ymd(str_sub(date_temp,1,10))
# 
# tbl_oi <- weather %>%
#   dplyr::filter(date == date_temp) %>%
#   select(-date , -dir_deg_t) %>% 
#   mutate(
#     tair_c = paste0(tair_c,"C"),
#     rh_percent = paste0(rh_percent,"%"),
#     pressure = paste0(pressure,"kpa"),
#     speed_km_hr = paste0(speed_km_hr,"kph")) %>% 
#   relocate(wd_cardinal, .after = rh_percent) %>% 
#   dplyr::rename(!!header)







