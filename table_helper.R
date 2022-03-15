#Package Coverage Test:
# library(rstudioapi)
# library(NCmisc)
# list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)


#### Table Set 4 - Player Page Tournaments-------------------------------
athlete_results <- function(results, athlete) {
  
  #Output table names
  header <- c( "Date" = "date",  "Race" = "event",  "Dist (km)" = "dist_km" , "Time" = "time" , "Speed (Kph)" = "speed" , "Place" = "position")
  
  tbl_oi <- results %>%
    dplyr::filter(rider_name == athlete) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::select(date, event, dist_km, time, position, speed) %>%
    dplyr::mutate(date = format(date, "%B %e, %Y")) %>% 
    dplyr::rename(!!header) 
  
  return(tbl_oi)
}

event_results <- function(results, date) {
  
  #Output table names
  header <- c( "Dist (km)" = "dist_km" , "Time" = "time" , "Speed (Kph)" = "speed" , "Place" = "position", "Athlete" = "rider_name")
  
  date_filter <- ymd(str_sub(date,1,10))
  
  tbl_oi <- results %>%
    dplyr::filter(date == date_filter) %>%
    dplyr::arrange(dplyr::desc(dist_km), position) %>%
    dplyr::select(position, rider_name, dist_km, time, speed) %>%
    dplyr::rename(!!header) 
  
  # tbl_oi <- tbl_oi %>% dplyr::mutate(Time = format(Time , '%M:%S'))
  
  return(tbl_oi)
}

weather_results <- function(weather, date) {
  
  #Output table names
  header <- c( "Temp" = "tair_c" , "Humidity" = "rh_percent" , "Pressure" = "pressure" , "Wind Spd" = "speed_km_hr", "Wind Dir" = "wd_cardinal" , "Density" = "density")
  
  date_filter <- ymd(stringr::str_sub(date,1,10))
  
  tbl_oi <- weather %>%
    dplyr::filter(date == date_filter) %>%
    dplyr::select(-date , -dir_deg_t) %>% 
    dplyr::mutate(
      tair_c = paste0(tair_c,"C"),
      rh_percent = paste0(rh_percent,"%"),
      pressure = paste0(pressure,"kpa"),
      speed_km_hr = paste0(speed_km_hr,"kph")) %>% 
      relocate(wd_cardinal, .after = rh_percent) %>% 
      dplyr::rename(!!header)

  return(tbl_oi)
}


leaderboard_table <- function(results, gender_filter, dist_filter, bike_type_filter, ag_filter, leaderboard_type, date_filter) {
  
  #Output table names
  header <- c( "Date" = "date" , "Time" = "time" , "Speed (Kph)" = "speed" , "Athlete" = "rider_name", "Place" = "position")
  
  
    leaderboard_type_fxn <- function(df, lb_t){
    if(lb_t == "riders"){
      df %>% dplyr::group_by(rider_name) %>%   
      dplyr::arrange(time) %>% 
      dplyr::slice(1L) %>%
      dplyr::ungroup() %>% 
      dplyr::arrange(time)
       }  
    
    else {
      df %>% 
      dplyr::arrange(time) %>%
      slice_head(n = 500)  
      }
  }
  
  tbl_oi <- results %>% 
    dplyr::filter(gender == gender_filter) %>% 
    dplyr::filter(dist_km == dist_filter) %>% 
    dplyr::filter(bike == bike_type_filter) %>%
    dplyr::filter(if(date_filter == "All") TRUE else season == date_filter ) %>% 
    dplyr::filter(rider_name_2 != "DNF") %>% 
    dplyr::filter(rider_name != "Cancelled") %>% 
    leaderboard_type_fxn(lb_t = leaderboard_type) %>% 
    dplyr::mutate(position = row_number()) %>% 
    dplyr::mutate(date = format(date, "%B %e, %Y")) %>%
    dplyr::select(position, rider_name, date, time, speed) %>% 
    dplyr::rename(!!header)
  
  return(tbl_oi)
}


compare_table <- function(results){
  
  
  
  
  
  
  return()
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







