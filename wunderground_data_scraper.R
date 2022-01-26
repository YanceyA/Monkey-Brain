library(tidyverse)
library(rvest)
library(lubridate)
library(jsonlite)
library(lubridate)

# Read static html. It won't create the table, but it holds the API key
# we need to retrieve the source JSON.

htm_obj <- 
  read_html('https://www.wunderground.com/dashboard/pws/ICHRISTC226/table/2021-11-2/2021-11-2/daily')

# Retrieve the API key. This key is stored in a node with javascript content.
str_apikey <- 
  html_node(htm_obj, xpath = '//script[@id="app-root-state"]') %>%
  html_text() %>% gsub("^.*SUN_API_KEY&q;:&q;|&q;.*$", "", . )

dates <- tt_results %>% filter(location == "Tai Tapu") %>% select(date) %>% distinct()

stations <- c("ICHRISTC226" , "ICHRISTC3" )

# Create a URI pointong to the API', with the API key as the first key-value pair of the query
      # url_apijson <- paste0(
      #   "https://api.weather.com/v2/pws/history/all?stationId=ICHRISTC226&format=json&units=m&date=20211010&numericPrecision=decimal&apiKey=",
      #   str_apikey)

# Capture the JSON
dates_2021 <- dates %>% filter(year(date) > 2020, year(date) < 2022 ) 
dates_2021 <- ymd(dates_2021$date)
dates_2021 <- format.Date(dates_2021, "%Y%m%d")

weather_table <- tbl_daily_fetch

for (n in 1:length(dates_2021)) {
  date <- dates_2021[n]
  station_test <- c("ICHRISTC3")
  
  url_apijson <- paste0(
    "https://api.weather.com/v2/pws/history/all?stationId=",station_test,"&format=json&units=m&date=",date,"&numericPrecision=decimal&apiKey=",
    str_apikey)
  
  json_obj <- fromJSON(txt = url_apijson)
  
  if (length(json_obj$observations) > 0) {
    tbl_daily_fetch <- json_obj$observations %>% 
      as_tibble() %>% 
      jsonlite::flatten() %>% 
      mutate(obsTimeUtc = as_datetime(obsTimeUtc) %>% with_tz("Pacific/Auckland")) %>%
      select(stationID, obsTimeUtc, metric.tempAvg, metric.dewptAvg, humidityAvg, winddirAvg, metric.windspeedAvg, metric.pressureMax, metric.pressureMin ) 
    
    weather_table <- full_join(weather_table, tbl_daily_fetch, by = NULL)
    Sys.sleep(5)
  }
  else
  Sys.sleep(1)
  
}

min_date <- min(weather_table$obsTimeUtc) %>% format.Date("%Y-%m-%d")
max_date <- max(weather_table$obsTimeUtc) %>% format.Date("%Y-%m-%d")

write_csv(weather_table, paste0("weather_",min_date,"_",max_date,".csv"))
  
options(digits = 1)
weather_table_6pm_only <- weather_table %>% filter(hour(obsTimeUtc) == 18) %>% mutate(date = as_date(obsTimeUtc)) %>% group_by(stationID, date) %>% 
                          summarise(temp = mean(metric.tempAvg), dew_point = mean(metric.dewptAvg), humidity = mean(humidityAvg), 
 
                                                                       wind_dir = mean(winddirAvg), wind_spd = mean(metric.windspeedAvg) , pressure = mean(metric.pressureMax))

air_density <- function(temp_C, rel_humidity, pressure)
{

# T_C <- temp_C
# T_K <- T_C + 273.15  #kelvins
# RH <- rel_humidity
# p <- pressure

pv <- (6.1078 * 10^(  (7.5 * temp_C) / (T + 237.3) ) )* rel_humidity
pd <- pressure - pv

density = 100  * ((pd / (287.058 * (T_C + 273.15))) + (pv / (461.495 * (T_C + 273.15))) )

return(density)

}

digits (air_density( 10 , 0.8 , 1050 ))


