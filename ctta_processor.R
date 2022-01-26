library(tidyverse)
library(hms)
library(lubridate)
library(magrittr)
library(vroom)
library(tidyselect)
library(janitor)
library(readxl)
library(tidytable)
library(naniar)
library(data.table)
library(here)
library(zoo)
library(ggplot2)

i_am("ctta_processor.R")

#--------Cleaned Results Processor--------------------------

speed_fxn <- function(dist, time)
{
  as_hour <- lubridate::hour(time)+lubridate::minute(time)/60+lubridate::second(time)/3600
  
  speed_kph <- dist / as_hour 
  
  return(speed_kph)
}

season_fxn <- function(results)
{
  results_seasoned <- results %>%
                         mutate(season = 
                                case_when(date >= ymd("2010-6-01") & date < ymd("2011-6-01") ~ "2010-11 Season",
                                           date >= ymd("2011-6-01") & date < ymd("2012-6-01") ~ "2011-12 Season",
                                           date >= ymd("2012-6-01") & date < ymd("2013-6-01") ~ "2012-13 Season",
                                           date >= ymd("2013-6-01") & date < ymd("2014-6-01") ~ "2013-14 Season",
                                           date >= ymd("2014-6-01") & date < ymd("2015-6-01") ~ "2014-15 Season",
                                           date >= ymd("2015-6-01") & date < ymd("2016-6-01") ~ "2015-16 Season",
                                           date >= ymd("2016-6-01") & date < ymd("2017-6-01") ~ "2016-17 Season",
                                           date >= ymd("2017-6-01") & date < ymd("2018-6-01") ~ "2017-18 Season",
                                           date >= ymd("2018-6-01") & date < ymd("2019-6-01") ~ "2018-19 Season",
                                           date >= ymd("2019-6-01") & date < ymd("2020-6-01") ~ "2019-20 Season",
                                           date >= ymd("2020-6-01") & date < ymd("2021-6-01") ~ "2020-21 Season",
                                           date >= ymd("2021-6-01") & date < ymd("2022-6-01") ~ "2021-22 Season",
                                           TRUE ~ "out_of_range"))
  return(results_seasoned)
}

air_density <- function(temp_C, rel_humidity, pressure)
{
  
  # T_C <- temp_C
  # T_K <- T_C + 273.15  #kelvins
  # RH <- rel_humidity
  # p <- pressure
  
  pv <- (6.1078 * 10^(  (7.5 * temp_C) / (T + 237.3) ) )* rel_humidity
  pd <- pressure - pv
  
  density <- 100  * ((pd / (287.0531 * (temp_C + 273.15))) + (pv / (461.4964 * (temp_C + 273.15))) )
  
  return(density)
  
}

#for air density quick calc
air_density(21,.70,1002)


#Time Data read in
#ctta_results <- read_csv(here("TT dump/Cleaned Results", "ctta_results.csv")) %>% clean_names() %>% remove_empty(which = c("rows", "cols"))
ctta_results <- read_xlsx(here("TT dump/Cleaned Results", "tt_results_master.xlsx")) %>% clean_names() %>% remove_empty(which = c("rows", "cols"))


#change time to be HMS format
ctta_results$time <- hms::as_hms(ctta_results$time_sec)

ctta_results$time <- round_hms(hms::as_hms(ctta_results$time), digits = 2)

#calculate speeds if speed = 0 , i.e. speed was not recorded in source data
ctta_results <- ctta_results %>% 
                mutate(speed = case_when(speed_bit == 0 ~ speed_fxn(dist_km , time),
                                                          TRUE ~ speed))

#split and remerge data to add positions
ctta_results_pos_1 <- ctta_results %>% 
                      dplyr::filter(position_bit == 1) 

ctta_results_pos_0 <- ctta_results %>%
  dplyr::filter(position_bit == 0) %>% 
  arrange(date, dist_km, desc(speed)) %>% 
  group_by(date, dist_km, event) %>% 
  mutate(position =  1:n()) %>% 
  ungroup() 

ctta_results <- full_join(ctta_results_pos_1, ctta_results_pos_0) %>% 
                mutate(speed = round(speed, digits = 1))

rm(ctta_results_pos_0 , ctta_results_pos_1 )

#Add season
ctta_results <- season_fxn(ctta_results)

#Write Data to CSV
min_year <-  min(lubridate::year(ctta_results$date))
max_year <-  max(lubridate::year(ctta_results$date))

write_csv(ctta_results, paste0("ctta_results_",min_year,"-",max_year,".csv"))

#---------Temperature merge and creation------------

temperature_data_lincoln <- vroom(here("TT dump/weather/wind and temp/Temp", "2010_2021_lincoln_weather_temp.txt")) %>%
  clean_names() %>%  
  distinct() %>% 
  unite(date_hr, date:hr, sep = " ") %>% 
  mutate(date_hr = dmy_h(date_hr))

wind_data_lincoln <- vroom(here("TT dump/weather/wind and temp/Wind", "2010_2021_lincoln_weather_wind.txt")) %>%
  clean_names() %>%  
  distinct() %>% 
  unite(date_hr, date:hr, sep = " ") %>% 
  mutate(date_hr = dmy_h(date_hr))

pressure_data_lincoln <- vroom(here("TT dump/weather/wind and temp/pressure", "2017_2021_pressure_lincoln.txt")) %>%
  clean_names() %>%  
  distinct() %>% 
  unite(date_hr, date:hr, sep = " ") %>% 
  mutate(date_hr = ymd_h(date_hr)) %>% 
  rename(pmsl_h_pa_linc = pmsl_h_pa)

weather_lincoln <- full_join(temperature_data_lincoln, wind_data_lincoln, by = c("date_hr","station")) %>% 
                   full_join( . , pressure_data_lincoln, by = c("date_hr","station"))


pressure_data_chch <- vroom(here("TT dump/weather/wind and temp/pressure", "2010_2021_pressure_chch.txt")) %>%
  clean_names() %>%  
  distinct() %>% 
  unite(date_hr, date:hr, sep = " ") %>% 
  mutate(date_hr = ymd_h(date_hr)) %>% 
  select(-station) %>% 
  rename(pmsl_h_pa_chch = pmsl_h_pa)



weather_combined <- full_join(weather_lincoln, pressure_data_chch, by = c("date_hr")) %>% 
                    mutate(pressure = case_when(is.na(pmsl_h_pa_linc) == TRUE ~ pmsl_h_pa_chch, TRUE ~ pmsl_h_pa_linc)) %>% 
                    arrange(date_hr) %>% 
                    mutate(tair_c = na.approx(tair_c)) %>% 
                    mutate(pressure = na.approx(pressure, na.rm = FALSE)) %>%  #ignore N/A's at end due to embargo'd data
                    replace_with_na_at(. , .vars = "rh_percent", condition = ~.x == "-") %>% 
                    mutate(rh_percent = na.approx(rh_percent)) %>% 
                    filter(is.na(pressure) != TRUE) %>% #remove embargoed pressure values
                    mutate(density = air_density(tair_c, rh_percent/100, pressure) )          
                    
tai_tapu_dates <- ctta_results %>% filter(location == "Tai Tapu") %>% select(date) %>% distinct() 

weather_combined_filtered <- weather_combined %>% 
                              select(date_hr,tair_c,rh_percent,dir_deg_t,speed_km_hr,pressure,density) %>%  
                              filter(as_date(date_hr) %in% as_date(tai_tapu_dates$date)) %>% 
                              filter(hour(date_hr) > 17 , hour(date_hr) < 20) %>% 
                              group_by(as_date(date_hr)) %>% 
                              summarise_all(mean) %>% 
                              select(-date_hr) %>% 
                              rename(date = 'as_date(date_hr)') %>% 
                              mutate_at(vars(tair_c, rh_percent, speed_km_hr, pressure,dir_deg_t), round, 0) %>% 
                              mutate_at(vars(density), round, 3)

#Mutate Wind Direction Labels
rose_breaks <- c(0, 360/16, (1/16 + (1:7 / 8)) * 360, 360)

rose_labs <- c(
  "North", "Northeast",
  "East", "Southeast", 
  "South",  "Southwest", 
  "West",  "Northwest",
  "North")

weather_combined_filtered <- weather_combined_filtered %>% 
  mutate(wd_cardinal = cut(
    dir_deg_t, 
    breaks = rose_breaks, 
    labels = rose_labs
  ))

write_csv(weather_combined_filtered, paste0("tai_tapu_weather",min(lubridate::year(weather_combined_filtered$date)),"-",max(lubridate::year(weather_combined_filtered$date)),".csv"))






weather_combined_afternoons <- subset(weather_combined, date(date) == (tai_tapu_dates$date) ) %>% filter(hour(date_hr) > 17 , hour(date_hr) < 20) 

weather_combined_afternoons <- weather_combined %>%  filter(hour(date_hr) > 17 , hour(date_hr) < 20) %>% filter(month(date_hr) != 5, month(date_hr) != 6, month(date_hr) != 7, month(date_hr) != 8, month(date_hr) != 9 )



S <- ggplot(weather_combined_afternoons, aes(density)) +
                     geom_histogram(binwidth = .005)


#String distance matching for similar names comparison---------------

library(stringdist)
a <- ctta_results %>%  distinct(rider_name) 
b = c(NA) 
df = data.frame(a$rider_name,b, stringsAsFactors = FALSE)
mat <- stringdistmatrix(df$a.rider_name, df$a.rider_name)
mat[mat==0] <- NA # ignore self
mat[mat>4] <- NA  # cut level
amatch <- rowSums(mat, na.rm = TRUE)>0 # ignore no match
df$b[amatch] <- df$a[apply(mat[amatch,],1,which.min)]
write.csv(df, "names_match.csv")

#check to see if chch presure is the same as lincoln pressure... pretty good +/-2.0
pressure_comp <- full_join(pressure_data_lincoln, pressure_data_chch, by = c("date_hr")) %>% mutate(diff = pmsl_h_pa.x - pmsl_h_pa.y)









