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

#Weather test area
weather_combined_afternoons <- subset(weather_combined, date(date) == (tai_tapu_dates$date) ) %>% filter(hour(date_hr) > 17 , hour(date_hr) < 20) 
weather_combined_afternoons <- weather_combined %>%  filter(hour(date_hr) > 17 , hour(date_hr) < 20) %>% filter(month(date_hr) != 5, month(date_hr) != 6, month(date_hr) != 7, month(date_hr) != 8, month(date_hr) != 9 )
S <- ggplot(weather_combined_afternoons, aes(density)) +
  geom_histogram(binwidth = .005)


#check to see if chch presure is the same as lincoln pressure... pretty good +/-2.0
pressure_comp <- full_join(pressure_data_lincoln, pressure_data_chch, by = c("date_hr")) %>% mutate(diff = pmsl_h_pa.x - pmsl_h_pa.y)
