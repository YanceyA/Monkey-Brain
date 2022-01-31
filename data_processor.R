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

#function to calcualte the speed based from distance and time vector
speed_fxn <- function(dist, time)
{
  as_hour <- lubridate::hour(time)+lubridate::minute(time)/60+lubridate::second(time)/3600
  
  speed_kph <- dist / as_hour 
  
  return(speed_kph)
}

#Function to apply the appropriate Season title to each record
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

#Detailed air density function based on temp, humidity and pressure
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

#Function for string distance matching for similar names comparison
#send this a vector of names
name_match <- function(name_list)
{
  library(stringdist)
  
  a <- name_list %>%  distinct() 

b = c(NA) 

df = data.frame(a,b, stringsAsFactors = FALSE)

mat <- stringdistmatrix(df$a, df$a)

mat[mat==0] <- NA # ignore self

mat[mat>4] <- NA  # cut level

amatch <- rowSums(mat, na.rm = TRUE)>0 # ignore no match

df$b[amatch] <- df$a[apply(mat[amatch,],1,which.min)]

return(df)

#write.csv(df, "names_match.csv")

}

#Air density quick calc
air_density(21,.70,1002)


#Master Results Data read in
ctta_results <- read_xlsx(here("Data/Master Results", "tt_results_master.xlsx")) %>% clean_names() %>% remove_empty(which = c("rows", "cols"))

#Convert seconds column to time object and overwrite time column
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

#remove intermediate dataframes
rm(ctta_results_pos_0 , ctta_results_pos_1 )

#Add season to data
ctta_results <- season_fxn(ctta_results)

#Determine min/max year for file naming
min_year <-  min(lubridate::year(ctta_results$date))
max_year <-  max(lubridate::year(ctta_results$date))

#Write Data to CSV for Shiny program
write_csv(ctta_results, here("Data/Master Results", paste0("ctta_results_",min_year,"-",max_year,".csv")))

#String distance matching for similar names comparison---------------












