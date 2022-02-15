# #Package Coverage Test:
# library(rstudioapi)
# library(NCmisc)
# list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)


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
                      dplyr::mutate(season = 
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

#Air density quick calc
air_density(18,.73,1021)

process_raw_tt_data <- function(results , roster) 
{
#Convert seconds column to time object and overwrite time column
results$time <- hms::as_hms(results$time_sec)

results$time <- round_hms(hms::as_hms(results$time), digits = 1)

#calculate speeds if speed = 0 , i.e. speed was not recorded in source data
results <- results %>% 
           dplyr::mutate(speed = case_when(speed_bit == 0 ~ speed_fxn(dist_km , time),
                                                          TRUE ~ speed))

#split and remerge data to add positions
results_pos_1 <- results %>% 
                      dplyr::filter(position_bit == 1) 

results_pos_0 <- results %>%
  dplyr::filter(position_bit == 0) %>% 
  dplyr::arrange(date, dist_km, desc(speed)) %>% 
  dplyr::group_by(date, dist_km, event) %>% 
  dplyr::mutate(position =  1:n()) %>% 
  dplyr::ungroup() 

results <- full_join(results_pos_1, results_pos_0) %>% 
                mutate(speed = round(speed, digits = 1))

#remove intermediate dataframes
rm(results_pos_0 , results_pos_1 )

#Add season to data
results <- season_fxn(results)

#add gender and age group to results, taken from roster
results <- left_join(results, roster, by= "rider_name") %>% dplyr::mutate( rider_name_abbv = paste0(rider_name_first," ",str_sub(rider_name_last , 1 , 1),".") )

#Determine min/max year for file naming
min_year <-  min(lubridate::year(results$date))
max_year <-  max(lubridate::year(results$date))

#Write Data to CSV for Shiny program
write_csv(results, here("Data/Master Results", paste0("ctta_results_",min_year,"-",max_year,".csv")))

return(results)
}

#####Name Match code deactivated

#Function for string distance matching for similar names comparison
#match_level = 1 strict (1char diff) / 5 loose. 2-3 best setting 
name_match <- function(name_list, match_level)
{
  
  a <- unique(unlist(name_list)) 
  
  b = c(NA) 
  
  df = data.frame(a,b, stringsAsFactors = FALSE)
  
  mat <- stringdistmatrix(df$a, df$a)
  
  mat[mat==0] <- NA # ignore self
  
  mat[mat>match_level] <- NA  # cut level
  
  amatch <- rowSums(mat, na.rm = TRUE)>0 # ignore no match
  
  df$b[amatch] <- df$a[apply(mat[amatch,],1,which.min)]
  
  return(df)
  
}

#String distance matching for similar names comparison---------------
    # match_level <- 2
    # matched_names <- name_match(ctta_results$rider_name, match_level) %>%  filter(!is.na(b)) %>% arrange(a)
    # write.csv(matched_names, here("Data/Misc", paste0("matched_names_level_",match_level,".csv")))

add_age_group <- function(results)
{
  #requires results with column date_of_birth or year_of_birth
  
  results_with_age_group <- results %>% 
    dplyr::mutate(age_group = 
                    case_when(year(date) - year(date_of_birth) < 15 ~ "U15",
                              year(date) - year(date_of_birth) > 14 & year(date) - year(date_of_birth) < 17 ~ "U17",
                              year(date) - year(date_of_birth) > 16 & year(date) - year(date_of_birth) < 19 ~ "U19",
                              year(date) - year(date_of_birth) > 18 & year(date) - year(date_of_birth) < 23 ~ "U23",
                              year(date) - year(date_of_birth) > 22 & year(date) - year(date_of_birth) < 35 ~ "Elite-Senior",
                              year(date) - year(date_of_birth) > 34 & year(date) - year(date_of_birth) < 40 ~ "Masters 1",
                              year(date) - year(date_of_birth) > 39 & year(date) - year(date_of_birth) < 45 ~ "Masters 2",
                              year(date) - year(date_of_birth) > 44 & year(date) - year(date_of_birth) < 50 ~ "Masters 3",
                              year(date) - year(date_of_birth) > 49 & year(date) - year(date_of_birth) < 55 ~ "Masters 4",
                              year(date) - year(date_of_birth) > 54 & year(date) - year(date_of_birth) < 60 ~ "Masters 5",
                              year(date) - year(date_of_birth) > 59 & year(date) - year(date_of_birth) < 65 ~ "Masters 6",
                              year(date) - year(date_of_birth) > 64 & year(date) - year(date_of_birth) < 70 ~ "Masters 7",
                              year(date) - year(date_of_birth) > 69 & year(date) - year(date_of_birth) < 75 ~ "Masters 8",
                              year(date) - year(date_of_birth) > 74 & year(date) - year(date_of_birth) < 80 ~ "Masters 9",
                              year(date) - year(date_of_birth) > 79  ~ "Masters 10",
                              TRUE ~ "out_of_range"))
  
  return(results_seasoned)
}

new_name_check <-function()
{
  library(readxl)
  library(readr)
  library(magrittr)
  library(janitor)
  library(here)
  library(dplyr)
  
  results <- read_xlsx(here("Data/Master Results", "tt_results_master.xlsx")) %>% clean_names() %>% remove_empty(which = c("rows", "cols"))
  roster <- read_csv(here("Data/Master Results", "ctta_roster.csv"))
  missingnames <- anti_join(results, roster, by="rider_name")
  return(missingnames)
}

