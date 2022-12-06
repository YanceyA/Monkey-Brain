#Package Coverage Test:
# library(rstudioapi)
# library(NCmisc)
# list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)

library(magrittr)
library(readr)
library(readxl)
library(janitor)
library(here)
library(hms)
library(dplyr)
library(lubridate)
library(stringr)
library(stringdist)
library(clifro)
library(gender)
library(tibble)
library(tidyr)

source("data_processor_functions.R")

i_am("data_processor.R")

#temp process to add in file from shiny app
#todays_results <- read_csv(here("updated_data.csv")) %>% mutate(record_note = as.character(record_note)) %>% mutate(date = as.Date(date))
#ctta_results <- full_join(ctta_results, todays_results, by = NULL)

#check results and roster file for newnames and write new (CSV and RDS) if TRUE
new_riders_roster <- new_name_check(write_files = FALSE)
ctta_roster <- read_csv(here("data" , "ctta_roster.csv"))

#Read in results and roster file, process data, and save to RDS files for shiny app.
ctta_results <- read_xlsx(here("data", "tt_results_master.xlsx"), guess_max = 100000) %>% clean_names() %>% remove_empty(which = c("rows", "cols"))

#Process tt_results into single table for shiny page
tt_results <- process_raw_tt_data(results = ctta_results , roster = ctta_roster )
tt_results <- add_age_group(tt_results) #add age group to records

#Read in weather file, process data, and save to RDS files for shiny app.
tt_weather <- readr::read_csv(here("data","tai_tapu_weather2010-2022.csv")) #%>% dplyr::mutate(date = dmy(date))
tt_weather <- weather_update(tt_results, tt_weather) %>% full_join(tt_weather, . , c("date", "tair_c", "rh_percent", "dir_deg_t", "speed_km_hr", "pressure", "density", "wd_cardinal") )
write.csv(tt_weather, file = here("data/tai_tapu_weather2010-2022.csv"), row.names = FALSE)

#write binary files to TT Analysis Directory
saveRDS(tt_weather, file = here("data/tai_tapu_weather2010-2022.RDS"))
saveRDS(tt_results, file = here("data/tt_results.RDS"))

#----HOME-------
#write binary files to shiny app directory when home
saveRDS(tt_results, file = here("C:/Users/yance/OneDrive - Allied Motion Technologies Inc/Desktop/R and BI Projects/CTTA Shiny App/data/tt_results.RDS"))
saveRDS(tt_weather, file = here("C:/Users/yance/OneDrive - Allied Motion Technologies Inc/Desktop/R and BI Projects/CTTA Shiny App/data/tai_tapu_weather2010-2022.RDS"))

#------WORK--------
#write binary files to shiny app directory when at work
saveRDS(tt_results, file = here("C:/Users/Yancey.Arrington/OneDrive - Allied Motion Technologies Inc/Desktop/R and BI Projects/CTTA Shiny App/data/tt_results.RDS"))
saveRDS(tt_weather, file = here("C:/Users/Yancey.Arrington/OneDrive - Allied Motion Technologies Inc/Desktop/R and BI Projects/CTTA Shiny App/data/tai_tapu_weather2010-2022.RDS"))



#----------Trash code---------------
#ctta_roster <- read_csv(here("data", "ctta_roster.csv")) 
#%>% mutate(date_added_to_roster = dmy(date_added_to_roster)) #only required if the dates need to be reformated from text dmy
#write_csv(ctta_roster, file = here("data/ctta_roster.csv") )
#saveRDS(ctta_roster, file = here("data/ctta_roster.RDS"))




