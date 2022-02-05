library(gender)
library(tidyverse)
library(magrittr)
library(tidyselect)
library(here)
library(stringr)
library(here)

gender_names_genderize_corrected <- read_csv(here("Data/Misc", "gender_names_genderize_corrected.csv"), col_select = c(name,gender))

ctta_roster <- as_tibble(unique(unlist(ctta_results$rider_name))) 

ctta_roster <- as_tibble(str_split_fixed(ctta_roster$value," ", 2)) %>% 
              bind_cols(ctta_roster) %>% 
              left_join(. , gender_names_genderize_corrected, by=c("V1" = "name")) %>% 
              rename("rider_name" = "value" , "rider_name_first" = "V1" , "rider_name_last" = "V2")
  
write_csv(ctta_roster, here("Data/Master Results", "ctta_roster.csv"))  

#make a unique list of names from the results
##calculate ctta_results with script to get latest list
names_list <- unique(unlist(ctta_results$rider_name)) %>% 
              str_split_fixed(. ," ", 2) %>% 
              as_tibble() %>% 
              rename("rider_name_first" = "V1" , "rider_name_last" = "V2")

write.csv(names_list, here("Data/Misc", "names_list.csv"))

#User Genderize method 
gender_names_genderize <- gender(names_list$rider_name_first, method = "genderize")
write.csv(gender_names_genderize, here("Data/Misc", "gender_names_genderize.csv"))

#Use united states Social Security method database
gender_names_SSA <- gender(names_list$rider_name_first, years = c(1960,2012))
write.csv(gender_names_genderize, here("Data/Misc", "gender_names_SSA.csv"))





