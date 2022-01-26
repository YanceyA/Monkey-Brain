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
library(scales)

TT_16km <- ctta_results %>% filter(dist_km == 16) %>% filter(event == "Tai Tapu TT") %>% 
          filter(category == "Solo") %>%  mutate(time = as.numeric(seconds(time))) %>% 
          filter(time < 4000, time > 1 ) %>% filter(rider_name != "Wayne Wardell")

TT_16km <- TT_16km %>% group_by(date) %>% filter( time <= quantile(time, 0.99), time >= quantile(time, 0.01)) %>% summarize( time = mean(time) )


TT_16km <- TT_16km %>% group_by(date) %>% summarize( time = mean(time) )


mean <- ggplot(TT_16km, aes(x = date, y = time)) +
        geom_point() +
        scale_x_datetime(breaks = scales::breaks_width("1 year"), date_labels = "%Y") +
        geom_smooth() +
        geom_vline(xintercept = as.POSIXct(as.Date("2017-11-01")), color = "red", 
                   lwd = 1)

