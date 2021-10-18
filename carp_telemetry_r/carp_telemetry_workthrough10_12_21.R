library(readr)
library(tidyverse)
library(lubridate)
library(janitor)

Receiver16_10_05_21 <- read_csv("~/Desktop/telemetry_data/Receiver16_10_05_21.csv")
Receiver15_10_05_21 <- read_csv("~/Desktop/telemetry_data/Receiver15_10_05_21.csv")
Receiver17_10_05_21 <- read_csv("~/Desktop/telemetry_data/Receiver17_10-05_21.csv")
Receiver19_10_05_21 <- read_csv("~/Desktop/telemetry_data/Receiver19_10_05_21.csv")

all_data <- bind_rows(Receiver15_10_05_21, 
                      Receiver16_10_05_21,
                      Receiver17_10_05_21,
                      Receiver19_10_05_21) %>% 
  clean_names %>% 
  separate(transmitter, c("freq1", "freq2", "transmitter_id")) %>% 
  mutate(transmitter_id = as.numeric(transmitter_id)) %>% 
  filter(transmitter_id < 50000) %>% 
  mutate(date = ymd(as.Date(date_and_time_utc))) %>%  
  select(date, station_name,transmitter_id) %>% 
  distinct(station_name,transmitter_id, date)
