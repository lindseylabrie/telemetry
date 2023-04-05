# Load packages
library(Matrix)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(ggplot2)
library(ggbreak)

# Data

# Receiver 303
receiver303_10_22_21 <- read_csv("receiver_data/Receiver303_10_22_21.csv")
receiver303_5_13_22 <- read_csv("receiver_data/Receiver303_5_13_22.csv")
receiver303_8_25_22 <- read_csv("receiver_data/Receiver303_8_25_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver303_10_12_22 <- read_csv("receiver_data/Receiver303_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver303 <- bind_rows(receiver303_10_22_21, receiver303_5_13_22,receiver303_8_25_22,receiver303_10_12_22)

# Receiver 1
receiver1_11_16_21 <- read_csv("receiver_data/Receiver1_11_16_21.csv") 
receiver1_5_11_22 <- read_csv("receiver_data/Receiver1_5_11_22.csv") 
receiver1_8_17_22 <- read_csv("receiver_data/Receiver1_8_17_22.csv") %>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver1_10_13_22 <- read_csv("receiver_data/Receiver1_10_13_2022.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver1 <- bind_rows(receiver1_11_16_21,receiver1_5_11_22,receiver1_8_17_22,receiver1_10_13_22)

# Receiver 2

receiver2_11_16_21 <- read_csv("receiver_data/Receiver2_11_16_21.csv")
receiver2_5_11_22 <- read_csv("receiver_data/Receiver2_5_11_22.csv")
receiver2_8_17_22 <- read_csv("receiver_data/Receiver2_8_17_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver2_10_13_22 <- read_csv("receiver_data/Receiver2_10_13_2022.csv")
receiver2 <- bind_rows(receiver2_11_16_21, receiver2_5_11_22, receiver2_8_17_22, receiver2_10_13_22)

# Receiver 3
receiver3_10_22_21 <- read_csv("receiver_data/Receiver3_10_22_21.csv")
receiver3_5_10_22 <- read_csv("receiver_data/Receiver3_5_10_22.csv")
receiver3_8_8_22 <- read_csv("receiver_data/Receiver3_8_8_22.csv")
receiver3_10_12_22 <- read_csv("receiver_data/Receiver3_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver3 <- bind_rows(receiver3_10_22_21, receiver3_5_10_22,receiver3_8_8_22,receiver3_10_12_22)

# Receiver 4
receiver4_10_22_21 <- read_csv("receiver_data/Receiver4_10_22_21.csv")
receiver4_5_18_22 <- read_csv("receiver_data/Receiver4_5_18_22.csv")
receiver4_8_8_22 <- read_csv("receiver_data/Receiver4_8_8_22.csv")
receiver4_10_12_22 <- read_csv("receiver_data/Receiver4_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver4 <- bind_rows(receiver4_10_22_21, receiver4_5_18_22, receiver4_8_8_22,receiver4_10_12_22)

# Receiver 5
receiver5_5_16_22 <- read_csv("receiver_data/Receiver5_5_16_22.csv")
receiver5_8_25_22 <- read_csv("receiver_data/Receiver5_8_25_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver5_10_12_22 <- read_csv("receiver_data/Receiver5_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver5 <- bind_rows(receiver5_5_16_22,receiver5_8_25_22,receiver5_10_12_22)

# Receiver 6
receiver6_10_22_21 <- read_csv("receiver_data/Receiver6_10_22_21.csv")
receiver6_5_16_22 <- read_csv("receiver_data/Receiver6_5_16_22.csv")
receiver6_8_8_22 <- read_csv("receiver_data/Receiver6_8_8_22.csv")
receiver6_10_12_22 <- read_csv("receiver_data/Receiver6_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver6 <- bind_rows(receiver6_10_22_21, receiver6_5_16_22,receiver6_8_8_22,receiver6_10_12_22)


# Receiver 7
receiver7_10_22_21 <- read_csv("receiver_data/Receiver7_10_22_21.csv")
receiver7_5_10_22 <- read_csv("receiver_data/Receiver7_5_10_22.csv")
receiver7_8_8_22 <- read_csv("receiver_data/Receiver7_8_8_22.csv")
receiver7_10_12_22 <- read_csv("receiver_data/Receiver7_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver7 <- bind_rows(receiver7_5_10_22, receiver7_10_22_21,receiver7_8_8_22,receiver7_10_12_22)

# Receiver 8
# receiver 8 was removed and switched to in-stream because of silting in fall 2021
receiver8_10_22_21 <- read_csv("receiver_data/Receiver8_10_22_21.csv")
receiver8 <- receiver8_10_22_21 

# Receiver 9
receiver9_10_22_21 <- read_csv("receiver_data/Receiver9_10_22_21.csv") # no data
receiver9_5_17_22 <- read_csv("receiver_data/Receiver9_5_17_22.csv")
receiver9_8_8_22 <- read_csv("receiver_data/Receiver9_8_8_22.csv")
receiver9_10_12_22 <- read_csv("receiver_data/Receiver9_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver9 <- bind_rows(receiver9_5_17_22,receiver9_8_8_22,receiver9_10_12_22)

# Receiver 10
receiver10_10_22_21 <- read_csv("receiver_data/Receiver10_10_22_21.csv") # no data
receiver10_5_17_22 <- read_csv("receiver_data/Receiver10_5_17_22.csv")
receiver10_8_8_22 <- read_csv("receiver_data/Receiver10_8_8_22.csv")
receiver10_10_12_22 <- read_csv("receiver_data/Receiver10_10_12_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver10 <- bind_rows(receiver10_5_17_22,receiver10_8_8_22,receiver10_10_12_22)

# Receiver 11
receiver11_10_14_21 <- read_csv("receiver_data/Receiver11_10_14_21.csv")
# receiver 11 (in stream) has not been accessed yet for spring 2022, sent off to innovasea
# try to get data offloaded from vemco email, otherwise nbd if not
receiver11 <- receiver11_10_14_21

# Receiver 12
receiver12_10_14_21 <- read_csv("receiver_data/Receiver12_10_14_21.csv") # no carp on this one
receiver12_6_1_22 <- read_csv("receiver_data/Receiver12_6_1_22.csv")
receiver12_7_11_22 <- read_csv("receiver_data/Receiver12_7_11_22.csv")
receiver12_8_4_22 <- read_csv("receiver_data/Receiver12_8_4_22.csv") %>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))%>% 
  select(-Time)
receiver12_8_18_22 <- read_csv("receiver_data/Receiver12_8_18_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`)) %>% 
  select(-Time)
receiver12_8_22_22 <- read_csv("receiver_data/Receiver12_8_22_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver12_10_10_22 <- read_csv("receiver_data/Receiver12_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))

receiver12 <- bind_rows(receiver12_10_14_21,receiver12_6_1_22,receiver12_7_11_22,receiver12_8_4_22,
                        receiver12_8_18_22,receiver12_8_22_22,receiver12_10_10_22)

# Receiver 13
receiver13_10_14_21 <- read_csv("receiver_data/Receiver13_10_14_21.csv")
receiver13_8_25_22 <- read_csv("receiver_data/Receiver13_8_25_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver13_10_10_22 <- read_csv("receiver_data/Receiver13_10_10_22.csv") # nodata
receiver13 <- bind_rows(receiver13_10_14_21,receiver13_8_25_22)

# Receiver 14
receiver14_10_14_21 <- read_csv("receiver_data/Receiver14_10_14_21.csv")
receiver14_6_1_22 <- read_csv("receiver_data/Receiver14_6_1_22.csv")
receiver14_6_29_22 <- read_csv("receiver_data/Receiver14_6_29_22.csv")
receiver14_7_5_22 <- read_csv("receiver_data/Receiver14_7_5_22.csv")
receiver14_10_10_22 <- read_csv("receiver_data/Receiver14_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver14 <- bind_rows(receiver14_10_14_21,receiver14_6_1_22,receiver14_6_29_22,receiver14_7_5_22,receiver14_10_10_22)

# Receiver 15
receiver15_10_05_21 <- read_csv("receiver_data/Receiver15_10_05_21.csv")
receiver15_6_1_22 <- read_csv("receiver_data/Receiver15_6_1_22.csv")
receiver15_10_10_22 <- read_csv("receiver_data/Receiver15_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver15 <- bind_rows(receiver15_10_05_21,receiver15_6_1_22,receiver15_10_10_22)

# Receiver 16
receiver16_10_05_21 <- read_csv("receiver_data/Receiver16_10_05_21.csv")
receiver16_8_25_22 <- read_csv("receiver_data/Receiver16_8_25_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver16_10_10_22 <- read_csv("receiver_data/Receiver16_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver16 <- bind_rows(receiver16_10_05_21,receiver16_8_25_22,receiver16_10_10_22)

# Receiver 17
receiver17_10_05_21 <- read_csv("receiver_data/Receiver17_10-05_21.csv")
receiver17_5_10_22 <- read_csv("receiver_data/Receiver17_5_10_22.csv") 
receiver17_10_10_22 <- read_csv("receiver_data/Receiver17_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver17 <- bind_rows(receiver17_10_05_21,receiver17_5_10_22,receiver17_10_10_22)

# Receiver 18
receiver18_5_10_22 <- read_csv("receiver_data/Receiver18_5_10_22.csv")
receiver18_10_10_22 <- read_csv("receiver_data/Receiver18_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver18 <- bind_rows(receiver18_5_10_22,receiver18_10_10_22)

# Receiver 19
receiver19_10_05_21 <- read_csv("receiver_data/Receiver19_10_05_21.csv")
receiver19_10_10_22 <- read_csv("receiver_data/Receiver19_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver19 <- bind_rows(receiver19_10_05_21, receiver19_10_10_22)

# Receiver 20
receiver20_5_10_22 <- read_csv("receiver_data/Receiver20_5_10_22.csv")
receiver20_10_10_22 <- read_csv("receiver_data/Receiver20_10_10_22.csv")%>% 
  mutate(`Date and Time (UTC)` = mdy_hm(`Date and Time (UTC)`))
receiver20 <- bind_rows(receiver20_5_10_22,receiver20_10_10_22)

# silver carp release points, from June 2021
silver_carp_release_points <- read_excel("tracking_data/silver_carp_release_points.xlsx")

# receiver locations
all_vemco_receivers <- read_excel("receiver_data/all_vemco_receivers.xlsx") %>% 
  separate(name, c("receiver", "station_name")) %>% 
  mutate(station_name = as.numeric(station_name), 
         latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude))

all_data <- bind_rows(receiver1, 
                      receiver2, 
                      receiver3, 
                      receiver4, 
                      receiver5,
                      receiver6,
                      receiver7,
                      # receiver8, no data
                      receiver303,
                      receiver9,
                      receiver10,
                      receiver11,
                      receiver12,
                      receiver13,
                      receiver14,
                      receiver15,
                      receiver16,
                      receiver17,
                      receiver18,
                      receiver19,
                      receiver20) %>%
  clean_names %>% 
  mutate(transmitter_id = as.numeric(str_sub(transmitter, -5, -1))) %>% 
  filter(!is.na(transmitter_id)) %>%
  filter(transmitter_id < 49000 & transmitter_id >48000) %>% 
  mutate(date = ymd(as.Date(date_and_time_utc)),
         detected_by = as.character("stationary_receiver")) %>%  
  select(date, station_name,transmitter_id, detected_by) %>% 
  group_by(date, station_name, transmitter_id, detected_by) %>% 
  count() %>% 
  left_join(all_vemco_receivers)

write.csv(all_data, "all_data.csv")
write.csv(rkm_tracker_date, "rkm_tracker_date.csv")