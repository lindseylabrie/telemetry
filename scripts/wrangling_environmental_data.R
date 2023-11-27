
library(Matrix)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(ggplot2)
library(ggbreak)
library(brms)
library(tidybayes)

## Wrangling Environmental Data ##

# James River Flow Data

scotland_change <- read_csv("data/environmental_data/gage_heights/scotland.csv") %>% 
  mutate(change_flow_24=Flow-lag(Flow),
         change_flow_48=Flow-lag(Flow,n=2),
         date_prev=Date) %>% 
  distinct(date_prev,change_flow_24,change_flow_48, Flow)

# James River Temperature Data

temp <- read_excel("data/environmental_data/temperature/Hwy_50_Temp_10_12_22.xlsx") %>% clean_names() %>% 
  mutate(date_prev=as.Date(date_time_gmt_05_00),
         week=week(date_prev)) %>% 
  group_by(week) %>% 
  summarize(mean_temp=mean(temp_f,na.rm=TRUE))

temp_long <-read_excel("data/environmental_data/temperature/Hwy_50_Temp_10_12_22.xlsx") %>% clean_names() %>% 
  mutate(date_prev=as.Date(date_time_gmt_05_00)) %>% 
  mutate(temp_c=(temp_f-32)*.5556)

James_Temp_Plot <- ggplot(temp_long, aes(x=date_prev, y=temp_c))+
  geom_smooth()+
  labs(title="James River Temperature",
       x="Date",
       y="Temperature in Celsius")

ggsave(James_Temp_Plot,file="plots/JamesTemp.jpg", dpi = 750, width = 7, height = 6,
       units = "in")


# James River Dissolved Oxygen Data

DO <- read_csv("data/environmental_data/dissolved_oxygen/JamesRiverDOsensor.csv") %>% clean_names() %>% 
  filter(!is.na(dissolved_oxygen)) %>% 
  mutate(date=as.Date(timestamp_utc)) %>% 
  group_by(date) %>% 
  summarize(mean_do=mean(dissolved_oxygen))

DO_Plot <- ggplot(DO, aes(x=date, y=mean_do))+
  geom_smooth()+
  labs(title="James River Dissolved Oxygen",
       x="October 2021-August 2022",
       y="Dissolved Oxygen (percent)")

ggsave(DO_Plot,file="plots/JamesDO.jpg", dpi = 750, width = 4, height = 4,
       units = "in")

do <- read_csv("data/environmental_data/dissolved_oxygen/do.csv")

do %>% ggplot(aes(x=week, y=mean_do))+
  geom_point()+
  geom_smooth()
