# load packages

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


#### Plots ####

## all movement ##

rkm_tracker_date <- read_csv("data/rkm_tracker_date.csv")

All_Individuals <- rkm_tracker_date %>% 
  group_by(transmitter_id) %>% 
  mutate(color = case_when(transmitter_id==48456~"limegreen",
                           transmitter_id==48478~"lightblue",
                           transmitter_id==48491~"pink",
                           transmitter_id==48727~"purple",
                           transmitter_id==48728~"red")) %>% 
  ggplot(aes(color=as.character(color), group=transmitter_id,x=date, y=distance))+
  geom_line(alpha=0.8)+
  geom_point(shape=20, size=0.5, alpha=0.8) +
  labs(y = "Distance Traveled, rkm",
       x = "") +
  theme_linedraw()+
  theme(legend.position="none")+
  ylim(-475,200)+
  ggtitle("Telemetered Silver Carp Movement Over Time")
ggsave(All_Individuals, file = "plots/AllIndividuals.png", dpi = 750, width = 5, height = 4,
       units = "in")


# Detections per day for all tags, with axis break
AllDailyDetections <- rkm_tracker_date %>% 
  ggplot(aes(x=date, y=transmitter_id, group=transmitter_id))+
  geom_line(color="light blue")+
  geom_point(shape=16) +
  labs(y = "Telemetered Individual ID #",
       x = "") +
  theme_bw()+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(48452, 48733))+
  scale_y_break(c(48491, 48724), scales = 0.25) +
  scale_y_continuous(breaks=seq(48452, 48733, 2))+
  # ylim(48452, 48491)+
  ggtitle("Silver Carp Daily Detections",
          subtitle = "June 2021 - October 2022")
ggsave(AllDailyDetections, file = "plots/AllDailyDetections.png", dpi = 750, width = 7, height = 6,
       units = "in")

##Maximum minimum distance traveled by all individuals
## I don't think this is correct, or it only shows the maximum movement in one go
max_dist <- rkm_tracker_date %>% 
  group_by(transmitter_id) %>% 
  mutate(max_rkm = max(rkm),
         min_rkm = min(rkm)) %>% 
  distinct(transmitter_id, max_rkm, min_rkm) %>% 
  mutate(distance_max = max_rkm - min_rkm)

mean(max_dist$distance_max)
median(max_dist$distance_max)

MaxDistancePlot <- ggplot(max_dist, aes(x = reorder(as.factor(transmitter_id), -distance_max), y = distance_max )) + 
  geom_point() +
  labs(x="",
       y="Maximum Total Distance Traveled (rkm)",
       title = "Maximum Silver Carp Movement per Individual")+
  geom_hline(data=max_dist,aes(yintercept=mean(distance_max)), color="purple")+
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.ticks = element_blank())

ggsave(MaxDistancePlot,file="plots/MaxDistancePlot.jpg", dpi = 750, width = 4.5, height = 3,
       units = "in")

## absolute movement

abs_mvmt <-rkm_tracker_date %>% mutate(abs_dist=abs(distance)) %>% arrange(transmitter_id, date) %>% 
  group_by(transmitter_id) %>% 
  mutate(total_movement = abs(rkm - lag(rkm,1))) %>% 
  group_by(transmitter_id) %>% 
  summarise(total_movement=sum(total_movement, na.rm = T))
# need to remove active tracking fish

sum(abs_mvmt$total_movement)
mean(abs_mvmt$total_movement)


TotalMovementPlot <- ggplot(abs_mvmt, aes(x = reorder(as.factor(transmitter_id),-total_movement), y = total_movement )) + 
  geom_point() +
  labs(x="",
       y="Total Distance Traveled (rkm)",
       title = "Silver Carp Movement by Individual")+
  # geom_hline(data=abs_mvmt,aes(yintercept=mean(total_movement)), color="purple")+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.ticks = element_blank())

mean(abs_mvmt$total_movement)
max(abs_mvmt$total_movement)

ggsave(TotalMovementPlot,file="plots/TotalMovementPlot.jpg", dpi = 750, width = 6, height = 5,
       units = "in")

## mean total distance moved, not including zeros
max_dist_nozero <-abs_mvmt %>% filter(total_movement > 1)


ggplot(max_dist_nozero, aes(x = reorder(as.factor(transmitter_id), -total_movement), y = total_movement )) + 
  geom_point() +
  labs(x="",
       y="Maximum Total Distance Traveled (rkm)",
       title = "Maximum Silver Carp Movement per Individual")+
  geom_hline(data=max_dist,aes(yintercept=mean(distance_max)), color="purple")+
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.ticks = element_blank())

mean(max_dist_nozero$total_movement)
# n = 31 fish


## Cumulative Movement Plot ##

cumulative_abs_movement <- read_csv("data/cumulative_abs_movement.csv")

mover_data=cumulative_abs_movement %>% filter(max_cumulative_mvmt>50)
Mover_Plot <- ggplot(data=mover_data,aes(x=date, y=cumulative_movement_s))+
  geom_line(aes(group=transmitter_id))+
  labs(x="Date",
       y="Proportion of cumulative movement",
       title="Cumulative Movement of Tagged Individuals")
ggsave(Mover_Plot,file="plots/MoverPlot.jpg", dpi = 750, width = 5, height = 4,
       units = "in")


#### Plot of all fish #####
# ONLY includes instances from: release date and dates fish pinged at receiver.
# NO active tracking points

All_Fish <- ggplot(rkm_tracker_date, aes(x=date, y=rkm, group=transmitter_id, color=as.character(transmitter_id)))+
  geom_line()+ labs(title="James River Silver Carp Detections", subtitle ="June 2021 - October 2022.", 
                    caption = "Grey horizontal lines represent stationary receiver locations")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)+
  guides(color="none")+
  labs(x=NULL)

ggsave(All_Fish, file = "plots/All_Fish.png", dpi = 750, width = 7, height = 5,
       units = "in")
  # facet_wrap(vars(transmitter_id))