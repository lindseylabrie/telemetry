
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


#### Individual Fish Plots ####

# ONLY includes instances from: release date and dates fish pinged at the receivers
# NO active tracking points

# Fish that were not detected after release (NDAR):
# 48452, 48459, 48465, 48467, 48470, 48471, 48473, 48475, 48480, 
# 48481, 48486, 48489, 48490, 48724, 48729
# n = 15 fish with "undetermined" fate 

# Fish that had 0 net movement but whose fate is "alive"
# 48461, 48472, 48474, 48476
# n = 5 fish


## 48452 ## 
# Not detected after release (NDAR)
id_48452 <- subset(rkm_tracker_date, transmitter_id == 48452)
plot_48452 <- ggplot(id_48452, aes(x=date, y=rkm))+
  geom_point()+labs(title="ID # 48452")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48452, file = "individual_movement/48452.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48453 ##
id_48453 <- subset(rkm_tracker_date, transmitter_id == 48453)
plot_48453 <- ggplot(id_48453, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48453")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48453, file = "individual_movement/48453.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48454 ## 
id_48454 <- subset(rkm_tracker_date, transmitter_id == 48454)
plot_48454 <- ggplot(id_48454, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48454")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48454, file = "individual_movement/48454.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48455 ## 
id_48455 <- subset(rkm_tracker_date, transmitter_id == 48455)
plot_48455 <- ggplot(id_48455, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48455")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48455, file = "individual_movement/48455.png", dpi = 750, width = 7, height = 5,
       units = "in")


## 48456 ## 
id_48456 <- subset(rkm_tracker_date, transmitter_id == 48456)
plot_48456 <- ggplot(id_48456, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48456")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48456, file = "individual_movement/48456.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48457 ##
id_48457 <- subset(rkm_tracker_date, transmitter_id == 48457)
plot_48457 <- ggplot(id_48457, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48457")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48457, file = "individual_movement/48457.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48458 ##
id_48458 <- subset(rkm_tracker_date, transmitter_id == 48458)
plot_48458 <- ggplot(id_48458, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48458")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48458, file = "individual_movement/48458.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48459 ## NDAR
id_48459 <- subset(rkm_tracker_date, transmitter_id == 48459)
plot_48459 <- ggplot(id_48459, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48459")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48459, file = "individual_movement/48459.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48460 ## 
id_48460 <- subset(rkm_tracker_date, transmitter_id == 48460)
plot_48460 <- ggplot(id_48460, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48460")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48460, file = "individual_movement/48460.png", dpi = 750, width = 7, height = 5,
       units = "in")


## 48461 ## 
# Net zero movement
id_48461 <- subset(rkm_tracker_date, transmitter_id == 48461)
plot_48461 <- ggplot(id_48461, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48461")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48461, file = "individual_movement/48461.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48462 ## 
id_48462 <- subset(rkm_tracker_date, transmitter_id == 48462)
plot_48462 <- ggplot(id_48462, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48462")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48462, file = "individual_movement/48462.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48463 ##
id_48463 <- subset(rkm_tracker_date, transmitter_id == 48463)
plot_48463 <-ggplot(id_48463, aes(x=date, y=rkm ))+
  geom_point()+geom_line()+labs(title="Fish ID # 48463",)+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48463, file = "individual_movement/48463.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48464 ## 
id_48464 <- subset(rkm_tracker_date, transmitter_id == 48464)
plot_48464 <-ggplot(id_48464, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48464")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48464, file = "individual_movement/48464.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48465 ## 
#NDAR
id_48465 <- subset(rkm_tracker_date, transmitter_id == 48465)
plot_48465 <-ggplot(id_48465, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48465")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48465, file = "individual_movement/48465.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48466 ## 
id_48466 <- subset(rkm_tracker_date, transmitter_id == 48466)
plot_48466 <-ggplot(id_48466, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48466")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48466, file = "individual_movement/48466.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48467 ## 
#NDAR
id_48467 <- subset(rkm_tracker_date, transmitter_id == 48467)
plot_48467 <-ggplot(id_48465, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48467")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48467, file = "individual_movement/48467.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48468 ##
id_48468 <- subset(rkm_tracker_date, transmitter_id == 48468)
plot_48468 <-ggplot(id_48468, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48468")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48468, file = "individual_movement/48468.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48469 ##
id_48469 <- subset(rkm_tracker_date, transmitter_id == 48469)
plot_48469 <-ggplot(id_48469, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48469")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48469, file = "individual_movement/48469.png", dpi = 750, width = 7, height = 5,
       units = "in")


## 48470 ## 
# NDAR
id_48470 <- subset(rkm_tracker_date, transmitter_id == 48470)
plot_48470 <-ggplot(id_48470, aes(x=date, y=rkm ))+
  geom_point()+geom_line()+labs(title="ID # 48470")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48470, file = "individual_movement/48470.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48471 ## 
# NDAR
id_48471 <- subset(rkm_tracker_date, transmitter_id == 48471)
plot_48471 <-ggplot(id_48471, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48471")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48471, file = "individual_movement/48471.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48472 ##
id_48472 <- subset(rkm_tracker_date, transmitter_id == 48472)
plot_48472 <- ggplot(id_48472, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48472")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48472, file = "individual_movement/48472.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48473 ## 
# NDAR
id_48473 <- subset(rkm_tracker_date, transmitter_id == 48473)
plot_48473 <-ggplot(id_48473, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48473")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48473, file = "individual_movement/48473.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48474 ## 
# Net zero movement
id_48474 <- subset(rkm_tracker_date, transmitter_id == 48474)
plot_48474 <-ggplot(id_48474, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48474")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48474, file = "individual_movement/48474.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48475 ##
# NDAR
id_48475 <- subset(rkm_tracker_date, transmitter_id == 48475)
plot_48475 <-ggplot(id_48475, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48475")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48475, file = "individual_movement/48475.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48476 ##
# Net zero movement
id_48476 <- subset(rkm_tracker_date, transmitter_id == 48476)
plot_48476 <-ggplot(id_48476, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48476", subtitle="Presumed dead")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48476, file = "individual_movement/48476.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48477 ##
id_48477 <- subset(rkm_tracker_date, transmitter_id == 48477)
plot_48477 <-ggplot(id_48477, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48477")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48477, file = "individual_movement/48477.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48478 ## 
id_48478 <- subset(rkm_tracker_date, transmitter_id == 48478)
plot_48478 <-ggplot(id_48478, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48478")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48478, file = "individual_movement/48478.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48479 ##
id_48479 <- subset(rkm_tracker_date, transmitter_id == 48479)
plot_48479 <-ggplot(id_48479, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48479")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48479, file = "individual_movement/48479.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48480 ##
# NDAR
id_48480 <- subset(rkm_tracker_date, transmitter_id == 48480)
plot_48480 <-ggplot(id_48480, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48480")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48480, file = "individual_movement/48480.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48481 ## 
# NDAR
id_48481 <- subset(rkm_tracker_date, transmitter_id == 48481)
plot_48481 <-ggplot(id_48481, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48481")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48481, file = "individual_movement/48481.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48482 ##  
id_48482 <- subset(rkm_tracker_date, transmitter_id == 48482)
plot_48482 <-ggplot(id_48482, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48482")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48482, file = "individual_movement/48482.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48483 ##
id_48483 <- subset(rkm_tracker_date, transmitter_id == 48483)
plot_48483 <-ggplot(id_48483, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48483")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48483, file = "individual_movement/48483.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48484 ## 
id_48484 <- subset(rkm_tracker_date, transmitter_id == 48484)
plot_48484 <-ggplot(id_48484, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48484")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48484, file = "individual_movement/48484.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48485 ## 
id_48485 <- subset(rkm_tracker_date, transmitter_id == 48485)
plot_48485 <-ggplot(id_48485, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48485")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48485, file = "individual_movement/48485.png", dpi = 750, width = 7, height = 5,
       units = "in")


## 48486 ##
# NDAR
id_48486 <- subset(rkm_tracker_date, transmitter_id == 48486)
plot_48486 <-ggplot(id_48486, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48486")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48486, file = "individual_movement/48486.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48487 ##
id_48487 <- subset(rkm_tracker_date, transmitter_id == 48487)
plot_48487 <-ggplot(id_48487, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48487")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48487, file = "individual_movement/48487.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48488 ##
id_48488 <- subset(rkm_tracker_date, transmitter_id == 48488)
plot_48488 <-ggplot(id_48488, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48488")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48488, file = "individual_movement/48488.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48489 ## 
# NDAR
id_48489 <- subset(rkm_tracker_date, transmitter_id == 48489)
plot_48489 <- ggplot(id_48489, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48489")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48489, file = "individual_movement/48489.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48490 ##
# NDAR
id_48490 <- subset(rkm_tracker_date, transmitter_id == 48490)
plot_48490 <- ggplot(id_48490, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48490")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48490, file = "individual_movement/48490.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48491 ## 
id_48491 <- subset(rkm_tracker_date, transmitter_id == 48491)
plot_48491 <- ggplot(id_48491, aes(x=date, y=rkm))+
  ylim(0,375)+
  geom_point()+geom_line()+labs(title="ID # 48491")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48491, file = "individual_movement/48491.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48724 ##
# NDAR
id_48724 <- subset(rkm_tracker_date, transmitter_id == 48724)
plot_48724 <-ggplot(id_48724, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48724")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48724, file = "individual_movement/48724.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48725 ## 
id_48725 <- subset(rkm_tracker_date, transmitter_id == 48725)
plot_48725 <-ggplot(id_48725, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48725")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48725, file = "individual_movement/48725.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48726 ##
id_48726 <- subset(rkm_tracker_date, transmitter_id == 48726)
plot_48726 <-ggplot(id_48726, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48726")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48726, file = "individual_movement/48726.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48727 ## 
id_48727 <- subset(rkm_tracker_date, transmitter_id == 48727)
plot_48727 <-ggplot(id_48727, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48727")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48727, file = "individual_movement/48727.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48728 ## the big MO mover
id_48728 <- subset(rkm_tracker_date, transmitter_id == 48728)
plot_48728 <-ggplot(id_48728, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48728")+
  geom_hline(yintercept=0)
# geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48728, file = "individual_movement/48728.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48729 ##
# NDAR
id_48729 <- subset(rkm_tracker_date, transmitter_id == 48729)
plot_48729 <-ggplot(id_48729, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48729")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48729, file = "individual_movement/48729.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48730 ##  
id_48730 <- subset(rkm_tracker_date, transmitter_id == 48730)
plot_48730 <-ggplot(id_48730, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48730")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48730, file = "individual_movement/48730.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48731 ##
id_48731 <- subset(rkm_tracker_date, transmitter_id == 48731)
plot_48731 <-ggplot(id_48731, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48731")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48731, file = "individual_movement/48731.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48732 ## 
id_48732 <- subset(rkm_tracker_date, transmitter_id == 48732)
plot_48732 <- ggplot(id_48732, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48732")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48732, file = "individual_movement/48732.png", dpi = 750, width = 7, height = 5,
       units = "in")

## 48733 ## 
id_48733 <- subset(rkm_tracker_date, transmitter_id == 48733)
plot_48733 <- ggplot(id_48733, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48733")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
ggsave(plot_48733, file = "individual_movement/48733.png", dpi = 750, width = 7, height = 5,
       units = "in")
