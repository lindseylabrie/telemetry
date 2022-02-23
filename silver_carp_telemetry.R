
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(ggplot2)
library(ggbreak)

receiver303_10_22_21 <- read_csv("receiver_data/Receiver303_10_22_21.csv")
receiver1_11_16_21 <- read_csv("receiver_data/Receiver1_11_16_21.csv") 
receiver2_11_16_21 <- read_csv("receiver_data/Receiver2_11_16_21.csv")
receiver3_10_22_21 <- read_csv("receiver_data/Receiver3_10_22_21.csv")
receiver4_10_22_21 <- read_csv("receiver_data/Receiver4_10_22_21.csv")
receiver6_10_22_21 <- read_csv("receiver_data/Receiver6_10_22_21.csv")
receiver7_10_22_21 <- read_csv("receiver_data/Receiver7_10_22_21.csv")
receiver8_10_22_21 <- read_csv("receiver_data/Receiver8_10_22_21.csv")
receiver9_10_22_21 <- read_csv("receiver_data/Receiver9_10_22_21.csv")
receiver10_10_22_21 <- read_csv("receiver_data/Receiver10_10_22_21.csv")
receiver11_10_14_21 <- read_csv("receiver_data/Receiver11_10_14_21.csv")
receiver12_10_14_21 <- read_csv("receiver_data/Receiver12_10_14_21.csv")
receiver13_10_14_21 <- read_csv("receiver_data/Receiver13_10_14_21.csv")
receiver14_10_14_21 <- read_csv("receiver_data/Receiver14_10_14_21.csv")
receiver15_10_05_21 <- read_csv("receiver_data/Receiver15_10_05_21.csv")
receiver16_10_05_21 <- read_csv("receiver_data/Receiver16_10_05_21.csv")
receiver17_10_05_21 <- read_csv("receiver_data/Receiver17_10-05_21.csv")
receiver19_10_05_21 <- read_csv("receiver_data/Receiver19_10_05_21.csv")
silver_carp_release_points <- read_excel("tracking_data/silver_carp_release_points.xlsx")

all_vemco_receivers <- read_excel("receiver_data/all_vemco_receivers.xlsx") %>% 
  separate(name, c("receiver", "station_name")) %>% 
  mutate(station_name = as.numeric(station_name), 
         latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude))


all_data <- bind_rows(receiver303_10_22_21,
                      receiver1_11_16_21,
                      receiver2_11_16_21,
                      receiver3_10_22_21,
                      receiver4_10_22_21,
                      receiver6_10_22_21,
                      receiver7_10_22_21,
                      # # Receiver8_10_22_21, no data, it was stuck in the mud
                      # # Receiver9_10_22_21, no data
                      # # Receiver10_10_22_21, no data
                      receiver11_10_14_21,
                      receiver12_10_14_21,
                      receiver13_10_14_21,
                      receiver14_10_14_21,
                      receiver15_10_05_21,
                      receiver16_10_05_21,
                      receiver17_10_05_21,
                      receiver19_10_05_21) %>%
  clean_names %>% 
  separate(transmitter, c("freq1", "freq2", "transmitter_id")) %>% 
  mutate(transmitter_id = as.numeric(transmitter_id)) %>% 
 # mutate() %>% 
  filter(transmitter_id < 49000) %>% 
  filter(transmitter_id > 48000) %>% 
  mutate(date = ymd(as.Date(date_and_time_utc)),
         detected_by = as.character("stationary_receiver")) %>%  
  select(date, station_name,transmitter_id, detected_by) %>% 
  distinct(station_name,transmitter_id, date, detected_by) %>% 
  left_join(all_vemco_receivers)

# write.csv(all_data, "all_data.csv")

active_tracking_individuals <- read_excel("tracking_data/active_tracking_points.xlsx") %>% 
  mutate(detected_by = as.character("active_tracking")) %>% 
  rownames_to_column()

combined_data <- bind_rows(active_tracking_individuals,all_data,silver_carp_release_points)

rkm_tracker_date <- combined_data %>% select(date, transmitter_id,rkm)%>%
  left_join(silver_carp_release_points %>% select(transmitter_id, rkm) %>% 
              rename(start_rkm = rkm)) %>% 
  mutate(distance = rkm-start_rkm)


##all movement
All_Individuals <- rkm_tracker_date %>% 
  # filter(transmitter_id %in% c(48453, 48455, 48456, 48457)) %>% 
  ggplot(aes(color=as.character(transmitter_id), group=transmitter_id,x=date, y=distance))+
  geom_line()+
  geom_point(shape=20) +
  labs(y = "Distance Traveled, rkm",
       x = "") +
  theme_gray()+
  theme(legend.position="none")+
  ggtitle("James River Silver Carp Movement",
          subtitle = "June - November 2021")
ggsave(All_Individuals, file = "plots/AllIndividuals.png", dpi = 750, width = 7, height = 5,
       units = "in")

# Detections per day for all tags, with axis break
AllDailyDetections <- rkm_tracker_date %>% 
  ggplot(aes(x=date, y=transmitter_id, group=transmitter_id))+
  geom_line(color="light blue")+
  geom_point(shape=16) +
  labs(y = "Telemetered Individual",
       x = "") +
  theme_bw()+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(48452, 48733))+
  scale_y_break(c(48491, 48724), scales = 0.25) +
  scale_y_continuous(breaks=seq(48452, 48733, 2))+
  # ylim(48452, 48491)+
  ggtitle("James River",
          subtitle = "Daily Detections, June - November 2021")
ggsave(AllDailyDetections, file = "plots/AllDailyDetections.png", dpi = 750, width = 7, height = 6,
       units = "in")

# Detections per day for 48724 through 48733
DailyDetections724through733 <- rkm_tracker_date %>% 
  ggplot(aes(x=date, y=transmitter_id, group=transmitter_id))+
  geom_line()+
  geom_point(shape=20) +
  coord_cartesian(ylim=c(48724, 48733))+
  scale_y_continuous(breaks=seq(48724, 48733, 1))+
  labs(y = "Telemetered Individual",
       x = "") +
  theme_gray()+
  theme(legend.position="none")+
  ggtitle("James River",
          subtitle = "Daily Detections, June - November 2021")
ggsave(DailyDetections724through733, file = "plots/DailyDetections724through733.png", dpi = 750, width = 7, height = 6,
       units = "in")

##Individs tagged near mitchell
Mitchell_Individuals <- rkm_tracker_date %>% 
  filter(start_rkm < 243) %>% 
  filter(start_rkm > 236) %>%
  ggplot(aes(color=as.character(transmitter_id), group=transmitter_id,x=date, y=distance))+
  geom_line()+
  geom_point(shape=20) +
  labs(y = "Distance Traveled, rkm",
       x = "") +
  theme_gray()+
  theme(legend.position="none")+
  ggtitle("James River Silver Carp Movement",
          subtitle = "Individuals Tagged Near Mitchell")

ggsave(Mitchell_Individuals, file = "plots/MitchellIndividuals.png", dpi = 750, width = 7, height = 5,
       units = "in")


##Individs tagged near Schramm
Schramm_Individuals <- rkm_tracker_date %>% 
  filter(start_rkm == 46.3) %>%
  ggplot(aes(color=as.character(transmitter_id), group=transmitter_id,x=date, y=distance))+
  geom_line()+
  geom_point(shape=20) +
  labs(y = "Distance Traveled, rkm",
       x = "") +
  theme_gray()+
  theme(legend.position="none")+
  ggtitle("James River Silver Carp Movement",
          subtitle = "Individuals Tagged Near Schramm Boat Access")

ggsave(Schramm_Individuals, file = "plots/SchrammIndividuals.png", dpi = 750, width = 7, height = 5,
       units = "in")

#9 are missing, 39 rkm gap between this and the next upstream receiver. 
# need to consider moving a receiver between here both directions


##Individs tagged near Confluence
Confluence_Individuals <- rkm_tracker_date %>% 
  filter(start_rkm <5) %>% 
  # filter(distance < 0)%>%
  ggplot(aes(color=as.character(transmitter_id), group=transmitter_id,x=date, y=distance))+
  geom_line()+
  geom_point(shape=20) +
  labs(y = "Distance Traveled, rkm",
       x = "") +
  theme_gray()+
  # theme(legend.position="none")+
  ggtitle("James River Silver Carp Movement",
          subtitle = "Individuals Tagged Near James/Missouri Confluence")

ggsave(Confluence_Individuals, file = "plots/ConfluenceIndividuals.png", dpi = 750, width = 7, height = 5,
       units = "in")

##Individs with no movement
QuestionablyAlive <- rkm_tracker_date %>% 
  filter(transmitter_id %in% c(48476, 48474, 48462, 48484, 48468)) %>% 
  ggplot(aes(color=as.character(transmitter_id), group=transmitter_id,x=date, y=distance))+
  geom_line()+
  geom_point(shape=20) +
  labs(y = "Distance Traveled, rkm",
       x = "",
       color="Individual") +
  theme_gray()+
  # theme(legend.position="none")+
  ggtitle("James River Silver Carp Movement",
          subtitle = "Questionably Alive")

ggsave(QuestionablyAlive, file = "plots/QuestionablyAlive.png", dpi = 750, width = 7, height = 5,
       units = "in")



##Maximum distance traveled by all individuals
max_dist <- rkm_tracker_date %>% 
  group_by(transmitter_id) %>% 
  mutate(max_rkm = max(rkm),
         min_rkm = min(rkm)) %>% 
  distinct(transmitter_id, max_rkm, min_rkm) %>% 
  mutate(distance_max = max_rkm - min_rkm)

mean(max_dist$distance_max)

MaxDistancePlot <- ggplot(max_dist, aes(x = reorder(as.factor(transmitter_id), -distance_max), y = distance_max )) + 
  geom_point() +
  labs(x="Tagged Individuals",
       y="Maximum Distance Traveled (rkm)",
       title = "Silver Carp Movement in the James River")+
  geom_hline(data=max_dist,aes(yintercept=mean(distance_max)), color="purple")+
  theme_linedraw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.ticks = element_blank())
  
ggsave(MaxDistancePlot,file="plots/MaxDistancePlot.jpg", dpi = 750, width = 5, height = 3,
       units = "in")



# rkm_tracker_date %>% 
#   mutate(distance_from_release = )

#i don't konw why i can't get the axis labels to go away

## find how people have tracked movement, look for descriptions of movement ##
## release point is 0 and movement bobs from there (covid data kinda looks like this)##

#something like: starting point = rkm of release location. each next detection 
# is subtracted from the previous one to give an idea of movement


# write.csv(rkm_tracker_date,"rkm_tracker_date.csv")

# work on making distance plots using rkm tracker date file

### Plot of all fish data ####
ggplot(rkm_tracker_date, aes(x=date, y=rkm, group=transmitter_id, color=as.character(transmitter_id)))+
  geom_line()+ labs(title="James River Silver Carp Detections", subtitle ="June - November 2021.", 
                    caption = "Grey horizontal lines represent stationary receiver locations")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)+
  guides(color="none")+
  labs(x=NULL)
# +
#   facet_wrap(vars(transmitter_id))

#### Individual Fish Plots ####

## 48452 ## Not detected after release
id_48452 <- subset(rkm_tracker_date, transmitter_id == 48452)
ggplot(id_48452, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+labs(title="ID # 48452")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48453 ## total detected distance traveled = 4.3 rkm downstream
id_48453 <- subset(rkm_tracker_date, transmitter_id == 48453)
ggplot(id_48453, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48453")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48454 ## total detected distance traveled = 0.7 km downstream
id_48454 <- subset(rkm_tracker_date, transmitter_id == 48454)
ggplot(id_48454, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48454")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)


## 48455 ## total detected distance traveled = 1.7 km upstream
id_48455 <- subset(rkm_tracker_date, transmitter_id == 48455)
ggplot(id_48455, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48455")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48456 ## total detected distance traveled = 0.7 km upstream
id_48456 <- subset(rkm_tracker_date, transmitter_id == 48456)
ggplot(id_48456, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48456")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48457 ## 1.5 km total movement
id_48457 <- subset(rkm_tracker_date, transmitter_id == 48457)
ggplot(id_48457, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48457")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48458 ## detected but no movement
id_48458 <- subset(rkm_tracker_date, transmitter_id == 48458)
ggplot(id_48458, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48458")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48459 ## 1.2 km ds on day of release, but not detected after release date
id_48459 <- subset(rkm_tracker_date, transmitter_id == 48459)
ggplot(id_48459, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48459")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48460 ## 0.7 km downstream
id_48460 <- subset(rkm_tracker_date, transmitter_id == 48460)
ggplot(id_48460, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48460")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48461 ## detected but no movement
id_48461 <- subset(rkm_tracker_date, transmitter_id == 48461)
ggplot(id_48461, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48461")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48462 ## 5.9 km downstream
id_48462 <- subset(rkm_tracker_date, transmitter_id == 48462)
ggplot(id_48462, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48462")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48463 ## 23.9 km total movement
id_48463 <- subset(rkm_tracker_date, transmitter_id == 48463)
ggplot(id_48463, aes(x=date, y=rkm ))+
  geom_point()+geom_line()+labs(title="Fish ID # 48463",)+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48464 ## 0.4 km downstream
id_48464 <- subset(rkm_tracker_date, transmitter_id == 48464)
ggplot(id_48464, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48464")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48465 ## NDAR
id_48465 <- subset(rkm_tracker_date, transmitter_id == 48465)
ggplot(id_48465, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48465")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48466 ## 2.2 km total movement
id_48466 <- subset(rkm_tracker_date, transmitter_id == 48466)
ggplot(id_48466, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48466")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48467 ## NDAR
id_48467 <- subset(rkm_tracker_date, transmitter_id == 48467)
ggplot(id_48465, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48467")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48468 #####  33 km total movement, 28 days to go 31.35 km upstream
id_48468 <- subset(rkm_tracker_date, transmitter_id == 48468)
ggplot(id_48468, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48468")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48469 ## 6.2 km downstream
id_48469 <- subset(rkm_tracker_date, transmitter_id == 48469)
ggplot(id_48469, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48469")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48470 ## NDAR
id_48470 <- subset(rkm_tracker_date, transmitter_id == 48470)
ggplot(id_48470, aes(x=date, y=rkm ))+
  geom_point()+geom_line()+labs(title="ID # 48470")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48471 ## 0.7 km upstream
id_48471 <- subset(rkm_tracker_date, transmitter_id == 48471)
ggplot(id_48471, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48471")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
  

## 48472 ## 0.3 km upstream
id_48472 <- subset(rkm_tracker_date, transmitter_id == 48472)
ggplot(id_48472, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48472")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48473 ## NDAR
id_48473 <- subset(rkm_tracker_date, transmitter_id == 48473)
ggplot(id_48473, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="ID # 48473")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48474 ## 0.2 km total
id_48474 <- subset(rkm_tracker_date, transmitter_id == 48474)
ggplot(id_48474, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_line()+labs(title="ID # 48474")+
  geom_point()+geom_line()+labs(title="ID # 48474")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48475 ## 0.6 km downstream
id_48475 <- subset(rkm_tracker_date, transmitter_id == 48475)
ggplot(id_48475, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48475")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48476 #### detected every day but no movement, probably dead
id_48476 <- subset(rkm_tracker_date, transmitter_id == 48476)
ggplot(id_48476, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48476", subtitle="Presumed dead")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48477 ## 2.6 km downstream, probably swam into MO River
id_48477 <- subset(rkm_tracker_date, transmitter_id == 48477)
ggplot(id_48477, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48477", subtitle="Presumed to have moved into the Missouri River")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48478 ## NDAR
id_48478 <- subset(rkm_tracker_date, transmitter_id == 48478)
ggplot(id_48478, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48478")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48479 ## 2.8 km total
id_48479 <- subset(rkm_tracker_date, transmitter_id == 48479)
ggplot(id_48479, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48479")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48480 ## 0.3 km upstream
id_48480 <- subset(rkm_tracker_date, transmitter_id == 48480)
ggplot(id_48480, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48480")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48481 ## NDAR
id_48481 <- subset(rkm_tracker_date, transmitter_id == 48481)
ggplot(id_48481, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48481")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48482 ## 3.9 km total distance 
id_48482 <- subset(rkm_tracker_date, transmitter_id == 48482)
ggplot(id_48482, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48482")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48483 ## 4.4 km total
id_48483 <- subset(rkm_tracker_date, transmitter_id == 48483)
ggplot(id_48483, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48483")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48484 ## 20.5 total km
id_48484 <- subset(rkm_tracker_date, transmitter_id == 48484)
ggplot(id_48484, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48484")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48485 ## 4.1 total km 
id_48485 <- subset(rkm_tracker_date, transmitter_id == 48485)
ggplot(id_48485, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48485")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48486 ## 4.9 total km, probably moved out into MO river
id_48486 <- subset(rkm_tracker_date, transmitter_id == 48486)
ggplot(id_48486, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48486", subtitle="Presumed to have moved into Missouri River")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48487 ## NDAR
id_48487 <- subset(rkm_tracker_date, transmitter_id == 48487)
ggplot(id_48487, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48487")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
  
## 48488 ## 8.6 km total 
id_48488 <- subset(rkm_tracker_date, transmitter_id == 48488)
ggplot(id_48488, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48488")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48489 ## 0.7 km upstream
id_48489 <- subset(rkm_tracker_date, transmitter_id == 48489)
ggplot(id_48489, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48489")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48490 ## 0.4 km downstream
id_48490 <- subset(rkm_tracker_date, transmitter_id == 48490)
ggplot(id_48490, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48490")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48491 ## NDAR
id_48491 <- subset(rkm_tracker_date, transmitter_id == 48491)
ggplot(id_48491, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48491")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48724 ## 0.1 km downstream
id_48724 <- subset(rkm_tracker_date, transmitter_id == 48724)
ggplot(id_48724, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48724")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48725 ## 13.1 km total 
id_48725 <- subset(rkm_tracker_date, transmitter_id == 48725)
ggplot(id_48725, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48725")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48726 ## 19.3 total km
id_48726 <- subset(rkm_tracker_date, transmitter_id == 48726)
ggplot(id_48726, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48726")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48727 ## 7.6 km total
id_48727 <- subset(rkm_tracker_date, transmitter_id == 48727)
ggplot(id_48727, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48727")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48728 ## 0.7 km downstream
id_48728 <- subset(rkm_tracker_date, transmitter_id == 48728)
ggplot(id_48728, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48728")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48729 ## 0.7 km on day of tagging
id_48729 <- subset(rkm_tracker_date, transmitter_id == 48729)
ggplot(id_48729, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48729")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48730 ## 2.3 km total 
id_48730 <- subset(rkm_tracker_date, transmitter_id == 48730)
ggplot(id_48730, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48730")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48731 ## 4.3 km downstream
id_48731 <- subset(rkm_tracker_date, transmitter_id == 48731)
ggplot(id_48731, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48731")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)
  
## 48732 ## 7.6 km total 
id_48732 <- subset(rkm_tracker_date, transmitter_id == 48732)
ggplot(id_48732, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 48732")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

## 48733 ## 4.3 km downstream
id_48733 <- subset(rkm_tracker_date, transmitter_id == 48733)
ggplot(id_48733, aes(x=date, y=rkm, color=as.character(transmitter_id)))+
  geom_point()+geom_line()+labs(title="ID # 48733")+
  geom_hline(data=all_vemco_receivers, aes(yintercept=rkm),size=0.2, alpha=0.5)

# email Alison for tips

# telemetry_detected_individuals <- all_data %>% distinct(transmitter_id)

# # getting a total number of fish seen from all sources
# all_detected_individuals <- bind_rows(active_tracking_individuals, telemetry_detected_individuals) %>% 
#   distinct(transmitter_id)









## USFWS Fish ####
GPFWCO_silver_carp <- read_excel("2021_GPFWCO_silver_carp_implant_data.xlsx")

GPFWCO_data <- bind_rows(receiver303_10_22_21,
                      receiver1_11_16_21,
                      receiver2_11_16_21,
                      receiver3_10_22_21,
                      receiver4_10_22_21,
                      receiver6_10_22_21,
                      receiver7_10_22_21,
                      # # Receiver8_10_22_21, no data, it was stuck in the mud
                      # # Receiver9_10_22_21, no data
                      # # Receiver10_10_22_21, no data
                      receiver11_10_14_21,
                      receiver12_10_14_21,
                      receiver13_10_14_21,
                      receiver14_10_14_21,
                      receiver15_10_05_21,
                      receiver16_10_05_21,
                      receiver17_10_05_21,
                      receiver19_10_05_21) %>%
  clean_names %>% 
  separate(transmitter, c("freq1", "freq2", "transmitter_id")) %>% 
  mutate(transmitter_id = as.numeric(transmitter_id)) %>% 
  # mutate() %>% 
  filter(transmitter_id < 51800) %>% 
  filter(transmitter_id > 51600) %>% 
  mutate(date = ymd(as.Date(date_and_time_utc)),
         detected_by = as.character("stationary_receiver")) %>%  
  select(date, station_name,transmitter_id, detected_by) %>% 
  distinct(station_name,transmitter_id, date, detected_by) %>% 
  left_join(all_vemco_receivers)

GPFWCO_rkm_tracker_date <- GPFWCO_data %>% select(date, transmitter_id,rkm)

id_51768 <- subset(GPFWCO_rkm_tracker_date, transmitter_id == 51768)
ggplot(id_51768, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 51768", subtitle="Originally Tagged in Vermillion River")+
  geom_hline(yintercept=1.4, size=0.2, alpha=0.5)+
  geom_hline(yintercept=9, size=0.2, alpha=0.5)+
  geom_hline(yintercept=11.6, size=0.2, alpha=0.5)
  # + geom_hline(yintercept=40.4, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=79.3, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=89, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=105, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=117, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=143, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=157.5, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=170.2, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=206.7, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=213, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=224.7, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=232, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=238.2, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=277.8, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=295.8, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=302.5, size=0.2, alpha=0.5)+
  # geom_hline(yintercept=356.4, size=0.2, alpha=0.5)
