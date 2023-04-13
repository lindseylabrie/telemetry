library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(ggplot2)


##### flow data #####
Yankton <- c("06478513")
Huron <- c("06476000")
Scotland <- c("06478500")
Mitchell <- c("06478000")
Vermillion <- c("06479010")
SiouxFalls <- c("06482000")
siteINFOhuron <- readNWISsite(Huron)
siteINFOyankton <-readNWISsite(Yankton)
siteINFOmitchell <-readNWISsite(Mitchell)
siteINFOvermillion <-readNWISsite(Vermillion)
siteINFOsiouxfalls <- readNWISsite(SiouxFalls)
siteINFOhuron$station_nm
siteINFOyankton$station_nm
siteINFOscotland$station_nm
siteINFOmitchell$station_nm
siteINFOvermillion$station_nm
siteINFOsiouxfalls$station_nm


surfaceDataYankton <- readNWISmeas(Yankton)
surfaceDataHuron <- readNWISmeas(Huron)
surfaceDataScotland <- readNWISmeas(Scotland)
surfaceDataMitchell <- readNWISmeas(Mitchell)
surfaceDataVermillion <- readNWISmeas(Vermillion)
surfaceDataSiouxFalls <- readNWISmeas(SiouxFalls)

# This pulls out just the daily, mean data:
parameterCd <- c("00060", "00065") # discharge ft^3/sec and gage height, ft
statCd <- c("00001","00003") # Mean and maximum
startDate <- "2021-06-01"
endDate <- "2023-1-31"

# James at Huron

FlowsHuron <- readNWISdv(Huron, parameterCd,
                                 startDate, endDate, statCd=statCd)
names(FlowsHuron)

FlowsHuron <- renameNWISColumns(FlowsHuron)
names(FlowsHuron)
names(attributes(FlowsHuron))

statInfo <- attr(FlowsHuron, "statisticInfo")
variableInfo <- attr(FlowsHuron, "variableInfo")
siteInfo <- attr(FlowsHuron, "siteInfo")

#James at yankton

FlowsYankton <- readNWISdv(Yankton, parameterCd,
                         startDate, endDate, statCd=statCd)
names(FlowsYankton)

FlowsYankton <- renameNWISColumns(FlowsYankton)
names(FlowsYankton)
names(attributes(FlowsYankton))

statInfo <- attr(FlowsYankton, "statisticInfo")
variableInfo <- attr(FlowsYankton, "variableInfo")
siteInfo <- attr(FlowsYankton, "siteInfo")

# James at Scotland

FlowsScotland <- readNWISdv(Scotland, parameterCd,
                         startDate, endDate, statCd=statCd)
names(FlowsScotland)

FlowsScotland <- renameNWISColumns(FlowsScotland)
names(FlowsScotland)
names(attributes(FlowsScotland))

statInfo <- attr(FlowsScotland, "statisticInfo")
variableInfo <- attr(FlowsScotland, "variableInfo")
siteInfo <- attr(FlowsScotland, "siteInfo")

# James at Michell, SD

FlowsMitchell <- readNWISdv(Mitchell, parameterCd,
                            startDate, endDate, statCd=statCd)
names(FlowsMitchell)

FlowsMitchell <- renameNWISColumns(FlowsMitchell)
names(FlowsMitchell)
names(attributes(FlowsMitchell))

statInfo <- attr(FlowsMitchell, "statisticInfo")
variableInfo <- attr(FlowsMitchell, "variableInfo")
siteInfo <- attr(FlowsMitchell, "siteInfo")

# Vermillion at Vermillion, SD

FlowsVermillion <- readNWISdv(Vermillion, parameterCd,
                         startDate, endDate, statCd=statCd)
names(FlowsVermillion)

FlowsVermillion <- renameNWISColumns(FlowsVermillion)
names(FlowsVermillion)
names(attributes(FlowsVermillion))

statInfo <- attr(FlowsVermillion, "statisticInfo")
variableInfo <- attr(FlowsVermillion, "variableInfo")
siteInfo <- attr(FlowsVermillion, "siteInfo")


# Big Sioux At Sioux Falls

FlowsSiouxFalls <- readNWISdv(SiouxFalls, parameterCd,
                         startDate, endDate, statCd=statCd)
names(FlowsSiouxFalls)

FlowsSiouxFalls <- renameNWISColumns(FlowsSiouxFalls)
names(FlowsSiouxFalls)
names(attributes(FlowsSiouxFalls))

statInfo <- attr(FlowsSiouxFalls, "statisticInfo")
variableInfo <- attr(FlowsSiouxFalls, "variableInfo")
siteInfo <- attr(FlowsSiouxFalls, "siteInfo")


AllFlows <- bind_rows (FlowsMitchell, FlowsHuron, FlowsYankton, FlowsScotland, FlowsVermillion, FlowsSiouxFalls)
Mitchell <- FlowsMitchell %>% mutate(name = "James River Near Mitchell")
Huron <- FlowsHuron %>% mutate(name = "James River Near Huron")
Yankton <- FlowsYankton %>% mutate(name = "James River Near Yankton")
Scotland <- FlowsScotland %>% mutate(name = "James River Near Scotland")
Vermillion <- FlowsVermillion %>% mutate(name = "Vermillion River at Vermillion, SD")
SiouxFalls <- FlowsSiouxFalls %>% mutate(name = "Big Sioux River at Sioux Falls, SD")
All <- bind_rows(Huron, Yankton, Scotland, Mitchell)
 
# Scotland data
DischargeScotland <- ggplot(data=Scotland, aes(x=Date, y=Flow, color=name,group=name))+
  geom_line() +
  theme_minimal()+
  labs(color="Gage Station",
       title = "James River Discharge at Scotland, SD",
       x="June 2021 - Jan 2023",
       y="Mean Daily Discharge, CFS")
ggsave(DischargeScotland, file="plots/DischargeScotland.jpg", dpi = 750, width = 7, height = 6,
       units = "in")

GageHeightScotland <- ggplot(data=Scotland, aes(x=Date, y=GH, color=name,group=name))+
  geom_line() +
  theme_minimal()+
  labs(color="Gage Station",
       title= "James River Gage Height at Scotland, SD",
       x="June 2021 - Jan 2022",
       y="Mean Daily Gage Height, ft")+
  theme(legend.position = "none")
ggsave(GageHeightScotland, file="plots/GageHeightScotland.jpg", dpi = 750, width = 7, height = 6,
       units = "in")

write_csv(Scotland, "environmental_data/scotland.csv")

# mitchell data
DischargeMitchell <- ggplot(data=Mitchell, aes(x=Date, y=Flow, color=name,group=name))+
  geom_line() +
  theme_minimal()+
  labs(color="Gage Station",
       title = "James River Discharge at Mitchell, SD",
       x="June 2021 - Jan 2023",
       y="Mean Daily Discharge, CFS")
ggsave(DischargeMitchell, file="plots/DischargeMitchell.jpg", dpi = 750, width = 7, height = 6,
       units = "in")

GageHeightMitchell <- ggplot(data=Mitchell, aes(x=Date, y=GH, color=name,group=name))+
  geom_line() +
  theme_minimal()+
  labs(color="Gage Station",
       title= "James River Gage Height at Mitchell, SD",
       x="June 2021 - Jan 2022",
       y="Mean Daily Gage Height, ft")+
  theme(legend.position = "none")
ggsave(GageHeightMitchell, file="plots/GageHeightMitchell.jpg", dpi = 750, width = 7, height = 6,
       units = "in")




# make another graph for temp data
# use the hwy 50 temp data sheet

James_Temp <- read_excel("Hwy_50_Temp_10_12_22.xlsx") %>% clean_names()

JamesTemp <- ggplot(data=James_Temp, aes(x=date_time_gmt_05_00, y=temp_f))+
  # geom_point() +
  geom_smooth() +
  theme_minimal()+
  labs(title= "James River Average Temperature at Yankton, SD",
       x="June 2021 - October 2022",
       y="Temperature (deg F)")+
  theme(legend.position = "none")

ggsave(GageHeightMitchell, file="plots/GageHeightMitchell.jpg", dpi = 750, width = 7, height = 6,
       units = "in")


    