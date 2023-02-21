library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(ggplot2)


##### James River #####
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
 
ggplot(data=All, aes(x=Date, y=Flow, color=name,group=name))+
  geom_line() +
  theme_minimal()+
  labs(color="Gage Station",
       x="June 2021 - Jan 2023",
       y="Mean Daily Discharge")

ggplot(data=All, aes(x=Date, y=GH, color=name,group=name))+
  geom_line() +
  theme_minimal()+
  labs(color="Gage Station",
       x="June 2021 - Jan 2022",
       y="Mean Daily Gage Height, ft")


#Plot with 2 Y axes

ScotlandRecent <- ggplot(data=Scotland, aes(x=Date, y=GH, group=name))+
  # geom_line(aes(y=Flow/2000))+
  geom_line(aes(y=GH), color="blue") +

  scale_y_continuous(
    name = "Gage Height, ft") +
    #, sec.axis = sec_axis(~ . *2000,name="Mean Daily Discharge, ft^3/sec"))+ 
  
  theme_linedraw()+
  
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"))+
  labs(x="")+
  
  ggtitle("James River Gage Height at Scotland, SD")

ggsave(ScotlandRecent, file = "ScotlandRecent.png", dpi = 550, width = 5, height = 3,
       units = "in")

# Vermillion River
VermillionRecent <- ggplot(data=Vermillion, aes(x=Date, y=GH, group=name))+
  # geom_line(aes(y=Flow/2000))+
  geom_line(aes(y=GH), color="blue") +
  
  scale_y_continuous(
    name = "Gage Height, ft") +
  #, sec.axis = sec_axis(~ . *2000,name="Mean Daily Discharge, ft^3/sec"))+ 
  
  theme_linedraw()+
  
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"))+
  labs(x="")+
  
  ggtitle("Vermillion River Gage Height")

ggsave(VermillionRecent, file = "VermillionRecent.png", dpi = 550, width = 5, height = 3,
       units = "in")

# Sioux Falls
SiouxFallsRecent <- ggplot(data=SiouxFalls, aes(x=Date, y=GH, group=name))+
  # geom_line(aes(y=Flow/2000))+
  geom_line(aes(y=GH), color="blue") +
  
  scale_y_continuous(
    name = "Gage Height, ft") +
  #, sec.axis = sec_axis(~ . *2000,name="Mean Daily Discharge, ft^3/sec"))+ 
  
  theme_linedraw()+
  
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"))+
  labs(x="")+
  
  ggtitle("Big Sioux River Gage Height at Sioux Falls, SD")

ggsave(SiouxFallsRecent, file = "SiouxFallsRecent.png", dpi = 550, width = 5, height = 3,
       units = "in")

#huron

HuronPlot <- ggplot(data=Huron, aes(x=Date, y=GH, group=name))+
  geom_line( aes(y=GH), color="blue")+
  geom_line(aes(y=Flow/25))+
  
  scale_y_continuous(
    name = "Gage Height, ft",
    sec.axis = sec_axis(~ . *25,name="Mean Daily Discharge"))+ 
  
  theme_linedraw()+
  
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "black"))+
  labs(x="")+
  
  ggtitle("James River at Huron",subtitle = "June - November 2021")

ggsave(HuronPlot, file = "HuronGageHeight.jpg", dpi = 750, width = 5, height = 3,
       units = "in")


# make another graph for temp data


#sets the size of the plot window
plot(FlowsHuron$Date,FlowsHuron$Flow, type="l", xlab="", ylab="",axes=FALSE)
par(new=TRUE)
plot(FlowsYankton$Date,FlowsYankton$Flow,
     col=c("blue"),
     type="l",
     xlab="",
     y="Mean daily discharge, m^3")
title("Mean Daily Discharge, 2021")
legend("topright", legend=c("James River Near Yankton", "James River Near Huron"), 
       col=c("blue", "black"),lty=1)


ggplot(Flows, aes(x=Date, y=Flow)) +                    # basic graphical object
  geom_smooth(aes(y=Flows$Flow), color="red") 
# # +  # first layer
#   geom_line(aes(y=Flows$Flow), colour="green")



##### JAMES R NEAR MITCHELL,SD #####

# siteNumbers <- c("06478000")
# siteINFO <- readNWISsite(siteNumbers)
# siteINFO$station_nm
# 
# # This pulls out just the daily, mean data:
# parameterCd <- c("00060") # discharge
# statCd <- c("00001","00003") # Mean and maximum
# startDate <- "2021-01-01"
# endDate <- "2021-12-31"
# Flows <- readNWISdv(siteNumbers, parameterCd,
#                     startDate, endDate, statCd=statCd)
# 
# names(Flows)
# 
# Flows <- renameNWISColumns(Flows)
# 
# names(Flows)
# 
# names(attributes(Flows))
# 
# statInfo <- attr(Flows, "statisticInfo")
# variableInfo <- attr(Flows, "variableInfo")
# siteInfo <- attr(Flows, "siteInfo")
# #sets the size of the plot window
# plot(Flows$Date,Flows$Flow,
#      col="red",type="l",xlab="",ylab="Mean daily discharge")
# title(paste(siteInfo$station_nm,"2021"))
# legend("topright", legend=variableInfo$unit,
#        col="red",lty=1)

##### JAMES RIVER NR YANKTON SD and HURON #####

siteNumbers <- c("06478513", "06476000")

bothsites <- readNWISsite(siteNumbers)

bothsites$station_nm

# This pulls out just the daily, mean data:
parameterCd <- c("00060") # discharge
statCd <- c("00001","00003") # Mean and maximum
startDate <- "2021-01-01"
endDate <- "2021-12-31"
BothFlows <- readNWISdv(siteNumbers, parameterCd,
                    startDate, endDate, statCd=statCd)

names(BothFlows)

BothFlows <- renameNWISColumns(BothFlows)

names(BothFlows)

names(attributes(BothFlows))

statInfo <- attr(BothFlows, "statisticInfo")
variableInfo <- attr(BothFlows, "variableInfo")
siteInfo <- attr(BothFlows, "siteInfo")


Selected <-BothFlows %>% mutate(FlowCFS=as.numeric(Flow)) %>% select(site_no,Date,FlowCFS) %>% 
  separate(site_no, into = "huron")



WideSelected <- Selected %>% pivot_wider(Selected,names_from = site_no, values_from = FlowCFS) %>% 
  group_by(site_no)

#sets the size of the plot window
ggplot(data=Selected, aes(x=Date, y=FlowCFS, group_by(site_no)+
       geom_line()))

# # %>% 
#   select(Date, `06476000`, `06478513`)
# data_try
# 
# ggplot(data_try, x=Date, color=)

    