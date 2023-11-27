# Load packages
library(Matrix)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(ggplot2)
library(ggbreak)

## USFWS Fish ####
GPFWCO_silver_carp <- read_excel("USFWS/2021 GPFWCO silver carp telemetry implant data.xlsx")

GPFWCO_simplified_detections <- read.csv("USFWS/GPFWCO James River Detections 2021 and 2022 simplified.csv")
GPFWCO_data <- read_excel("USFWS/GPFWCO James River Silver Carp Detections 2021 and 2022.xlsx")

GPFWCO_rkm_tracker_date <- GPFWCO_data %>% select(date, transmitter_id,rkm)


id_51768 <- subset(GPFWCO_rkm_tracker_date, transmitter_id == 51768)
ggplot(id_51768, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 51768", subtitle="Originally Tagged in Vermillion River")+
  geom_hline(yintercept=1.4, size=0.2, alpha=0.5)+
  geom_hline(yintercept=9, size=0.2, alpha=0.5)+
  geom_hline(yintercept=11.6, size=0.2, alpha=0.5)

id_51701 <- subset(GPFWCO_rkm_tracker_date, transmitter_id == 51701)
ggplot(id_51701, aes(x=date, y=rkm))+
  geom_point()+geom_line()+labs(title="Fish ID # 51768", subtitle="Originally Tagged in Vermillion River")+
  geom_hline(yintercept=1.4, size=0.2, alpha=0.5)+
  geom_hline(yintercept=9, size=0.2, alpha=0.5)+
  geom_hline(yintercept=11.6, size=0.2, alpha=0.5)
