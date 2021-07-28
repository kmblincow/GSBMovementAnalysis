#Kayla Blincow
#6/16/2021


#Is Woohoo Dead?

#The purpose of this script is to plot the detections per hour of Woohoo at 
#the Del Mar buoy to assess whether we think it died.


#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(lubridate)

#load my data
d <- read.csv("Data/GSB_detections.csv", header = T)

#filter for Woohoo at Del Mar
d2 <- filter(d, Transmitter == "A69-1601-56711" & Station == "Del Mar Mooring")

#calculate detections per hour
d3 <- d2 %>% group_by(Date, Hour) %>% 
  summarize(tot = n())

d3$DatHr <- paste(d3$Date, d3$Hour) %>% 
  ymd_h()

#plot it
ggplot(d3, aes(x = DatHr, y = tot)) +
  geom_point()
