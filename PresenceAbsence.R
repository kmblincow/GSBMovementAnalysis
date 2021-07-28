#Kayla Blincow
#5/21/2021

#The purpose of this script is to explore the probability of presence analysis
#for the GSB detection data

#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)

#load the data
d <- read.csv("Data/GSB_detections.csv")

#array level presence/absence
#should this combine all tags?? Or separate by tag number??

#removing del mar receiver
LJ <- filter(d, Station != "Del Mar Mooring")
#group detections by summing total in 1-h time bins by day
#create time bins
days <- seq(from = min(as.Date(LJ$Date)), to = max(as.Date(LJ$Date)), by = "day")
total_days <- length(days)
total_hrs <- rep(0:23, total_days)
index <- rep(days, 24) %>% 
  sort() %>% 
  as.Date() %>% 
  as.data.frame()
index <- cbind(index, total_hrs)
names(index) <- c("Date", "Hour")

LJ <- LJ %>% group_by(Transmitter, Date, Hour) %>% 
  summarize(n = n(),
            dielp = first(dielp),
            lunar = first(lunar),
            spawn = first(spawn), 
            lobster = first(lobster),
            season = first(season), 
            TL = first(TL)) %>% 
  ungroup()

LJ$Date <- as.Date(LJ$Date)  


full_join(index, LJ, by = c("Date" = "Date", "Hour" = "Hour"))
  
