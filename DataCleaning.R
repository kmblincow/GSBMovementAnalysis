#Kayla Blincow
#5/18/2021

#Data cleaning for GSBs
#The purpose of this script is to filter the full detection database for just the
#GSBs, and integrate detections from other researcher's receivers
#Then do some cleaning to make sure that it's all good to go.


#Cleaning: some receivers mislabeled (LJK12, Del Mar), some receivers with gaps

#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(lubridate)
library(suncalc)
library(lunar)

#load data (our database)
d <- read.csv("Data/VUE_Export_06142021.csv", header = T)
d <- d[,1:10]

#load data (Andy's receivers)
d2 <- read.csv("Data/kaylaGSB_113020.csv", header = T)

#load data (Del Mar)
#d3 <- read.csv("Data/VR2Cdata_labcomp.csv", header = T)
#no GSB data in this file

#renames columns for both
cnames <- c("DateTime", "Receiver", "Transmitter", "TName", "TSerial", "SensorV",
            "SensorU", "Station", "Latitude", "Longitude")

names(d) <- cnames
names(d2) <- cnames
#names(d3) <- cnames

#filter for just GSB tags
#select GSB tag nubmers
d <- d[d$Transmitter=="A69-1601-56711" | 
          d$Transmitter=="A69-1601-56705" |
          d$Transmitter=="A69-1601-27063" |
          d$Transmitter=="A69-1601-56704" |
          d$Transmitter=="A69-9001-11125" |
          d$Transmitter=="A69-1601-56706" |
          d$Transmitter=="A69-1601-27070",]



#combine the datasets
d <- rbind(d, d2)

#deal with dates and times...
d$DateTime <- ymd_hms(d$DateTime)
d$Year <- year(d$DateTime)
d$Month <- month(d$DateTime)
d$Date <- date(d$DateTime)
d$Hour <- hour(d$DateTime)


#filter extraneous records (from VR2c testing) and remove unneeded columns
d <- d %>% 
  filter(Year > 2016) %>% 
  dplyr::select(DateTime, Date, Year, Month, Hour, Transmitter, Station, Latitude, Longitude)

#also need to remove some detections from LJK4, because it was clearly drifting
#off coordinates during it's final detections on 1/4/2021
for(i in 1:nrow(d)){
  if(d$Station[i] == "LJK 4" & d$Date[i] == "2021-01-04") {
    d$Station[i] <- NA
  }
}

#check that
d[is.na(d$Station),]

d <- d[!is.na(d$Station),]

#fill in missing lats and longs for mislabeled receivers
d[d$Station == "LJ (La Jolla Shores)",]$Latitude <- 
  rep(first(d[d$Station == "LJ (La Jolla Shores)",]$Latitude), 
      nrow(d[d$Station == "LJ (La Jolla Shores)",]))

d[d$Station == "LJ (La Jolla Shores)",]$Longitude <- 
  rep(first(d[d$Station == "LJ (La Jolla Shores)",]$Longitude), 
      nrow(d[d$Station == "LJ (La Jolla Shores)",]))


d[d$Station == "Pacific Beach NEW",]$Latitude <- 
  rep(first(d[d$Station == "Pacific Beach NEW",]$Latitude),
      nrow(d[d$Station == "Pacific Beach NEW",]))

d[d$Station == "Pacific Beach NEW",]$Longitude <- 
  rep(first(d[d$Station == "Pacific Beach NEW",]$Longitude),
      nrow(d[d$Station == "Pacific Beach NEW",]))

d[d$Station == "Del Mar Mooring",]$Latitude <- 32.92955
d[d$Station == "Del Mar Mooring",]$Longitude <- -117.31708

d[d$Station == "LJK 17",]$Longitude <- -117.28815


#add additional information for analysis
#diel period
sun <- d %>% dplyr::select(Date, Latitude, Longitude) %>% 
  rename(date = Date, lat = Latitude, lon = Longitude)
sun <- getSunlightTimes(data = sun, keep = c("sunrise", "sunset", "night", "nightEnd"))
          
d$dielp <- NA

#nightEnd gives you the end of night in the morning of the given date, not the 
#end of night the next day...
#adding 24 hours to nightEnd to get an approximate value of the end of 
#night the next day
sun$night2 <- sun$night - 24*60*60
sun$sunrise2 <- sun$sunrise - 24*60*60
sun$sunset2 <- sun$sunset - 24*60*60

d[d$DateTime >= sun$sunset2 & d$DateTime <= sun$night2,]$dielp <- "dusk"
d[d$DateTime >= sun$nightEnd & d$DateTime <= sun$sunrise,]$dielp <- "dawn"
d[d$DateTime >= sun$night2 & d$DateTime <= sun$nightEnd,]$dielp <- "night"
d[d$DateTime >= sun$sunrise & d$DateTime <= sun$sunset,]$dielp <- "day"
d[d$DateTime >= sun$sunrise2 & d$DateTime <= sun$sunset2,]$dielp <- "day"

#lunar phase
d$lunar <- lunar.phase(d$Date, name = TRUE)

#spawning season
d$spawn <- NA
d[d$Month >= 6 & d$Month <= 9,]$spawn <- "spwn"
d[d$Month < 6 | d$Month > 9,]$spawn <- "nonspwn"

#lobster season
d$lobster <- NA
d[d$Month >= 10 & d$Month <= 12,]$lobster <- "high"
d[d$Month >= 1 & d$Month <= 3,]$lobster <- "mid"
d[d$Month > 3 & d$Month < 10,]$lobster <- "off"

#season
d$season <- NA
d[d$Month == 12 | d$Month <= 2,]$season <- "winter"
d[d$Month >= 3 & d$Month <= 5,]$season <- "spring"
d[d$Month >= 6 & d$Month <= 8,]$season <- "summer"
d[d$Month >= 9 & d$Month <= 11,]$season <- "fall"

#MPA receivers
#**NOTE** Make sure this is updated with any new receiver detections 
d$MPA <- NA
d[d$Station == "LJK 33" | d$Station == "LJK 8" | d$Station == "LJK 6" |
    d$Station == "LJK 7" | d$Station == "LJK 34" | d$Station == "LJK 32" |
    d$Station == "LJK 10" | d$Station == "LJK 9" | d$Station == "LJK 11" |
    d$Station == "LJK 12" | d$Station == "LJK 5" | d$Station == "LJK 43" |
    d$Station == "LJK 19" | d$Station == "LJK 4" | d$Station == "LJK20" | 
    d$Station == "PL - North LJ"|
    d$Station == "Pacific Beach NEW",]$MPA <- "out"

d[d$Station == "LJK 13" | d$Station == "LJK 39" | d$Station == "LJK 15" |
    d$Station == "LJK 40" | d$Station == "LJK 41" | d$Station == "LJK 17" |
    d$Station == "LJK 42" | d$Station == "LJK 18" | d$Station == "LJK 24" |
    d$Station == "LJK 14" | d$Station == "LJK 3" | d$Station == "LJK 1" |
    d$Station == "Charles -  LJK" | d$Station == "LJ (La Jolla Shores)" ,]$MPA <- "in"

d[d$Station == "Del Mar Mooring",]$MPA <- "DelMar"

#kelp cover (MegaPatch Data)
d$kelp <- NA
d[d$Station == "LJK 3" | d$Station == "LJK 4" | d$Station == "LJK 5" |
    d$Station == "LJK 6" | d$Station == "LJK 7" | d$Station == "LJK 8" |
    d$Station == "LJK 9" | d$Station == "LJK 10" | d$Station == "LJK 11" |
    d$Station == "LJK 13" | d$Station == "LJK 14" | d$Station == "LJK 15" |
    d$Station == "LJK 17" | d$Station == "LJK 18" | d$Station == "LJK 24" |
    d$Station == "LJK 32" | d$Station == "LJK 33" | d$Station == "LJK 34" |
    d$Station == "LJK 40" ,]$kelp <- "kelp"
d[d$Station == "Pacific Beach NEW" | d$Station == "LJ (La Jolla Shores)" |
    d$Station == "Del Mar Mooring" | d$Station == "PL - North LJ" |
    d$Station == "Charles -  LJK" | d$Station == "LJK 1" |
    d$Station == "LJK 12" | d$Station == "LJK 19" | d$Station == "LJK20" |
    d$Station == "LJK 39" | d$Station == "LJK 41" | d$Station == "LJK 42" |
    d$Station == "LJK 43",]$kelp <- "nokelp"


#depth
d$depth <- NA
d[d$Station == "LJK 33",]$depth <- 55
d[d$Station == "LJK 8",]$depth <- 68
d[d$Station == "PL - North LJ",]$depth <- 118
d[d$Station == "LJK 7",]$depth <- 67
d[d$Station == "LJK 34",]$depth <- 37
d[d$Station == "LJK 6",]$depth <- 63
d[d$Station == "LJK 32",]$depth <- 54
d[d$Station == "LJK 10",]$depth <- 70
d[d$Station == "LJK 9",]$depth <- 65
d[d$Station == "LJK 11",]$depth <- 70
d[d$Station == "LJK 12",]$depth <- 75
d[d$Station == "LJK 13",]$depth <- 69
d[d$Station == "LJK 5",]$depth <- 65
d[d$Station == "LJK 39",]$depth <- 80
d[d$Station == "LJK 15",]$depth <- 74
d[d$Station == "LJK 40",]$depth <- 77
d[d$Station == "LJK 41",]$depth <- 77
d[d$Station == "LJK 17",]$depth <- 67
d[d$Station == "LJK 42",]$depth <- 66
d[d$Station == "LJK 43",]$depth <- 81
d[d$Station == "LJK 1",]$depth <- 52
d[d$Station == "LJK 19",]$depth <- 67
d[d$Station == "LJK 18",]$depth <- 59
d[d$Station == "LJK 24",]$depth <- 54
d[d$Station == "LJK 14",]$depth <- 69
d[d$Station == "LJK 4",]$depth <- 60
d[d$Station == "LJK20",]$depth <- 51
d[d$Station == "LJK 3",]$depth <- 56
d[d$Station == "Del Mar Mooring",]$depth <- 328
d[d$Station == "Charles -  LJK",]$depth <- 80
d[d$Station == "LJ (La Jolla Shores)",]$depth <- 30 
d[d$Station == "Pacific Beach NEW",]$depth <- 30

#size
d$TL <- NA
d[d$Transmitter=="A69-1601-56711",]$TL <- 148   
d[d$Transmitter=="A69-1601-56705",]$TL <- 77
d[d$Transmitter=="A69-1601-27063",]$TL <- 117
d[d$Transmitter=="A69-1601-56704",]$TL <- 153
d[d$Transmitter=="A69-9001-11125",]$TL <- 163
d[d$Transmitter=="A69-1601-56706",]$TL <- 118
d[d$Transmitter=="A69-1601-27070",]$TL <- 107

#tag date
d$tagdate <- NA
d[d$Transmitter=="A69-1601-56711",]$tagdate <- "2018-11-09"   
d[d$Transmitter=="A69-1601-56705",]$tagdate <- "2018-08-15"
d[d$Transmitter=="A69-1601-27063",]$tagdate <- "2018-11-16"
d[d$Transmitter=="A69-1601-56704",]$tagdate <- "2019-07-22"
d[d$Transmitter=="A69-9001-11125",]$tagdate <- "2019-07-23"
d[d$Transmitter=="A69-1601-56706",]$tagdate <- "2019-07-24"
d[d$Transmitter=="A69-1601-27070",]$tagdate <- "2019-10-26"

d$tagdate <- as.Date(d$tagdate)

#filter data for first day of final receiver pull
d <- filter(d, Date < "2021-06-03")


#QC data filtering
#remove detections on same day as release
d <- filter(d, Date != tagdate)

#remove detections at exact same time on any given receiver 
d %>%  
  group_by(Station, DateTime) %>% 
  filter(n() > 1) %>% 
  summarize(n = n())
#there are none... (keeping code to check as data gets updated)

#remove single detections within an hour
d3 <- d %>% 
  group_by(Date, Station, Transmitter) %>% 
  filter(n() > 1) 

#remove detections if time interval from last detection is less than minimum 
#time it takes to transmit signal
split <- d3 %>% 
  arrange(DateTime) %>% 
  group_by(Transmitter) %>% 
  group_split(Transmitter) %>% 
  purrr::map(mutate, time_diff = as.numeric(DateTime - lag(DateTime), units = "secs")) %>% 
  purrr::map(filter, time_diff > 30 | is.na(time_diff)) 
  
d4 <- bind_rows(split)


#write the data file
write.csv(d4, "Data/GSB_detections.csv")
