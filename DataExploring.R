#Kayla Blincow
#5/20/21
#more exploring 6/16/2021

#The purpose of this script is to visually explore potential relationships in the
#GSB detection data

#Relationships to look for:
#Greater presence outside of MPAs during Lobster Season
#Trends in presence in v. out MPAs
#Trends in presence in v. out of kelp areas
#Trends in residency


#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(zoo)
library(lubridate)
library(geosphere)

#load data
d <- read.csv("Data/GSB_detections.csv", header = T)

#remove 56706 cuz they don't have enough detections
d <- filter(d, Transmitter != "A69-1601-56706")

####Abacus plot (full array detections by fish thru time)####
ab <- d %>% group_by(Transmitter, Date) %>% 
  summarize(tot = n())

ab$Date <- as.Date(ab$Date)

#presence absence thru time
ggplot(ab, aes(x = Date, y = Transmitter)) +
  geom_point()

#look at number of detections per day
ggplot(ab, aes(x = Date, y = tot)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Transmitter)

#abacus plots split by station
ab_st <- d %>% group_by(Transmitter, Date, Month, Station) %>% 
  summarize(tot = n())

ab_st$Date <- as.Date(ab_st$Date)

ggplot(ab_st, aes(x = Date, y = Station)) +
  geom_point() + 
  facet_wrap(~Transmitter)

ggplot(ab_st, aes(x = as.factor(Month), y = Station)) +
  geom_point() +
  facet_wrap(~Transmitter)
         

####Activity: number of detections per hour by month####
#going to remove DelMar for now
d2 <- filter(d, Station != "Del Mar Mooring")

#calculate detections per hour
d2 <- d2 %>% group_by(Transmitter, Date, Month, Hour) %>% 
  summarize(tot = n())


ggplot(d2, aes(x = as.factor(Month), y = tot, color = Transmitter)) +
  geom_boxplot()

ggplot(d2, aes(x = as.Date(Date), y = tot)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Transmitter)

d2a <- d2 %>% group_by(Transmitter, Date) %>% 
  summarize(mean_det = mean(tot),
            tot_det = sum(tot))

ggplot(d2a, aes(x = as.Date(Date), y = mean_det)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Transmitter)

ggplot(d2a, aes(x = as.Date(Date), y = tot_det)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Transmitter)

d3 <- d2 %>% group_by(Transmitter, Month) %>% 
  summarize(tot_det = sum(tot),
            mean_det = mean(tot),
            sd_det = sd(tot))


#plot it
ggplot(d3, aes(x = Month, y = mean_det, color = Transmitter)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_det - sd_det, ymax = mean_det + sd_det),
                width = 0.3)
#meh... dunno about that..

ggplot(d3, aes(x = Month, y = tot_det, color = Transmitter)) +
  geom_point() +
  geom_line() 
#June and July have wayyy less detections... could be spawning related?

#by hour
d4 <- d2 %>% group_by(Transmitter, Hour) %>% 
  summarize(tot_det = sum(tot),
            mean_det = mean(tot), 
            sd_det = sd(tot))

ggplot(d4, aes(x = Hour, y = mean_det, color = Transmitter)) +
  geom_point() +
  geom_line() 
  # geom_errorbar(aes(ymin = mean_det - sd_det, ymax = mean_det + sd_det),
  #               width = 0.3)

ggplot(d4, aes(x = Hour, y = tot_det, color = Transmitter)) +
  geom_point() +
  geom_line() 

####Activity straight line distance####
splt <- d %>% filter(time_diff < 86400) %>% 
  group_by(Transmitter) %>% 
  arrange(as.Date(DateTime)) %>% 
  ungroup() %>% 
  group_split(Transmitter) %>% 
  as.list() 

#calculate distances between each detection
for(i in 1:length(splt)){
  splt[[i]]$dist <- NA
  for(j in 1:nrow(splt[[i]])){
    splt[[i]]$dist[j] <- distm(splt[[i]][j, c(10,9),j], splt[[i]][j + 1, c(10,9)])
  }
}

#how to handle roll over to next row when the time is too far apart? remove last 
#row from each day?
dist <- splt %>% bind_rows() %>% 
  group_by(Transmitter, Month, Date) %>% 
  slice(1:n()-1) %>% 
  summarize(totdist = sum(dist))
  
dist2 <- dist %>% group_by(Transmitter) %>% 
  summarize(meand = mean(totdist),
            maxd = max(totdist),
            mind = min(totdist))

ggplot(dist, aes(x = as.Date(Date), y = totdist/1000)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Transmitter)

ggplot(dist, aes(x = as.factor(Month), y = totdist, color = Transmitter)) +
  geom_boxplot()

dist3 <- dist %>% group_by(Transmitter, Month) %>% 
  summarize(meandm = mean(totdist),
            maxdm = max(totdist),
            mindm = min(totdist))

ggplot(dist3, aes(x = Month, y = meandm)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Transmitter)

#check the max distance traveled
dist[dist$totdist == max(dist$totdist),]

filter(d, Transmitter == "A69-1601-56705" & Date == "2021-02-14") %>% 
  distinct(Station)

d5 <- splt[[4]] %>% filter(Date == "2021-02-14") %>% as.data.frame() 
#hmmm looks like the fish actually just sat on the edge of the detection range of
#LJK8, LJK10, and LJK33

#what about the next most active?
dist[dist$totdist > 164669,]
filter(d, Transmitter == "A69-1601-56711" & Date == "2019-08-16") %>% 
  distinct(Station)

splt[[6]] %>% filter(Date == "2019-08-16") %>% as.data.frame()

#not sure this is going to work how I want it to... 
#maybe use number of receivers detected in a day to compare activity?

d6 <- d %>% group_by(Transmitter, Date, Month) %>% 
  summarize(statnum = n_distinct(Station))

ggplot(d6, aes(x = as.Date(Date), y = statnum)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Transmitter)

ggplot(d6, aes(x = as.factor(Month), y = statnum)) +
  geom_boxplot() +
  facet_wrap(~Transmitter)

####time of day differences?####
d7 <- d %>% group_by(Transmitter, Date, Hour, Month, dielp) %>% 
  summarize(det_hr = n())

ggplot(d7, aes(x = dielp, y = det_hr)) +
  geom_boxplot() +
  facet_wrap(~Transmitter)

ggplot(d7, aes(x = dielp, y = det_hr, color = Transmitter)) +
  geom_boxplot() +
  facet_wrap(~Month)

ggplot(d7, aes(x = as.factor(Month), y = det_hr, color = dielp)) +
  geom_boxplot() +
  facet_wrap(~Transmitter)

ggplot(d7, aes(x = as.factor(Month), y = det_hr, color = dielp)) +
  geom_boxplot()

####Do fish ever co-occur?####

d8 <- d %>% group_by(Station, Date, Month, Hour) %>% 
  distinct(Transmitter) %>% 
  summarize(n = n(),
            ID = Transmitter)

grpdates <- d8[d8$n > 1,]

ggplot(grpdates, aes(x = as.Date(Date), y = Station, color = ID)) +
  geom_jitter()

grpdates2 <- grpdates %>% distinct(Station, Date, Hour)

hist(grpdates2$Month)
hist(grpdates2$Hour)


ggplot(grpdates2 %>% group_by(Station, Month) %>% summarize(n = n()), aes(fill = Station, x = as.factor(Month), y = n)) + 
  geom_bar(position="stack", stat="identity")


####MPA relationships####
MPA <- d %>% 
  mutate(yearmon = as.yearmon(paste(Year, Month, sep = "-"), "%Y-%m")) %>% 
  group_by(Transmitter, yearmon, MPA) %>% 
  summarize(n = n())  
  
MPA$Transmitter <- as.factor(MPA$Transmitter)  

ggplot(MPA) +
  geom_line(aes(x = yearmon, y = n, color = MPA)) +
  facet_wrap(~Transmitter)

MPA2 <- d %>% 
  filter(MPA != "DelMar") %>% 
  group_by(Transmitter, Month, MPA) %>% 
  summarize(n = n())  

ggplot(MPA2) +
  geom_line(aes(x = Month, y = n, color = MPA)) +
  facet_wrap(~Transmitter)

#kelp relationships
kelp <- d %>% 
  mutate(yearmon = as.yearmon(paste(Year, Month, sep = "-"), "%Y-%m")) %>% 
  group_by(Transmitter, yearmon, kelp) %>% 
  summarize(n = n())  

ggplot(kelp) +
  geom_line(aes(x = yearmon, y = n, color = kelp)) +
  facet_wrap(~Transmitter) 

kelp2 <- d %>% 
  filter(MPA != "DelMar") %>% 
  group_by(Transmitter, Month, kelp) %>% 
  summarize(n = n())  

ggplot(kelp2) +
  geom_line(aes(x = Month, y = n, color = kelp)) +
  facet_wrap(~Transmitter)


#residency
#look at just La Jolla
LJ <- filter(d, Station != "Del Mar Mooring")

#caclulate total detection span (TDS) for each fish
TDS <- LJ %>% 
  group_by(Transmitter) %>% 
  summarize(TDS = difftime(max(Date), min(Date), units = "days")+1)
  
#calculate number of days a fish was detected in the array
det <- LJ %>% 
  distinct(Transmitter, Date) %>%
  group_by(Transmitter) %>% 
  summarize(n = n())

#residency
TDS$TDS <- as.numeric(TDS$TDS)
res <- left_join(TDS, det) %>% 
  mutate(res = n/TDS)
#Pharaoh spends all it's time in the LJ kelp? cool...
#how much does he move in a day??

ggplot(LJ, aes(x = as.Date(Date), y = Station)) +
  geom_point() +
  facet_wrap(~Transmitter)


#monthly residency
mTDS <- LJ %>% 
  group_by(Transmitter, Year, Month) %>% 
  summarize(TDS = difftime(max(Date), min(Date), units = "days")+1)

mTDS$days <- NaN
mTDS[mTDS$Month == 1 | mTDS$Month == 3 | mTDS$Month == 5 | mTDS$Month == 7 |
       mTDS$Month == 8 | mTDS$Month == 10 | mTDS$Month == 12,]$days <- 31
mTDS[mTDS$Month == 4 | mTDS$Month == 6 | mTDS$Month == 9 | 
       mTDS$Month == 11,]$days <- 30
mTDS[mTDS$Month == 2 & mTDS$Year!= 2020,]$days <- 28
mTDS[mTDS$Month == 2 & mTDS$Year== 2020,]$days <- 29

mTDS$mres <- mTDS$TDS/mTDS$days

mTDS <- mTDS %>% 
  mutate(yearmon = as.yearmon(paste(Year, Month, sep = "-"), "%Y-%m"))

ggplot(mTDS, aes(x = as.factor(Month), y = as.numeric(mres))) +
  geom_boxplot()

ggplot(mTDS, aes(x = as.factor(Month), y = as.numeric(mres))) +
  geom_boxplot(aes(fill = Transmitter))

ggplot(mTDS) +
  geom_line(aes(x = yearmon, y = as.numeric(mres), color = Transmitter))


#site fidelity
totalh <- d %>% distinct(Transmitter, Date, Hour) %>% 
  group_by(Transmitter) %>% 
  summarize(n = n())


sitefid <- d %>% 
  distinct(Transmitter, Station, Date, Hour) %>% 
  group_by(Transmitter, Station) %>%
  summarize(statn = n()) 
  
sitefid <- left_join(sitefid, totalh) %>% 
  mutate(fid = statn/n)

ggplot(sitefid) +
  geom_point(aes(x = as.factor(Station), y = fid, color = Transmitter))


