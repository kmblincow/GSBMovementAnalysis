library(tidyverse)

d %>% filter(Station != "Del Mar Mooring") %>% 
  group_by(Transmitter) %>% 
  summarize(minD = min(as.Date(Date)),
            maxD = max(as.Date(Date)),
            diff = maxD - minD,
            diffmonths = diff/30,
            diffyears = diff/365)

d %>% filter(Station == "Del Mar Mooring", as.Date(Date) < "2020-06-25") %>% 
  group_by(Transmitter) %>% 
  summarize(minD = min(as.Date(Date)),
            maxD = max(as.Date(Date)),
            diff = maxD - minD,
            diffmonths = diff/30,
            diffyears = diff/365)

library(VTrack)
recs<- d %>% 
  dplyr::select(Station, Latitude, Longitude) %>% 
  distinct() %>% 
  mutate(RADIUS = 600)

colnames(recs) <- c("LOCATION", "LATITUDE", "LONGITUDE", "RADIUS")

#create distance matrix
distM <- GenerateDirectDistance(recs)

d2 <- d %>% filter(Transmitter == "A69-1601-56711" & 
                     as.Date(Date) > "2019-02-01" &
               as.Date(Date) <= "2019-03-01") 


8.252957/(20900/3600)
