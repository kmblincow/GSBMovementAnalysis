#Kayla Blincow
#6/16/2021

#This code creates bubble maps of the detections of my GSBs
#Note: This code is pretty inefficient as a result of being a hodge podge from
#different time periods, but it works


#clear workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lubridate)
library(patchwork)

#import detections and receiver files
detections <- read.csv("Data/GSB_detections.csv")

#remove Del Mar
detections <- filter(detections, Station != "Del Mar Mooring")

#create an object which has the number of days each receiver was listening for
####NEED TO CONFIRM DEPLOYMENT PERIOD OF ANDY'S RECEIVERS!!!####
rec <- data.frame(Station = rep(unique(detections$Station), 6),
                  Days = NA,
                  Tag = sort(rep(unique(detections$Transmitter), 31)))

#27063
rec[rec$Station != "LJK 4" & rec$Station != "PL - North LJ"
    & rec$Tag == "A69-1601-27063",]$Days <- 930 
rec[rec$Station == "LJK 4" & rec$Tag == "A69-1601-27063",]$Days <- 930 - 51
rec[rec$Station == "PL - North LJ" 
    & rec$Tag == "A69-1601-27063",]$Days <- 930 - 548
rec[rec$Station == "Charles -  LJK" &
      rec$Tag == "A69-1601-27063",]$Days <- 588

#27070
rec[rec$Station != "LJK 4" & rec$Station != "PL - North LJ"
    & rec$Tag == "A69-1601-27070",]$Days <- 586 
rec[rec$Station == "LJK 4" & rec$Tag == "A69-1601-27070",]$Days <- 586 - 51
rec[rec$Station == "PL - North LJ" 
    & rec$Tag == "A69-1601-27070",]$Days <- 586 - 548
rec[rec$Station == "Charles -  LJK" &
      rec$Tag == "A69-1601-27070",]$Days <- 586

#56704
rec[rec$Station != "LJK 4" & rec$Station != "PL - North LJ"
    & rec$Tag == "A69-1601-56704",]$Days <- 680 
rec[rec$Station == "LJK 4" & rec$Tag == "A69-1601-56704",]$Days <- 680 - 51
rec[rec$Station == "PL - North LJ" 
    & rec$Tag == "A69-1601-56704",]$Days <- 680 - 548
rec[rec$Station == "Charles -  LJK" &
      rec$Tag == "A69-1601-56704",]$Days <- 588


#56705
rec[rec$Station != "LJK 4" & rec$Station != "PL - North LJ"
    & rec$Tag == "A69-1601-56705",]$Days <- 1023 
rec[rec$Station == "LJK 4" & rec$Tag == "A69-1601-56705",]$Days <- 1023 - 51
rec[rec$Station == "PL - North LJ" 
    & rec$Tag == "A69-1601-56705",]$Days <- 1023 - 548
rec[rec$Station == "Charles -  LJK" &
      rec$Tag == "A69-1601-56705",]$Days <- 588

#56706
rec[rec$Station != "LJK 4" & rec$Station != "PL - North LJ"
    & rec$Tag == "A69-1601-56706",]$Days <- 682 
rec[rec$Station == "LJK 4" & rec$Tag == "A69-1601-56706",]$Days <- 682 - 51
rec[rec$Station == "PL - North LJ" 
    & rec$Tag == "A69-1601-56706",]$Days <- 682 - 548
rec[rec$Station == "Charles -  LJK" &
      rec$Tag == "A69-1601-56706",]$Days <- 588

#56711
rec[rec$Station != "LJK 4" & rec$Station != "PL - North LJ"
    & rec$Tag == "A69-1601-56711",]$Days <- 937
rec[rec$Station == "LJK 4" & rec$Tag == "A69-1601-56711",]$Days <- 937 - 51
rec[rec$Station == "PL - North LJ" 
    & rec$Tag == "A69-1601-56711",]$Days <- 937 - 548
rec[rec$Station == "Charles -  LJK" &
      rec$Tag == "A69-1601-56711",]$Days <- 588

#join receiver days to detections dataframe
det <- left_join(detections, rec, by = c("Station" = "Station", 
                                         "Transmitter" = "Tag"))



#calculate number of detections per day for each fish at each receiver
days <- det %>% group_by(Station, Transmitter, 
                         Latitude, Longitude, Days, MPA) %>% 
  summarize(n_det = n(),
            det_p_day = n_det/Days) %>% 
  distinct()


#create receiver object
array <- dplyr::select(detections, Station, Latitude, Longitude, MPA) %>%
  distinct()

#create a column "tag" which is just the unique tagID
d <- separate(detections, col = "Transmitter", into = c("a", "b", "tag"), 
              sep = "-", remove = TRUE)
d$a <- NULL
d$b <- NULL

n.detections <- dim(d)[1]

#add total number of detections for each receiver
for(i in unique(d$Station)){
  array[array$Station==i,"Detections"]<-nrow(d[d$Station==i,])
}

#do this for each tag
#Nugget
for(i in unique(d$Station)){
  array[array$Station==i,"Nugget"]<-nrow(d[d$Station==i & d$tag==56705,])
}

#Woohoo
for(i in unique(d$Station)){
  array[array$Station==i,"Woohoo"]<-nrow(d[d$Station==i & d$tag==56711,])
}

#Yippee
for(i in unique(d$Station)){
  array[array$Station==i,"Yippee"]<-nrow(d[d$Station==i & d$tag==27063,])
}

#Pharaoh
for(i in unique(d$Station)){
  array[array$Station==i,"Pharaoh"]<-nrow(d[d$Station==i & d$tag==56704,])
}

# #Dave
# for(i in unique(d$Station)){
#   array[array$Station==i,"Dave"]<-nrow(d[d$Station==i & d$tag==11125,])
# }

#Jacare
for(i in unique(d$Station)){
  array[array$Station==i,"Jacare"] <- nrow(d[d$Station==i & d$tag==56706,])
}

#Sir Wellington
for(i in unique(d$Station)){
  array[array$Station==i,"SirW"]<-nrow(d[d$Station==i & d$tag==27070,])
}


#find sum of detections for each fish
t_Nug <- sum(array$Nugget)
t_Woo <- sum(array$Woohoo)
t_Yip <- sum(array$Yippee)
t_Phar <- sum(array$Pharaoh)
#t_Dave <- sum(array$Dave)
t_Jac <- sum(array$Jacare)
t_SirW <- sum(array$SirW)

#calculate percent of detections inside vs. outside reserves
#make column that says whether each station is inside or outside

inside <- filter(array, MPA == "in")
in_Nug <- sum(inside$Nugget)/t_Nug
in_Woo <- sum(inside$Woohoo)/t_Woo
in_Yip <- sum(inside$Yippee)/t_Yip
in_Phar <- sum(inside$Pharaoh)/t_Phar
#in_Dave <- sum(inside$Dave)/t_Dave
in_Jac <- sum(inside$Jacare)/t_Jac
in_SirW <- sum(inside$SirW)/t_SirW

Fish <- c("Nugget", "Woohoo", "Yippee", "Pharaoh", "Jacare", "SirW")
Prop_in <- c(in_Nug, in_Woo, in_Yip, in_Phar, in_Jac, in_SirW)
MPA <- as.data.frame(cbind(Fish, Prop_in))
MPA$Prop_out <- 1 - as.numeric(as.character(MPA$Prop_in))

#generate maps
library(maps)
library(ggmap)
library(mapdata)
library(ggsn)
library(ggspatial)
library(rgdal)
library(sp)


#pull our base map
cbbox <- with(array,c(lon=mean(Longitude),lat=mean(Latitude)-.005))
mymap <- get_map(location = cbbox, zoom = 13, source = "stamen", 
                  maptype = "terrain-background")

#pull/format MPA boundary data
mpasf <- readOGR("Data/MPAmapfiles/MPA_CA_StateAndFederal_180905.shp")
mpasf_df <- spTransform(mpasf, CRS("+proj=longlat +datum=WGS84"))

#turn it into a dataframe that ggplot will understand and filter for just the
#MPAs I care about
lj_mpasf <- broom::tidy(mpasf_df, region = "FULLNAME") %>% 
  filter(id == "Matlahuayl State Marine Reserve" |
           id == "San Diego-Scripps Coastal State Marine Conservation Area" |
           id == "South La Jolla State Marine Reserve" |
           id == "South La Jolla State Marine Conservation Area")

#load tagging location dataset
tagloc <- read.csv("Data/tagginglocations.csv", header = T)


#make a base map to build off of
my_Map <- ggmap(mymap) +
  theme_classic() +
  geom_point(data = array, aes(x = Longitude, y = Latitude), 
             color = "black", alpha = 0.1, size = 0.75) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  scalebar(dist = 1, dist_unit = "km", x.min = -117.33, 
           x.max = -117.30, y.min = 32.778, y.max = 32.85, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.05, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tl")

#Detections per day by receiver for each fish given the number of days the 
#receiver was listening and the number of days the fish was swimming
#Nugget
nug <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = det_p_day, color = MPA),
             data = filter(days, Transmitter == "A69-1601-56705")) +
  scale_size_continuous(limits=c(0.002, 60), name = "Detections per Day") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "56705")) +
  annotate("text", x = -117.335, y = 32.865, label = "56705") +
  labs(x = "", y = "") +
  theme(legend.position = "none")

png(file="Figures/Bubble56705.png",
    width = 2000,
    height = 1700,
    res = 300)
nug
dev.off()

#Woohoo
woo <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = det_p_day, color = MPA),
             data = filter(days, Transmitter == "A69-1601-56711")) +
  scale_size_continuous(limits=c(0.002, 60), name = "Detections per Day") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "56711")) +
  annotate("text", x = -117.335, y = 32.865, label = "56711") +
  labs(x = "") +
  theme(legend.position = "none") 

png(file="Figures/Bubble56711.png",
    width = 2000,
    height = 1700,
    res = 300)
woo
dev.off()

#Yippee
yip <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = det_p_day, color = MPA),
             data = filter(days, Transmitter == "A69-1601-27063")) +
  scale_size_continuous(limits=c(0.002, 60), name = "Detections per Day") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "27063")) +
  annotate("text", x = -117.335, y = 32.865, label = "27063") +
  labs(y = "") 

png(file="Figures/Bubble27063.png",
    width = 2000,
    height = 1700,
    res = 300)
yip
dev.off()

#Pharaoh
pha <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = det_p_day, color = MPA),
             data = filter(days, Transmitter == "A69-1601-56704")) +
  scale_size_continuous(limits=c(0.002, 60), name = "Detections per Day") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "56704")) +
  annotate("text", x = -117.335, y = 32.865, label = "56704") +
  theme(legend.position = "none")

png(file="Figures/Bubble56704.png",
    width = 2000,
    height = 1700,
    res = 300)
pha
dev.off()

#SirW
sirw <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = det_p_day, color = MPA),
             data = filter(days, Transmitter == "A69-1601-27070")) +
  scale_size_continuous(limits=c(0.002, 60), name = "Mean Detections/Month") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "27070")) +
  annotate("text", x = -117.335, y = 32.865, label = "27070") +
  labs(y = "") +
  theme(legend.position = "none")

png(file="Figures/Bubble27070.png",
    width = 2000,
    height = 1700,
    res = 300)
sirw
dev.off()

#MAKE A MULTI-PANEL PLOT INCLUDING THE TAG ABACUS PLOT
detections$Transmitter <- factor(detections$Transmitter, 
                                 labels = c("27063", "27070",
                                            "56704", "56705",
                                            "56706", "56711"))

p <- ggplot(filter(detections, Transmitter != "56706"), 
            aes(x = as.Date(Date), y = Transmitter)) +
  geom_point(shape = 16) +
  annotate("segment", x = as.Date("2018-08-14"), xend = as.Date("2018-08-14"),
           y = 3.75, yend = 4.25, size = 1) +
  annotate("segment", x = as.Date("2018-11-08"), xend = as.Date("2018-11-08"),
           y = 4.75, yend = 5.25, size = 1) +
  annotate("segment", x = as.Date("2018-11-15"), xend = as.Date("2018-11-15"),
           y = 0.75, yend = 1.25, size = 1) +
  annotate("segment", x = as.Date("2019-07-21"), xend = as.Date("2019-07-21"),
           y = 2.75, yend = 3.25, size = 1) +
  annotate("segment", x = as.Date("2019-10-25"), xend = as.Date("2019-10-25"),
           y = 1.75, yend = 2.25, size = 1) +
  labs(x = "Time", y = "Tag Number") +
  scale_x_date(date_labels = "%b %Y", 
               breaks = "6 months") +
  theme_classic()


(p + woo + nug)/(pha + sirw + yip)


png(file="Figures/AbacusMapCombo.png",
    width = 5000,
    height = 2500,
    res = 300)

(p + woo + nug)/(pha + sirw + yip)

dev.off()








##spawning versus non-spawning season
spwn <- detections %>% 
  filter(Transmitter != "A69-1601-56706") %>% 
  group_by(Station, Month, MPA, Latitude, Longitude, spawn) %>% 
  summarize(n_det = n(), 
            n_fish = n_distinct(Transmitter))

spwn_mnth <- detections %>% group_by(Station, Month, Transmitter, Latitude,
                        Longitude, MPA, spawn) %>% 
  summarize(det_p_mnth = n()) %>% 
  ungroup() %>% 
  group_by(Station, Latitude, Longitude, spawn, MPA) %>% 
  summarize(mean_det = mean(det_p_mnth),
            n_fish = n_distinct(Transmitter))


spwn_day <- detections %>% group_by(Transmitter, Station, Date, Latitude, 
                                    Longitude, spawn) %>% 
  summarize(ndet = n(),
            n_fish = n_distinct(Transmitter))


my_Map2 <- ggmap(mymap) +
  theme_classic() +
  geom_point(data = array, aes(x = Longitude, y = Latitude), 
             alpha = 0.2, size = 0.75) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  scalebar(dist = 1, dist_unit = "km", x.min = -117.33, 
           x.max = -117.30, y.min = 32.778, y.max = 32.85, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.05, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tl")

sp_seas <- my_Map2 +
  geom_point(aes(x = Longitude, y = Latitude, size = mean_det, color = n_fish),
             data = filter(spwn_mnth, spawn == "spwn")) +
  scale_size_continuous(limits=c(1, 2500), name = "Mean Detections/Month") +
  scale_color_gradient(low = "blue", high = "red", name = "# of Fish Detected")+
  annotate("text", x = -117.325, y = 32.865, label = "Spawning Season")

png(file="Figures/BubbleSpawn.png",
    width = 2300,
    height = 2100,
    res = 300)
sp_seas
dev.off()


nonsp_seas <- my_Map2 +
  geom_point(aes(x = Longitude, y = Latitude, size = mean_det, color = n_fish),
             data = filter(spwn_mnth, spawn == "nonspwn")) +
  scale_size_continuous(limits=c(1, 2500), name = "Mean Detections/Month") +
  scale_color_gradient(low = "blue", high = "red", name = "# of Fish Detected")+
  annotate("text", x = -117.323, y = 32.865, label = "Non-Spawning Season")

png(file="Figures/BubbleNonSpawn.png",
    width = 2300,
    height = 2100,
    res = 300)
nonsp_seas
dev.off()


#Try a new thing...
#let's do a bubble map where the size of the bubble is the proprotion of potential
#days in the spawning v. non-spawning season that a station detected a fish

#first need to know number of spawning season days in the study
library(lubridate)

#for most receivers
days <- seq.Date(as.Date("2018-08-15"), as.Date("2021-06-02"), by = "days")

days_df <- data.frame(Date = days,
                      Month = month(days))
days_df$spawn <- NA
days_df[days_df$Month >= 6 & days_df$Month <= 9,]$spawn <- "spwn"
days_df[days_df$Month < 6 | days_df$Month > 9,]$spawn <- "nonspwn"

days_df2 <- days_df %>% group_by(spawn) %>% summarize(n_tot = n())

#for LJK 4
days4 <- c(seq.Date(as.Date("2018-08-15"), as.Date("2021-01-04"), by = "days"),
           seq.Date(as.Date("2021-02-24"), as.Date("2021-06-02"), by = "days"))
days_df4 <- data.frame(Date = days4,
                      Month = month(days4))
days_df4$spawn <- NA
days_df4[days_df4$Month >= 6 & days_df4$Month <= 9,]$spawn <- "spwn"
days_df4[days_df4$Month < 6 | days_df4$Month > 9,]$spawn <- "nonspwn"

days_df24 <- days_df4 %>% group_by(spawn) %>% summarize(n_tot = n())

#for PL North LJ
daysPL <- c(seq.Date(as.Date("2018-08-15"), as.Date("2019-12-02"), by = "days"))
days_dfPL <- data.frame(Date = daysPL,
                       Month = month(daysPL))
days_dfPL$spawn <- NA
days_dfPL[days_dfPL$Month >= 6 & days_dfPL$Month <= 9,]$spawn <- "spwn"
days_dfPL[days_dfPL$Month < 6 | days_dfPL$Month > 9,]$spawn <- "nonspwn"

days_df2PL <- days_dfPL %>% group_by(spawn) %>% summarize(n_tot = n())

#combine the info!
spwn_d <- detections %>% group_by(Station, Date, Latitude, Longitude, spawn) %>% 
  summarize(n = n(),
            nfish = n_distinct(Transmitter))

spwn_d$n_tot <- NaN

spwn_d[spwn_d$Station != "LJK 4" & spwn_d$Station != "PL - North LJ" &
         spwn_d$spawn == "spwn",]$n_tot <- days_df2$n_tot[2]
spwn_d[spwn_d$Station != "LJK 4" & spwn_d$Station != "PL - North LJ" &
         spwn_d$spawn == "nonspwn",]$n_tot <- days_df2$n_tot[1]

spwn_d[spwn_d$Station == "LJK 4" &
         spwn_d$spawn == "spwn",]$n_tot <- days_df24$n_tot[2]
spwn_d[spwn_d$Station == "LJK 4" &
         spwn_d$spawn == "nonspwn",]$n_tot <- days_df24$n_tot[1]

spwn_d[spwn_d$Station == "PL - North LJ" &
         spwn_d$spawn == "spwn",]$n_tot <- days_df2PL$n_tot[2]
spwn_d[spwn_d$Station == "PL - North LJ" &
         spwn_d$spawn == "nonspwn",]$n_tot <- days_df2PL$n_tot[1]

#calculate proportion of spawning and non-spawning days each receiver detected a fish
spawn_d2 <- spwn_d %>% group_by(Station, Latitude, Longitude, spawn, n_tot) %>% 
  summarize(total_n = sum(n),
            total_fish = sum(nfish)) %>% 
  mutate(mean_n = total_n/n_tot,
         mean_fish = total_fish/n_tot)

#map it!
my_Map2 <- ggmap(mymap) +
  theme_classic() +
  geom_point(data = array, aes(x = Longitude, y = Latitude), 
             alpha = 0.2, size = 0.75) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  scalebar(dist = 1, dist_unit = "km", x.min = -117.33, 
           x.max = -117.30, y.min = 32.778, y.max = 32.85, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.05, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tl")

sp_seas <- my_Map2 +
  geom_point(aes(x = Longitude, y = Latitude, color = mean_fish, size = mean_n),
             data = filter(spawn_d2, spawn == "spwn")) +
  scale_color_gradient(low = "blue", high = "red", 
                       name = "# of Fish Detected")+
  annotate("text", x = -117.325, y = 32.865, label = "Spawning Season") 

nsp_seas <- my_Map2 +
  geom_point(aes(x = Longitude, y = Latitude, color = mean_fish, size = mean_n),
             data = filter(spawn_d2, spawn == "nonspwn")) +
  scale_color_gradient(low = "blue", high = "red", 
                       name = "# of Fish Detected")+
  annotate("text", x = -117.325, y = 32.865, label = "Non-Spawning Season")

library(patchwork)

png(file="Figures/BubbleSpawn.png",
    width = 4400,
    height = 2000,
    res = 300)
sp_seas + nsp_seas
dev.off()



#OBSOLETE OLD CODE

#all detections (not including Del Mar)
my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size=Detections, color = MPA),
             data = array) +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out"))

#Nugget
nug <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = Nugget, color = MPA),
             data = array) +
  scale_size_continuous(limits=c(1, 55000), name = "# of Detections") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "56705")) +
  annotate("text", x = -117.325, y = 32.865, label = "A69-1601-56705") 

png(file="Figures/Bubble56705.png",
    width = 2000,
    height = 1700,
    res = 300)
nug
dev.off()

#Woohoo
woo <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = Woohoo, color = MPA),
             data = array) +
  scale_size_continuous(limits=c(1, 55000), name = "# of Detections") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "56711")) +
  annotate("text", x = -117.325, y = 32.865, label = "A69-1601-56711")

png(file="Figures/Bubble56711.png",
    width = 2000,
    height = 1700,
    res = 300)
woo
dev.off()

#Yippee
yip <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = Yippee, color = MPA),
             data = array) +
  scale_size_continuous(limits=c(1, 55000), name = "# of Detections") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "27063")) +
  annotate("text", x = -117.325, y = 32.865, label = "A69-1601-27063")

png(file="Figures/Bubble27063.png",
    width = 2000,
    height = 1700,
    res = 300)
yip
dev.off()

#Pharaoh
pha <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = Pharaoh, color = MPA),
             data = array) +
  scale_size_continuous(limits=c(1, 55000), name = "# of Detections") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "56704")) +
  annotate("text", x = -117.325, y = 32.865, label = "A69-1601-56704")

png(file="Figures/Bubble56704.png",
    width = 2000,
    height = 1700,
    res = 300)
pha
dev.off()

#SirW
sirw <- my_Map +
  geom_point(aes(x = Longitude, y = Latitude, size = SirW, color = MPA),
             data = array) +
  scale_size_continuous(limits=c(1, 55000), name = "# of Detections") +
  scale_color_manual(values = c("#0072B2", "#000000"),
                     labels = c("MPA-In", "MPA-Out")) +
  geom_point(aes(x = Long, y = Lat), color = "red", shape = 8, size = 2,
             data = filter(tagloc, ID == "27070")) +
  annotate("text", x = -117.325, y = 32.865, label = "A69-1601-27070")

png(file="Figures/Bubble27070.png",
    width = 2000,
    height = 1700,
    res = 300)
sirw
dev.off()