#Kayla Blincow
#6/25/2021

#Study Map

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(maps)
library(ggmap)
library(mapdata)
library(ggsn)
library(ggspatial)
library(rgdal)
library(sp)

#load my data
d <- read.csv("Data/GSB_detections.csv", header = T)

#get unique stations from that data
stations <- d %>% dplyr::select(Station, Latitude, Longitude) %>% 
  distinct()

#load tagging location dataset
tagloc <- read.csv("Data/tagginglocations.csv", header = T)

#other receivers/bigger picture
bigd <- read.csv("Data/OtherReceivers.csv", header = T)

####Make Ze Maps!####

####Bigger Area Map####
#pull our base map
cbbox <- with(d,c(lon=mean(Longitude)-3.5,lat=mean(Latitude)-3))
mymap <- get_map(location = cbbox, size = c(1280,640),
                 zoom = 6, source = "stamen", 
                 maptype = "terrain-background")

ggmap(mymap)

#make a base map to build off of
base <- ggmap(mymap) +
  theme_classic() +
  geom_point(data = bigd, aes(x = Longitude, y = Latitude), 
             color = "black", size = 2) +
  geom_point(data = dplyr::filter(bigd,
                             Station == "LJ"),
               aes(x = Longitude, y = Latitude),
               color = "black", size = 6, shape = 0) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA))


####Smaller Area Map####
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

#add info to LJ stations
TP <- c("TP",	32.89223,	-117.26051)
stations <- rbind(stations, TP)
stations$Latitude <- as.numeric(stations$Latitude)
stations$Longitude <- as.numeric(stations$Longitude)

stations$range <- NA
stations[stations$Station == "LJK20" |
           stations$Station == "LJK 19" |
           stations$Station == "LJK 11" |
           stations$Station == "LJK 3" |
           stations$Station == "LJK 7" |
           stations$Station == "Charles -  LJK", ]$range <- "Y"
stations$core <- NA
stations[stations$Station == "TP" |
           stations$Station == "Del Mar Mooring", ]$core <- "out"
stations[stations$Station != "TP" &
           stations$Station != "Del Mar Mooring", ]$core <- "in"


#pull our base map
cbbox2 <- with(d,c(lon=mean(Longitude),lat=mean(Latitude)-0.02))
mymap2 <- get_map(location = cbbox2, zoom = 12, source = "stamen", 
                 maptype = "terrain-background")

zoom <- ggmap(mymap2) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "skyblue4", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "San Diego-Scripps Coastal State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "skyblue4", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_point(data = stations, aes(y = Latitude, x = Longitude, color = core),
             size = 3) +
  geom_point(data = dplyr::filter(stations, range == "Y"), 
             aes(x = Longitude, y = Latitude), shape = 1, size = 3) +
  geom_point(data = tagloc, aes(y = Lat, x = Long),
             shape = 8, size = 3) +
  scale_color_manual(values = c("#9a5ea1",
                                "#98823c")) +
  labs(x = "", y = "") +
  scalebar(dist = 2, dist_unit = "km", x.min = -117.40, 
           x.max = -117.35, y.min = 32.77, y.max = 32.85, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.05, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tl") +
  ggtitle("La Jolla Region") +
  annotate("text", y = 32.92955, x = -117.337, label = "Del Mar") +
  annotate("text", y= 32.89223,	x = -117.2875, label = "Torrey Pines") +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size = 1.5),
        plot.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
  

library(patchwork)

base + inset_element(zoom, left = 0.01, top = 0.8, 
                     bottom = 0.01, right = 0.7)

png(file="Figures/StudyMap.png",
    width = 3000,
    height = 2500,
    res = 300)

base + inset_element(zoom, left = 0.01, top = 0.8, 
                     bottom = 0.01, right = 0.7)

dev.off()



array <- ggmap(mymap2) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "skyblue4", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "San Diego-Scripps Coastal State Marine Conservation Area"), 
               aes(x = long, y = lat), fill = NA, 
               color = "skyblue4", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "Matlahuayl State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_polygon(data = filter(lj_mpasf, 
                             id == "South La Jolla State Marine Reserve"), 
               aes(x = long, y = lat), fill = NA, 
               color = "darkblue", size = 1) +
  geom_point(data = stations, aes(y = Latitude, x = Longitude),
             size = 3) +
  labs(x = "", y = "") +
  scalebar(dist = 2, dist_unit = "km", x.min = -117.40, 
           x.max = -117.35, y.min = 32.77, y.max = 32.85, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.05, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tl") 


png(file="Figures/array.png",
    width = 3000,
    height = 2800,
    res = 300)

array

dev.off()
