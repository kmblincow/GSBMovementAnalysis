#Kayla Blincow
#6/13/2021

#Range Testing, calculating distance between VR100 points and receiver

#then pulling in the final data and plotting the results


#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(geosphere)

#set my working directory
setwd("C:/Users/kmbli/OneDrive - UC San Diego/PhDzNuts/ArrayStuff/LJRangeTest")

#load my data
d <- read.csv("RangeDistance.csv", header = T)

#calculate the distance between VR100 reads (lat/long) and receiver position (LatR, LongR)

#create longitude/latitude objects
recs <- d %>%  
  group_by(Receiver) %>% 
  distinct(LatR, LongR)

VR100_20 <- d %>% 
  filter(Receiver == "LJK20") %>%
  select(Long, Lat)

VR100_19 <- d %>% 
  filter(Receiver == "LJK19") %>%
  select(Long, Lat)

VR100_11 <- d %>% 
  filter(Receiver == "LJK11") %>%
  select(Long, Lat)

VR100_3 <- d %>% 
  filter(Receiver == "LJK3") %>%
  select(Long, Lat)

VR100_Charles <- d %>% 
  filter(Receiver == "Charles") %>%
  select(Long, Lat)

VR100_7 <- d %>% 
  filter(Receiver == "LJK7") %>%
  select(Long, Lat)


dist_20 <- distm(recs[1,c(3,2)], VR100_20)
dist_19 <- distm(recs[2,c(3,2)], VR100_19)
dist_11 <- distm(recs[3,c(3,2)], VR100_11)
dist_3 <- distm(recs[4,c(3,2)], VR100_3)
dist_Charles <- distm(recs[5,c(3,2)], VR100_Charles)
dist_7 <- distm(recs[6,c(3,2)], VR100_7)

d$dist <- c(dist_20, dist_19, dist_11, dist_3, dist_Charles, dist_7)

#write the csv
write.csv(d, "RangeDistance.csv")


#now actually plot the range test
d2 <- read.csv("LJRangeTestData.csv", header = T)


#build the glm
g <- glm(RecDet~Dist, family=binomial, d2) 

# At what distance is detection probability 50%?
newdata = data.frame(Dist = seq(226, 227, 0.1))
predict(g, newdata, type="response")



p <- ggplot(d2, aes(x = Dist, y = RecDet)) +
  geom_vline(xintercept = 226.5, linetype = "dotted") +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  theme_classic() +
  labs(x = "Distance (m)", y = "Probability of Detection")

png(file="RangeTestAnalysis.png",
    width = 2000,
    height = 1700,
    res = 300)
p
dev.off()
