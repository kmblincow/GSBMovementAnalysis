#Kayla Blincow
#6/25/2021

#Exploring the utility of VTrack for the GSB study

#clear my workspace 
rm(list = ls())

#load packages
library(tidyverse)
library(VTrack)
library(lme4)
library(effects)

#Skip the data formatting and modeling by loading this file
#load data for nonzero movement models
load("C:/Users/kmbli/OneDrive - UC San Diego/PhDzNuts/Giant Sea Bass/MovementMS/GSBMovementAnalysis/DistanceData.RData")


#Generate Direct Distance example
# Load the points file
data(PointsDirect_crocs)
# Generate the direct distance 
matrixDirectDM <- GenerateDirectDistance(PointsDirect_crocs)

# Now plot example of how direct distances between receivers were generated
# In this example there are no structural boundary preventing an individual from
#moving between receivers
par(mfrow=c(1,1),las=1 ,bty="l")

plot(PointsDirect_crocs$LONGITUDE,
     PointsDirect_crocs$LATITUDE,
     pch=10,cex=1,xlab="Longitude",ylab="Latitude")

for(i in 1:length(PointsDirect_crocs$LONGITUDE)){
  lines(PointsDirect_crocs$LONGITUDE[c(1,i)],
        PointsDirect_crocs$LATITUDE[c(1,i)],
        lwd=0.3,col="grey",lty=3)}

points(PointsDirect_crocs$LONGITUDE,
       PointsDirect_crocs$LATITUDE,
       pch=10,cex=1)

#Do this with the GSB data
d <- read.csv("Data/GSB_detections.csv", header = T)


#create dataframe which is location of each Station in LJ array
#setting the detection radius to 600m to ensure that there is adequate distance
#between receivers (no overlapping detections)
recs <- d %>% 
  dplyr::select(Station, Latitude, Longitude) %>% 
  filter(Station != "Del Mar Mooring") %>% 
  distinct() %>% 
  mutate(RADIUS = 600)
  
colnames(recs) <- c("LOCATION", "LATITUDE", "LONGITUDE", "RADIUS")

#create distance matrix
distM <- GenerateDirectDistance(recs)

#create same plot just to scope
par(mfrow=c(1,1),las=1 ,bty="l")

plot(recs$LONGITUDE,
     recs$LATITUDE,
     pch=10,cex=1,xlab="Longitude",ylab="Latitude")

for(i in 1:length(recs$LONGITUDE)){
  lines(recs$LONGITUDE[c(1,i)],
        recs$LATITUDE[c(1,i)],
        lwd=0.3,col="grey",lty=3)}
#checks out/makes sense... 

#Calculate distance traveled per day for each fish using the new distance matrix

#for each day and each tag, what is the sequential list of receivers visited?
dailydist <- d %>% 
  filter(Station != "Del Mar Mooring") %>% 
  arrange(Transmitter, Date)
  

#convert distance matrix to long dataframe
dist_df <- pivot_longer(distM, cols = 2:32, names_to = "DM2")

dailydist$dist_diff <- NA

for(i in 1:(nrow(dailydist)-1)){
  if(dailydist$Transmitter[i] == dailydist$Transmitter[i+1]){
    dailydist$dist_diff[i+1] <- dist_df[dist_df$DM == dailydist$Station[i] &
                                     dist_df$DM2 == dailydist$Station[i+1],]$value
    
  }
}

#explore movement rates
rates <- dailydist %>% mutate(rate = dist_diff/time_diff)

ggplot(filter(rates, rate >0), aes(x = Transmitter, y = rate)) +
  geom_jitter() +
  geom_boxplot()

rates[rates$rate == max(rates$rate, na.rm = T),]

rates %>% arrange(desc(rate)) %>% slice(1:10)
#these all seem semi-reasonable to me...

ggplot(filter(rates, rate > 0), aes(x = Transmitter, y = rate, color = spawn)) +
  geom_boxplot()

#Do a binomial glmm to see if movement is more likely in one time or another

#remove the 56706 which was only detected at two receivers
rates <- filter(rates, Transmitter != "A69-1601-56706")

rates$binom <- 0
rates[rates$rate > 0 & !is.na(rates$rate), ]$binom <- 1
rates$Month <- as.factor(rates$Month)

#run the models!
rate_m1 <- glmer(binom ~ dielp + Month + (1|Transmitter),
                 data = rates, family = "binomial")

summary(rate_m1) #use this one
MuMIn::r.squaredGLMM(rate_m1)

rate_m1a <- glmer(binom ~ dielp + spawn + (1|Transmitter),
                  data = rates, family = "binomial")
summary(rate_m1a)


rate_m1b <- blmer(binom ~ dielp*spawn + (1|Transmitter),
                  data = rates, family = "binomial")

rate_m1c <- glmer(binom ~ as.factor(Month) + (1|Transmitter),
                  data = rates, family = "binomial")
summary(rate_m1c)

ggplot(rates, aes(x = as.factor(Month))) + geom_histogram(stat = "count")
ggplot(rates, aes(x = as.factor(dielp))) + geom_histogram(stat = "count")

#check model predictions
effects_m2 <- effects::effect(term = "Month", mod = rate_m1) %>% as.data.frame()
effects_m2$Month <- factor(effects_m2$Month,
                           levels = c("1", "2", "3", "4", "5", "6",
                                      "7", "8", "9", "10", "11", "12"))

p <- ggplot(effects_m2, aes(x = Month, y = fit)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = 1) +
  # geom_vline(xintercept = 4.5, size = 1.25) +
  # geom_vline(xintercept = 9.5, size = 1.25) +
  # annotate("text", x = 7, y = 0.001, label = "Spawning Season") +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "Probability of\nNon-Zero Movement Rate") +
  theme_classic()




#binomial glmm to see if movement in/out of reserves more likely in certain months
rates$MPAbinom <- 0

for(i in 1:(nrow(rates)-1)) {
  if(rates$Transmitter[i] == rates$Transmitter[i+1] &
     rates$MPA[i] != rates$MPA[i+1]){
    rates$MPAbinom[i+1] <- 1
  } else {
    rates$MPAbinom[i+1] <- 0
  }
}

rate_m3 <- glmer(MPAbinom ~ dielp + as.factor(Month) + (1|Transmitter), 
                 data = rates, family = "binomial")
summary(rate_m3)
#meh, there's not really much of a pattern here... I'm not sure it serves the
#story, will probably not include it


#check non-zero movement rates
nz_rates <- filter(rates, rate > 0)
library(nlme)
rate_m2 <- lme(log(rate) ~ dielp + as.factor(Month), random = ~1|Transmitter,
                 data = nz_rates)
summary(rate_m2)

rate_m2x <- lmer(log(rate) ~ dielp + as.factor(Month) + (1|Transmitter),
               data = nz_rates)
summary(rate_m2x)


rate_m2a <- lme(log(rate) ~ spawn, random = ~1|Transmitter, data = nz_rates)
summary(rate_m2a)

rate_m2b <- lmer(log(rate) ~ as.factor(Month)*dielp + (1|Transmitter),
                data = nz_rates)
summary(rate_m2b)

ggplot(nz_rates, aes(x = log(rate))) + geom_histogram()

ggplot(nz_rates, aes(x = dielp, y = log(rate))) +
  geom_violin()

ggplot(nz_rates, aes(x = as.factor(Month), y = log(rate))) +
  geom_violin()

ggplot(nz_rates, aes(x = spawn, y = log(rate))) +
  geom_violin()

ggplot(nz_rates, aes(x = as.factor(Month), y = log(rate), color = dielp)) +
  geom_violin()



#calculate distance metrics per day
mvmt <- rates %>% group_by(Transmitter, Date, MPA, spawn, Month) %>%
  summarize(total_movement = sum(dist_diff, na.rm = T),
            num_rec = n_distinct(Station),
            avg_rate = mean(rate))

ggplot(mvmt, aes(x = as.factor(Month), y = total_movement, 
                 color = Transmitter)) +
  geom_boxplot() 

ggplot(mvmt, aes(x = as.factor(spawn), y = total_movement, 
                 color = Transmitter)) +
  geom_boxplot() 

ggplot(mvmt, aes(x = as.factor(spawn), y = total_movement)) +
  geom_boxplot() 

ggplot(mvmt, aes(x = as.Date(Date), y = total_movement)) +
  geom_line() +
  facet_wrap(~Transmitter)


ggplot(mvmt, aes(x = Transmitter, y = log(avg_rate ), color = spawn)) +
  geom_boxplot()



####What if I only look at detections where the station changes?####
rates$keep <- 0

for(i in 1:(nrow(rates)-1)){
  if(rates$Station[i] != rates$Station[i+1]){
    rates$keep[i+1] <- 1
  } 
}

rates2 <- filter(rates, keep >0)
rates2$Month <- as.factor(rates2$Month)

rate_m1x <- glmer(binom ~ dielp + Month + (1|Transmitter),
                  data = rates2, family = "binomial")
summary(rate_m1x)
MuMIn::r.squaredGLMM(rate_m1x)

library(effects)
effects_m <- effects::effect(term= "Month", mod = rate_m1x) %>% as.data.frame()

effects_m$Month <- factor(effects_m$Month,
                          levels = c("1", "2", "3", "4", "5", "6",
                                     "7", "8", "9", "10", "11", "12"))
ggplot(effects_m, aes(x = Month, y = fit)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, size = 1) +
  # geom_vline(xintercept = 4.5, size = 1.25) +
  # geom_vline(xintercept = 9.5, size = 1.25) +
  # annotate("text", x = 7, y = 0.001, label = "Spawning Season") +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "Probability of Movement") +
  theme_classic()



nz_rates2 <- filter(rates2, rate > 0)
library(nlme)
rate_m2 <- lme(log(rate) ~ dielp + Month, random = ~1|Transmitter,
               data = nz_rates2)
summary(rate_m2)

effects_m3 <- effects::effect(term= "Month", mod = rate_m2) %>% as.data.frame()

effects_m3$Month <- factor(effects_m3$Month,
                          levels = c("1", "2", "3", "4", "5", "6",
                                     "7", "8", "9", "10", "11", "12"))
ggplot(effects_m3, aes(x = Month, y = exp(fit))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = exp(lower), ymax = exp(upper)), width = 0.25, size = 1) +
  # geom_vline(xintercept = 4.5, size = 1.25) +
  # geom_vline(xintercept = 9.5, size = 1.25) +
  # annotate("text", x = 7, y = 0.001, label = "Spawning Season") +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "Movement Rate") +
  theme_classic()

