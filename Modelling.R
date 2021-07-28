#Kayla Blincow
#6/17/2021

#The purpose of this script is to begin running models to explore the different
#relationships in the GSB movement data.

#I initially plan to run 4 models:
# 1. Detections/hr ~ Month + Time of Day + Lunar Phase + FishID
# 2. # of Receivers/d ~ Month + Time of Day + Lunar Phase + FishID
# 3. Presence/Absence MPAs ~ Month + Time of Day + FishID
# 4. Presence/Absence Array ~ Month + Time of Day + Lunar Phase + FishID


#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lme4)
library(merTools)

#load data
d <- read.csv("Data/GSB_detections.csv", header = T)

#remove 56706, because there are so few detections
#remove Del Mar Station since these models are specific to the array
d <- filter(d, Transmitter != "A69-1601-56706", 
            Station != "Del Mar Mooring")


####Activity Models####
#need to calculate number of detections per hour for each fish
act1 <- d %>% group_by(Transmitter, Date, Hour, Month, dielp, lunar) %>% 
  summarize(det = n())

#check that we have a unique value for each tag/date/hour
check <- act1 %>% group_by(Transmitter, Date, Hour) %>% 
  summarize(n = n()) %>% 
  filter(n > 1)
#we don't... I think this because the dielp measurements aren't on the hour
check <- left_join(check, act1) #yep

#going to convert duplicate diel periods in  hours to either dawn or dusk
for(i in 1:(nrow(check)-1)){
  if(check$Date[i] == check$Date[i + 1] & 
     check$Hour[i] == check$Hour[i + 1] & 
     check$dielp[i] != check$dielp[i + 1] &
     check$dielp[i] != "dusk" & 
     check$dielp[i] != "dawn"){
    check$dielp[i] <- check$dielp[i + 1]
  }
}

for(i in 1:(nrow(check)-1)){
  if(check$Date[i + 1] == check$Date[i] & 
     check$Hour[i + 1] == check$Hour[i] & 
     check$dielp[i + 1] != check$dielp[i] &
     check$dielp[i + 1] != "dusk" & 
     check$dielp[i + 1] != "dawn"){
    check$dielp[i + 1] <- check$dielp[i]
  }
}


#left_join again
act1a <- left_join(act1, check, 
          by = c("Transmitter" = "Transmitter", 
                 "Date" = "Date", 
                 "Hour" = "Hour", 
                 "Month" = "Month", 
                 "lunar" = "lunar", 
                 "det" = "det"))
act1a[is.na(act1a$dielp.y),]$dielp.y <- act1a[is.na(act1a$dielp.y),]$dielp.x

act1b <- act1a %>% select(-n, -dielp.x) %>% 
  rename(dielp = dielp.y) %>% 
  group_by(Transmitter, Date, Hour, Month, dielp, lunar) %>% 
  summarize(det = sum(det))
  
#check that we have a unique value for each tag/date/hour
check2 <- act1b %>% group_by(Transmitter, Date, Hour) %>% 
  summarize(n = n()) %>% 
  filter(n > 1)
#got it sorted! Yay!

#now we run the model! (using act1b data)
#first we need to classify our categorical variables as factors
act1b$Hour <- as.factor(act1b$Hour)
act1b$Month <- as.factor(act1b$Month)
act1b$lunar <- as.factor(act1b$lunar)
act1b$dielp <- as.factor(act1b$dielp)

#since we have a continuous response variable, we 
m_act1 <- lmer(det ~ Month + lunar + dielp + (1|Transmitter),
              data = act1b)
summary(m_act1)
plot(m_act1)

m_act1a <- lmer(det ~ Month + (1| Transmitter), data = act1b)
summary(m_act1a)

m_act1b <- lmer(det ~ lunar + (1| Transmitter), data = act1b)
summary(m_act1b)

m_act1c <- lmer(det ~ dielp + (1| Transmitter), data = act1b)
summary(m_act1c)

m_act1d <- lmer(det ~ dielp + Month*lunar + (1|Transmitter), data = act1b)
summary(m_act1d)

m_act1e <- lmer(det ~ dielp + Month + (1|Transmitter), data = act1b)
summary(m_act1e)

m_act1f <- lmer(det ~ Month*lunar + (1|Transmitter), data = act1b)
summary(m_act1f)

AIC(m_act1, m_act1a, m_act1b, m_act1c, m_act1d, m_act1e, m_act1f)

######USED THESE MODELS######
#Let's try modelling number of receivers visited per day as a measure of activity
act2 <- d %>% group_by(Transmitter, Date, Month, lunar, spawn) %>% 
  summarize(stations = n_distinct(Station))

act2$Month <- as.factor(act2$Month)

#let's build the model! Since we have counts we want to use a poisson glm
m_act2 <- glmer(stations ~ Month + lunar + (1|Transmitter), family = "poisson",
              data = act2)
summary(m_act2)

m_act2a <- glmer(stations ~ Month + (1|Transmitter), family = "poisson", 
                 data = act2)
summary(m_act2a)

m_act2b <- glmer(stations ~ lunar + (1|Transmitter), family = "poisson",
                 data = act2)
summary(m_act2b)

m_act2c <- glmer(stations ~ Month*lunar + (1|Transmitter), family = "poisson",
                 data = act2)
summary(m_act2c)

AIC(m_act2, m_act2a, m_act2b, m_act2c)
#m_act2a does the best at explaining the variability (effect of month)

p <- ggplot(act2, aes(x = Month, y = stations)) +
  geom_jitter(aes(color = Transmitter), alpha = 0.7, width = 0.25, 
              height = 0.1) +
  geom_violin(alpha = 0.6, fill = "gray30", scale = "count", bw = 0.7) +
  geom_boxplot(alpha = 0.7, width = 0.1) +
  geom_vline(xintercept = 4.5, size = 1.25) + 
  geom_vline(xintercept = 9.5, size = 1.25) +
  annotate("text", x = 7, y = 0.1, label = "Spawning Season") +
  scale_color_manual(values = c("#6971c9",
                      "#af953c",
                      "#a24f99",
                      "#56ae6c",
                      "#ba4a4f")) + 
  labs(y = "# of Stations Visited per Day") +
  theme_classic()

png(file="Figures/StationVisits.png",
    width = 2500,
    height = 1700,
    res = 300)
p
dev.off()

#check residuals
plot(residuals(m_act2a, type = "pearson") ~ predict(m_act2a, type = "link"))
plot(residuals(m_act2a, type = "pearson") ~ as.numeric(act2$Month))


#check model predictions
#with random effects
pred1 <- predict(m_act2a, 
                 newdata = data.frame(Month = gl(12, 5),
                                      Transmitter = sort(rep(
                                        unique(act2$Transmitter)))), 
        type = "response", level = 0)

#just global mean
newdata <- data.frame(Month = gl(12, 1))
pred2 <- predict(m_act2a, newdata = newdata, 
        type = "response", re.form = NA)

X <- model.matrix(formula(m_act2a,fixed.only=TRUE)[-2],
                  newdata)
V <- vcov(m_act2a)     ## var-cov matrix of beta
pred.se <- sqrt(diag(X %*% V %*% t(X))) ## SEs of predictions
predf <- data.frame(Month = gl(12,1),
                    Pred = log(pred2),
                    SE = pred.se)
predf$lower90 <- exp(predf$Pred - 1.645*predf$SE)
predf$upper90 <- exp(predf$Pred + 1.645*predf$SE)


#plot predicted values
ggplot(predf, aes(x = Month, y = exp(Pred))) +
  geom_jitter(data = act2, aes(x = Month, y = stations, color = Transmitter), 
              alpha = 0.5, width = 0.25, 
              height = 0.1) +
  geom_violin(data = act2, aes(x = Month, y = stations), 
              alpha = 0.3, fill = "gray30", scale = "count", bw = 0.7) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower90, ymax = upper90), width = 0.25, size = 1) +
  geom_vline(xintercept = 4.5, size = 1.25) + 
  geom_vline(xintercept = 9.5, size = 1.25) +
  annotate("text", x = 7, y = 0.1, label = "Spawning Season") +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "# of Stations Visited per Day") +
  theme_classic()

#Cool! I found a thing!
#Should combine this with a bubble map of spawning vs. non-spawning detections





#Should I just do it with spawn v. nonspawning?
#let's build the model! Since we have counts we want to use a poisson glm
m_act2d <- glmer(stations ~ spawn + lunar + (1|Transmitter), family = "poisson",
                data = act2)
summary(m_act2d)

m_act2e <- glmer(stations ~ spawn + (1|Transmitter), family = "poisson", 
                 data = act2)
summary(m_act2e)

m_act2f <- glmer(stations ~ lunar + (1|Transmitter), family = "poisson",
                 data = act2)
summary(m_act2f)

m_act2g <- glmer(stations ~ spawn*lunar + (1|Transmitter), family = "poisson",
                 data = act2)
summary(m_act2g)

AIC(m_act2d, m_act2e, m_act2f, m_act2g)
#m_act2e does best of these models
#check residuals
plot(residuals(m_act2e, type = "pearson") ~ predict(m_act2e, type = "link"))


#check model predictions
#with random effects
pred1 <- predict(m_act2e, 
                 newdata = data.frame(spawn = gl(2, 5, labels = c("spwn", "nonspwn")),
                                      Transmitter = sort(rep(
                                        unique(act2$Transmitter)))), 
                 type = "response", level = 0)

#just global mean
newdata <- data.frame(spawn = gl(2, 1, labels = c("nonspwn", "spwn")))
pred2 <- predict(m_act2e, newdata = newdata, 
                 type = "response", re.form = NA)

X <- model.matrix(formula(m_act2e,fixed.only=TRUE)[-2],
                  newdata)
V <- vcov(m_act2e)     ## var-cov matrix of beta
pred.se <- sqrt(diag(X %*% V %*% t(X))) ## SEs of predictions
predf <- data.frame(spawn = gl(2,1, labels = c("Non-Spawning", "Spawning")),
                    Pred = log(pred2),
                    SE = pred.se)
predf$lower90 <- exp(predf$Pred - 1.645*predf$SE)
predf$upper90 <- exp(predf$Pred + 1.645*predf$SE)

act2$Transmitter <- factor(act2$Transmitter, 
                           labels = c("27063", "27070", "56704", "56705", "56711"))
act2$spawn <- factor(act2$spawn, labels = c("Non-Spawning", "Spawning"))

#plot predicted values
p_spwn <- ggplot(predf, aes(x = spawn, y = exp(Pred))) +
  geom_jitter(data = act2, aes(x = spawn, y = stations, color = Transmitter), 
              alpha = 0.5, width = 0.3, 
              height = 0.1) +
  geom_violin(data = act2, aes(x = spawn, y = stations), 
              alpha = 0.3, fill = "gray30", scale = "count", bw = 0.7) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower90, ymax = upper90), width = 0.1) +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "# of Stations Visited Each Detection Day", x = "Season",
       color = "Tag Number") +
  theme_classic()

png(file="Figures/StationVisits_spwn.png",
    width = 2200,
    height = 1700,
    res = 300)
p_spwn
dev.off()




####Presence/Absence in MPAs####
#given that you are detected in LJ, are there any relationships explain whether
#you are detected inside or outside an MPA?
d$MPA <- as.factor(d$MPA)
d$Month <- as.factor(d$Month)

mpa1 <- d %>% group_by(Date, Hour, MPA, Month, dielp, lunar) %>% 
  summarize(n_det = n())

ggplot(mpa1, aes(x = MPA, y = n_det, color = dielp)) +
  geom_boxplot()

ggplot(mpa1, aes(x = MPA, y = n_det, color = Month)) +
  geom_boxplot()

ggplot(mpa1, aes(x = MPA, y = n_det, color = lunar)) +
  geom_boxplot()

#based on these preliminary plots, month looks like the only potential explanatory
#variable to me...

mpa2 <- d %>% group_by(Transmitter, Date, MPA, Month, lunar) %>% 
  summarize(n_det = n())

ggplot(mpa2, aes(x = MPA, y = n_det, color = Month)) +
  geom_boxplot()

ggplot(mpa2, aes(x = MPA, y = n_det, color = lunar)) +
  geom_boxplot()


######USED THESE MODELS######
#build some models!
m_MPA1 <- glmer(n_det ~ MPA*Month + (1|Transmitter), family = "poisson",
      data = mpa2)
summary(m_MPA1)
MuMIn::r.squaredGLMM(m_MPA1)

m_MPA2 <- glmer(n_det ~ MPA + Month + (1|Transmitter), family = "poisson", 
                data = mpa2)
summary(m_MPA2)

AIC(m_MPA1, m_MPA2)

#m_MPA1 is the model! it gives us insight into the differences between daily 
#detections inside vs outside MPAs by month
levels(mpa2$MPA) <- c("MPA-In", "MPA-Out")
mpa2$Transmitter <- factor(mpa2$Transmitter, labels = c("27063", "27070",
                                                        "56704", "56705",
                                                        "56711")) 

p2 <- ggplot(mpa2, aes(x = Month, y = n_det)) +
  geom_jitter(aes(color = Transmitter), alpha = 0.7, width = 0.2, 
              height = 0.1) +
  geom_violin(alpha = 0.6, fill = "gray30", scale = "count", bw = 50) +
  geom_boxplot(alpha = 0.7, width = 0.1) +
  facet_wrap(~MPA, ncol = 1, nrow = 2) +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "# of Detections per Day", color = "Tag Number") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA))

png(file="Figures/MPAdet.png",
    width = 2500,
    height = 1700,
    res = 300)
p2
dev.off()



####Presence/Absence in the Array####
#create full sequence of days during the study
dates <- seq(min(as.Date(d$Date)), max(as.Date(d$Date)), by = "day") %>%
  rep(5) %>% 
  as.data.frame()
names(dates) <- "Date"

dates$Transmitter <- rep(c(unique(d$Transmitter)), 1020) %>% 
  sort()


#summarize database to show presence/absence of each transmitter for each day
pinarr <- d %>% group_by(Transmitter, Date, Month) %>% 
  summarize(n_det = n()) 
pinarr$Date <- as.Date(pinarr$Date)

#join data with full set of dates
pinarr <- left_join(dates, pinarr) 

#fill in months
pinarr$Month <- month(pinarr$Date) 

#assign dates with no detections 0s in number of detections column
pinarr[is.na(pinarr$n_det),]$n_det <- 0 

#create new presence/absence column
pinarr$presabs <- NA

#assign 0s and 1s based on whether a fish was detected that day or not
pinarr[pinarr$n_det == 0,]$presabs <- 0
pinarr[pinarr$n_det > 0,]$presabs <- 1

#make month a factor
pinarr$Month <- as.factor(pinarr$Month)

#do some modelling!
m_pa <- glmer(presabs ~ Month + (1|Transmitter), data = pinarr, 
              family = "binomial")
summary(m_pa)

ggplot(pinarr, aes(x = Month, y = presabs)) +
  geom_jitter()
#meh not sure what's here...
#carry on with what I've already got and see if Brice thinks it's enough.