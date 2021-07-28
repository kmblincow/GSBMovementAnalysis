#Kayla Blincow
#6/28/2021

#3 Panel Figure
#The purpose of this script is to generate a 3 panel figure showing the number of
#detections/day inside and outside of MPAs by month and the probability of 
#non-zero movement

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lme4)
library(patchwork)

#load data for nonzero movement models
load("C:/Users/kmbli/OneDrive - UC San Diego/PhDzNuts/Giant Sea Bass/MovementMS/GSBMovementAnalysis/DistanceData.RData")


#plot predicted values
p <- p +
   geom_vline(xintercept = 4.5, size = 1) +
   geom_vline(xintercept = 10.5, size = 1) 


#p is the plot I want to add the MPA plots

#do the MPA detection models
d <- d %>% filter(Transmitter != "A69-1601-56706" & Station != "Del Mar Mooring")
mpa2 <- d %>% group_by(Transmitter, Date, MPA, Month, lunar) %>% 
  summarize(n_det = n())


mpa2$Month <- as.factor(mpa2$Month)

######USED THESE MODELS######
#build some models!
m_MPA1 <- glmer(n_det ~ Month*MPA + (1|Transmitter), family = "poisson",
                data = mpa2)
summary(m_MPA1)

#check model predictions
#just global mean
effects_m <- effects::effect(term = "Month*MPA", mod = m_MPA1)  %>% 
  as.data.frame()


effects_m$Month <- factor(effects_m$Month,
                           levels = c("1", "2", "3", "4", "5", "6",
                                      "7", "8", "9", "10", "11", "12"))


#m_MPA1 is the model! it gives us insight into the differences between daily 
#detections inside vs outside MPAs by month
mpa2$Transmitter <- factor(mpa2$Transmitter, labels = c("27063", "27070",
                                                        "56704", "56705",
                                                        "56711"))
library(scales)
p_in <- ggplot(data = dplyr::filter(effects_m, MPA == "in")) +
  geom_jitter(data = dplyr::filter(mpa2, MPA == "in"), 
                aes(x = Month, y = n_det, color = Transmitter),
              alpha = 0.7, width = 0.2, height = 0.1) +
  geom_violin(data = dplyr::filter(mpa2, MPA == "in"), 
              aes(x = Month, y = n_det),
              alpha = 0.4, fill = "gray30", scale = "count", bw = 50) +
  geom_point(aes(x = Month, y = fit),
             size = 2) +
  geom_errorbar(aes(x = Month, ymin = lower, ymax = upper), 
                width = 0.1, size = 1) +
  geom_vline(xintercept = 4.5, size = 1) +
  geom_vline(xintercept = 10.5, size = 1) +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "Daily Detection Count\n(Inside MPAs)", x = "", color = "Tag Number") +
  ylim(c(0, 1150)) +
  annotate("text", x = 0.75, y = 1150, label = "(a)") +
  theme_classic() +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5))

p_out <- ggplot(data = dplyr::filter(effects_m, MPA == "out")) +
  geom_jitter(data = dplyr::filter(mpa2, MPA == "out"), 
              aes(x = Month, y = n_det, color = Transmitter),
              alpha = 0.7, width = 0.2, height = 0.1) +
  geom_violin(data = dplyr::filter(mpa2, MPA == "out"), 
              aes(x = Month, y = n_det),
              alpha = 0.4, fill = "gray30", scale = "count", bw = 50) +
  geom_point(aes(x = Month, y = fit),
             size = 2) +
  geom_errorbar(aes(x = Month, ymin = lower, ymax = upper), 
                width = 0.1, size = 1) +
  geom_vline(xintercept = 4.5, size = 1) +
  geom_vline(xintercept = 10.5, size = 1) +
  scale_color_manual(values = c("#6971c9",
                                "#af953c",
                                "#a24f99",
                                "#56ae6c",
                                "#ba4a4f")) + 
  labs(y = "Daily Detection Count\n(Outside MPAs)", x = "") +
  annotate("text", x = 0.75, y = 1150, label = "(b)") +
  ylim(c(0, 1150)) +
  theme_classic() +
  theme(legend.position = "none")


png(file="Figures/3panel.png",
    width = 2500,
    height = 2200,
    res = 300)

p_in / p_out / p

dev.off()

 

