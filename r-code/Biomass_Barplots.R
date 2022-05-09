#2/3
##Bar graphs showing biomass percent reduction. 


library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(ggplot2)
        
biodata <- read.csv(file = here("data", "TotalBiomassReductions.csv"))

earlybio <- biodata %>% 
  filter(Stat == "EarlyBio") %>% 
  arrange(desc(perc_red))

latebio <- biodata %>% 
  filter(Stat == "LateBio") %>% 
  arrange(desc(perc_red))


earlybiomass <- ggplot(data = earlybio, aes(x = Site, y = perc_red))+
  geom_bar(stat = "identity", fill = "firebrick4")+
  xlab("Site")+
  ylab("Cover Crop - Early Weed Biomass % Reduction")+
  theme_minimal()
  
latebiomass <- ggplot(data = latebio, aes(x = Site, y = perc_red))+
  geom_bar(stat = "identity", fill = "firebrick4")+
  xlab("Site")+
  ylab("Cover Crop - Late Weed Biomass % Reduction")+
  theme_minimal()




combined.data <- read.csv(file = here("data", "updated-combined-dataEO.csv")) %>% 
  filter(Treatment != "VetchDNI")
#summary(combined.data)


