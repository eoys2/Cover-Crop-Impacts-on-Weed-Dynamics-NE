# Shannon Hill Tests
# Derived from Virginia Nichol's and Lydia English's code (Iowa State University)

#FOR SEEDBANK DATA ONLY (note: seedbank data is not converted into seeds/ha in this dataset. ShanHill Results will still be the same.)

#NOTE: PRESS LINE OF CODE OF INTEREST TO EXPAND

# Libraries ---------------------------------------------------------------
library(lme4)
library(lmerTest) 
library(vegan)
library(emmeans)
library(broom)
library(dplyr)
library(ggplot2)
library(here)
library(tidyverse)
library(lattice)
library(here)
library(writexl)


# Upload data -------------------------------------------------------------

seedbank <- read.csv(file = here("data","FINAL_DATA", "updatedseedbank-data-sum.csv"))
summary(seedbank)

# Data Management ---------------------------------------------------------
newdata <-
  seedbank %>% 
  group_by(Site, Treatment, Rep) %>% 
  summarize_at(vars(AMATU:SIDSP), ~sum(., na.rm = TRUE)) %>%   #select all of the species.
  tidyr::unite("overview", Site, Treatment, Rep, remove = FALSE) #%>%  #can make a new column that summarizes the three components
  #ungroup() #if you are going to do more.



# Calculating the indices ------------------------------------------------

rich <- function(x){rowSums(ifelse(x > 0, 1, 0))} 

tests <-
  newdata %>% 
  mutate(shan_div = diversity(.[,5:62]), #select all species in dataset. functions are from 'vegan'
         shan_hill = exp(shan_div),
         richness = rich(.[,5:62]),
         evenness = shan_div/log(richness)) %>% 
  select(c(overview, Treatment, Site, Rep,shan_div, shan_hill, richness, evenness))

#this has already been done, use the file specified below in the next section
#write_xlsx(tests,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\ShanHill_TestsEO.xlsx")
#Because the fixed effects model uses contrasts, I manually saved onfarm and research station sites separately using excel from the write_xlsx above, which is why you don't see the 2 files I sent you here. 

newtests <- newdata %>% 
  mutate(shan_div = (diversity(newdata[5:62], "shannon")))
  
  diversity(newdata[5:62], "shannon")

# ---- Bring in datasets made ----

#Bring in the datasets

onfarm_tests <- read.csv(file = here("data","FINAL_DATA", "ShanHill_OnFarm.csv"))
summary(onfarm_tests)

rs_tests <- read.csv(file = here("data","FINAL_DATA", "ShanHill_ResearchStation.csv"))
summary(rs_tests)


# Diversity models ------------------------------------------------------

#LETS RUN SOME STATS
# i. diversity
# fixed effects model 

#Apply factor levels
onfarm_tests$Treatment <- factor(onfarm_tests$Treatment, levels = c("Check","Cover","Rye","Vetch","VetchDNI"))
onfarm_tests$Site <- factor(onfarm_tests$Site, levels = c("Colfax","Greeley","Howard","Merrick","SCAL","ENREC"))

#ON FARM DATA
div_lmer_farm <- lmer(shan_hill ~ Site*Treatment + (1|Rep), data = onfarm_tests) 
anova(div_lmer_farm)
div_stats_farm <- emmeans(div_lmer_farm, specs = pairwise ~ Treatment|Site)$contrasts %>% 
  tidy() %>% 
  mutate(metric = "shan_hill")
div_stats_farm

#Basic graph showing Shannon Hill Values
onfarm_tests %>%
  ggplot(aes(Site, shan_hill)) +
  geom_boxplot(aes(fill = Treatment))



#RESEARCH STATION DATA
rs_tests$Treatment <- factor(rs_tests$Treatment, levels = c("Check","Cover","Rye","Vetch","VetchDNI"))
rs_tests$Site <- factor(rs_tests$Site, levels = c("Colfax","Greeley","Howard","Merrick","SCAL","ENREC"))

#remember to ignore vetch @ ENREC
div_lmer_rs <- lmer(shan_hill ~ Site*Treatment + (1|Rep), data = rs_tests) 
anova(div_lmer_rs)
div_stats_rs <- emmeans(div_lmer_rs, specs = pairwise ~ Treatment|Site)$contrasts %>% 
  tidy() %>% 
  mutate(metric = "shan_hill")
div_stats_rs

#Basic graph showing Shannon Hill Values
rs_tests %>%
  ggplot(aes(Site, shan_hill)) +
  geom_boxplot(aes(fill = Treatment))


# Richness models ---------------------------------------------------------


# ii. richness

#ON FARM RICHNESS TESTS
farmrich <- lmer(richness ~Site*Treatment + (1|Rep), data = onfarm_tests)
anova(farmrich)
emmeans(farmrich, specs = pairwise ~ Treatment|Site)$contrasts %>% 
  tidy() %>% 
  mutate(metric = "richness")


#Plot richness - On Farm
onfarm_tests %>%
  ggplot(aes(Site, richness)) +
  geom_boxplot(aes(fill = Treatment))


#RESEARCH STATION RICHNESS TESTS
rsrich <- lmer(log(richness) ~Site*Treatment + (1|Rep), data = rs_tests)
anova(rsrich)
emmeans(rsrich, specs = pairwise ~ Treatment|Site)$contrasts %>% 
  tidy() %>% 
  mutate(metric = "richness")


#Plot richness - On Farm
rs_tests %>%
  ggplot(aes(Site, richness)) +
  geom_boxplot(aes(fill = Treatment))



# Evenness Models ----------------------------------------------------------------
# iii. evenness


#ON FARM
efarm <- lmer(evenness ~ Site*Treatment + (1|Rep), data = onfarm_tests)
anova(efarm)
emmeans(efarm, pairwise ~ Treatment|Site, type = "response")
efarm_stats <- emmeans(efarm, specs = pairwise ~ Treatment|Site)$contrasts %>% 
  tidy() %>% 
  mutate(metric = "evenness")
efarm_stats

#Plot evenness
onfarm_tests %>% 
  ggplot(aes(Site, evenness))+
  geom_boxplot(aes(fill = Treatment))+
  scale_fill_manual(values = c("darkgray","darkolivegreen3"))


#Research Stations
ers <- lmer(evenness ~ Site*Treatment + (1|Rep), data = rs_tests)
anova(ers)
emmeans(ers, pairwise ~ Treatment|Site, type = "response")
ers_stats <- emmeans(ers, specs = pairwise ~ Treatment|Site)$contrasts %>% 
  tidy() %>% 
  mutate(metric = "evenness")
ers_stats

#Plot evenness
rs_tests %>% 
  ggplot(aes(Site, evenness))+
  geom_boxplot(aes(fill = Treatment))


#___________________________________________________________________________________________


#if you want to save results.
#stats_contrasts <- div1_stats %>% bind_rows(rich1_stats) %>% bind_rows(even1_stats)


#stats_contrasts %>% write_csv("01_stats-uni/st_diversity-contrasts.csv")