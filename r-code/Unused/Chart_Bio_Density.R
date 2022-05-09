#Dual y axis, stacked bar chart


# Libraries ---------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(readr)     
library(tidyverse) 
library(here)      
library(lme4)      
library(lmerTest)  
library(emmeans)   



# Get weed biomass data in ------------------------------------------------

combined.data <- read.csv(file = here("data", "updated-combined-dataEO.csv")) %>% 
  filter(Site != "Colfax")
summary(combined.data)

#EARLY

# FIT LMM FOR ON FARM 
totbiomass.onfarm.lmer <- lmer(avgBiomass_Total ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                               data = combined.data %>% filter(Season == "Early", FieldType == "On Farm", Site != "Colfax")) #change season depending on what you want to look at

# LOOK AT LMM OUTPUT
summary(totbiomass.onfarm.lmer)
anova(totbiomass.onfarm.lmer)
plot(totbiomass.onfarm.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.onfarm.emmeans <- emmeans(totbiomass.onfarm.lmer, ~ Treatment | Site)
totbiomass.onfarm.emmeans %>% as_tibble()

write.csv(totbiomass.onfarm.emmeans,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\emmeansEarlyBio.csv")


# PLOT ESTIMATED TREATMENT MEANS
totbiomass_onfarm_emmeansplot <- totbiomass.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbiomass_onfarm_emmeansplot
# ggsave(totbiomass_onfarm_emmeansplot, file = here("results", "totbiomass_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbiomass.onfarm.trtdiff <- pairs(totbiomass.onfarm.emmeans, reverse = T)
totbiomass.onfarm.trtdiff %>% as_tibble()








#LATE

# FIT LMM FOR ON FARM 
totbiomass.onfarm.lmer.late <- lmer(avgBiomass_Total ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data %>% filter(Season == "Late", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(totbiomass.onfarm.lmer.late)
anova(totbiomass.onfarm.lmer.late)
plot(totbiomass.onfarm.lmer.late)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.onfarm.emmeans.late <- emmeans(totbiomass.onfarm.lmer.late, ~ Treatment | Site)
totbiomass.onfarm.emmeans.late %>% as_tibble()

write.csv(totbiomass.onfarm.emmeans.late,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\emmeansLateBio.csv")



# PLOT ESTIMATED TREATMENT MEANS
totbiomass_onfarm_emmeansplot <- totbiomass.onfarm.emmeans.late %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbiomass_onfarm_emmeansplot
# ggsave(totbiomass_onfarm_emmeansplot, file = here("results", "totbiomass_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbiomass.onfarm.trtdiff <- pairs(totbiomass.onfarm.emmeans.late, reverse = T)
totbiomass.onfarm.trtdiff %>% as_tibble()



#research station

# FIT GLMM (gamma)
totbiomass.research.glmer <- glmer(avgBiomass_Total ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
                                   data = combined.data %>% filter(FieldType == "Research Station"),
                                   family = "Gamma")


# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.research.emmeans <- emmeans(totbiomass.research.glmer, ~ Treatment | Site:Season, type = "response")
totbiomass.research.emmeans %>% as_tibble()


write.csv(totbiomass.research.emmeans,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\emmeansBioRS.csv")


# PLOT ESTIMATED TREATMENT MEANS
totbiomass_research_emmeansplot <- totbiomass.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totbiomass_research_emmeansplot
# ggsave(totbiomass_research_emmeansplot, file = here("results", "totbiomass_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbiomass.research.trtdiff <- pairs(totbiomass.research.emmeans, reverse = T)
totbiomass.research.trtdiff %>% as_tibble()





#save emmeans values in separate csv


# Get weed density data in ------------------------------------------------

weeddensity.onfarm.lmer.log <- lmer(log(sumWeed_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),data = combined.data %>% filter(FieldType == "On Farm"))


# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
weeddensity.onfarm.emmeans.log <- emmeans(weeddensity.onfarm.lmer.log, ~ Treatment | Site:Season, type = "response")
weeddensity.onfarm.emmeans.log %>% as_tibble()


write.csv(weeddensity.onfarm.emmeans.log,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\emmeansOFDensity.csv")



# PLOT ESTIMATED TREATMENT MEANS
weeddensity_onfarm_emmeansplot.log <- weeddensity.onfarm.emmeans.log %>%
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_grid(Site~Season, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Weed Density") +
  scale_fill_manual(values = c("darkgray", "firebrick4"))
weeddensity_onfarm_emmeansplot.log
ggsave(weeddensity_onfarm_emmeansplot.log, file = here("results", "weeddensity_onfarm_emmeansplot_log.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
weeddensity.onfarm.trtdiff.log <- pairs(weeddensity.onfarm.emmeans.log, reverse = F, type = "response")
weeddensity.onfarm.trtdiff.log %>% as_tibble()



write.csv(weeddensity.onfarm.emmeans.log,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\emmeansOFDensity.csv")



# Research stations
weeddensity.rs.lmer.log <- lmer(log(sumWeed_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                data = combined.data %>% filter(FieldType == "Research Station"))



# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
weeddensity.research.emmeans <- emmeans(weeddensity.rs.lmer.log, ~ Treatment | Site:Season, type = "response")
weeddensity.research.emmeans %>% as_tibble()



write.csv(weeddensity.research.emmeans,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\emmeansRSdensity.csv")



# PLOT ESTIMATED TREATMENT MEANS
weeddensity_research_emmeansplot <- weeddensity.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*100, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL*100, ymax = asymp.UCL*100), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Weed Density") +
  scale_fill_manual(values = c("darkgray", "firebrick4", "red3"))
weeddensity_research_emmeansplot
# ggsave(weeddensity_research_emmeansplot, file = here("results", "weeddensity_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
weeddensity.research.trtdiff <- pairs(weeddensity.research.emmeans, reverse = T)
weeddensity.research.trtdiff %>% as_tibble() %>% arrange(Site, Season)










# Creating the figure ----------------------------------------------------

#bring in the data
denbio <- read.csv(file = here("data", "emmeansDensityBio.csv")) 
summary(denbio)


den <- denbio %>% 
  group_by(Site) %>% 
  filter(type == "Density")

bio <- denbio %>% 
  group_by(Site) %>% 
  filter(type == "Biomass")



# sec_axis_data <- mpg %>%
#   group_by(manufacturer) %>%
#   summarise(entries = n())

dengraph <- den %>% 
  filter(season == "Early") %>% 
  #filter(Site != "Merrick") %>% 
  as_tibble() %>% 
  ggplot(aes(x = Site, y = emmean, fill = Treatment))+
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7)+
  #geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3)+
  #facet_grid(Site~season) +
  scale_y_continuous("Weed Density") +
  #facet_grid(Site~season) +
  theme_bw() +
  scale_fill_manual(values = c("darkgray", "firebrick4", "red3","black"))+
  theme(aspect.ratio = 1)

#need to figure out how to get early and late separate
#need to figure out how to paste the early and late season side by side. 


#error bars with other graphs
weeddensity_research_emmeansplot <- weeddensity.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*100, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL*100, ymax = asymp.UCL*100), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Weed Density") +
  scale_fill_manual(values = c("darkgray", "firebrick4", "red3"))
weeddensity_research_emmeansplot
