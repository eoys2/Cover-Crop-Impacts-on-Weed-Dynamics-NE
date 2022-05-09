# Soil Temperature (degrees celsius)

# LOAD LIBRARIES ---------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(patchwork)
library(gridExtra)

# IMPORT COMBINED DATA ---------------------------------------------------------
combined.data2 <- read.csv(file = here("data","FINAL_DATA", "multi_combined.csv")) %>% 
  filter(Treatment != "VetchDNI") #do not use vetch data from ENREC
summary(combined.data2)

#apply factor levels to treatments for graphs
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))

# ON FARM ----------------------------------------------------------------------

# FIT LMM FOR ON FARM 
temp.onfarm.lmer <- lmer(avgTemperature ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                             data = combined.data2 %>% filter(FieldType == "On Farm"))



# LOOK AT LMM OUTPUT
summary(temp.onfarm.lmer)
car::Anova(temp.onfarm.lmer, type = "2")
plot(temp.onfarm.lmer)  

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
temp.onfarm.emmeans <- emmeans(temp.onfarm.lmer, ~ Treatment | Site:Season, type = "response")
temp.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
temp_onfarm_emmeansplot <- temp.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Temperature") +
  scale_fill_manual(values = c("darkgray", "green4"))
temp_onfarm_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
temp.onfarm.trtdiff <- pairs(temp.onfarm.emmeans, reverse = T)
temp.onfarm.trtdiff %>% as_tibble()



# RESEARCH STATION -------------------------------------------------------------

# FIT GLMM 
temp.research.glmer <- lmer(avgTemperature ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                            data = combined.data2 %>% filter(FieldType == "Research Station"))


# LOOK AT GLMM OUTPUT
summary(temp.research.glmer)
car::Anova(temp.research.glmer, type = "2")
plot(temp.research.glmer) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
temp.research.emmeans <- emmeans(temp.research.glmer, ~ Treatment | Site:Season, type = "response")
temp.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
temp_research_emmeansplot <- temp.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Temperature") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
temp_research_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
temp.research.trtdiff <- pairs(temp.research.emmeans, reverse = T)
temp.research.trtdiff %>% as_tibble()





# GGPLOTTING FIGURES (used tables for this data though) --------------------------------------------------------------

#On farm
temp_onfarm_emmeansplot <- temp.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(strip.text.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,40),"Temperature") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
temp_onfarm_emmeansplot

#research station
temp_research_emmeansplot <- temp.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,40),"Temperature") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
temp_research_emmeansplot

#Put them together
temp_research_emmeansplot+temp_onfarm_emmeansplot+
  plot_layout(ncol = 1, heights = c(2,4))+
  plot_annotation(title = "Soil Temperature by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0)))-> temp
temp

temperature <- patchwork::patchworkGrob(temp)
gridExtra::grid.arrange(temperature, left = textGrob(expression(paste("Soil Temperature  (°C)")), rot = 90))

