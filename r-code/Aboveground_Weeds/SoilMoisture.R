# Soil moisture (in units of volumentric water content)


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
  filter(Treatment != "VetchDNI") #do not use vetch data from ENREC (this filters that out)
summary(combined.data2)

#apply factor levels to treatments for graphs
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))

# ON FARM ---------------------------------------------------------------------

# FIT LMM FOR ON FARM 
moist.onfarm.lmer <- lmer(avgMoisture ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                          data = combined.data2 %>% filter(FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(moist.onfarm.lmer)
car::Anova(moist.onfarm.lmer, type = "2")
plot(moist.onfarm.lmer)  

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
moist.onfarm.emmeans <- emmeans(moist.onfarm.lmer, ~ Treatment | Site:Season, type = "response")
moist.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
moist_onfarm_emmeansplot <- moist.onfarm.emmeans %>% 
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
  scale_y_continuous("Moisture") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
moist_onfarm_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
moist.onfarm.trtdiff <- pairs(moist.onfarm.emmeans, reverse = T)
moist.onfarm.trtdiff %>% as_tibble()



# RESEARCH STATION -------------------------------------------------------------

# FIT GLMM 
moist.research.glmer <- lmer(avgMoisture ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                             data = combined.data2 %>% filter(FieldType == "Research Station"))


# LOOK AT GLMM OUTPUT
summary(moist.research.glmer)
car::Anova(moist.research.glmer, type = "2")
plot(moist.research.glmer) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
moist.research.emmeans <- emmeans(moist.research.glmer, ~ Treatment | Site:Season, type = "response")
moist.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
moist_research_emmeansplot <- moist.research.emmeans %>% 
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
  scale_y_continuous("Moisture") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
moist_research_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
moist.research.trtdiff <- pairs(moist.research.emmeans, reverse = T)
moist.research.trtdiff %>% as_tibble()





# GGPLOTTING FIGURES (ended up using tables for this data though) --------------------------------------------------------------

#ON FARM
moist_onfarm_emmeansplot <- moist.onfarm.emmeans %>% 
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
  scale_y_continuous(limits = c(0,50),"Moisture") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
moist_onfarm_emmeansplot

#research stations
moist_research_emmeansplot <- moist.research.emmeans %>% 
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
  scale_y_continuous(limits = c(0,50),"Moisture") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
moist_research_emmeansplot

#put them together
moist_research_emmeansplot+moist_onfarm_emmeansplot+
  plot_layout(ncol = 1, heights = c(2,4))+
  plot_annotation(title = "Soil Moisture by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0)))-> moist
moist

moisture <- patchwork::patchworkGrob(moist)
gridExtra::grid.arrange(moisture, left = textGrob(expression(paste("Soil Moisture  (% VWC)")), rot = 90)) ->moistplot

#ggsave(moistplot, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\soilmoisture.png", dpi = 600, width = 6, height = 6)


