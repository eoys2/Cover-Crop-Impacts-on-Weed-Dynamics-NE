#GRASS BIOMASS

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
  filter(Site != "Colfax") %>% #filter out Colfax County because no aboveground weeds found at any time, and the zeros mess up the model
  filter(Treatment != "VetchDNI")
summary(combined.data2)

#apply factor levels
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))

# ON FARM ----------------------------------------------------------------------

#Fit LMM
grassbio.onfarm.lmer <- lmer(log(avgBiomass_Grass + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
     data = combined.data2 %>% filter(FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(grassbio.onfarm.lmer)
car::Anova(grassbio.onfarm.lmer, type = "2")
plot(grassbio.onfarm.lmer)  

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
grassbio.onfarm.emmeans <- emmeans(grassbio.onfarm.lmer, ~ Treatment | Site:Season, type = "response")
grassbio.onfarm.emmeans %>% 
  as_tibble() %>% 
  filter(Site != "Colfax") %>% 
  filter(response > 0.000001) -> grass
grass

# PLOT ESTIMATED TREATMENT MEANS
grassbio_onfarm_emmeansplot <- grass %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#9d0208","black"))
grassbio_onfarm_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
grassbio.onfarm.trtdiff <- pairs(grassbio.onfarm.emmeans, reverse = T)
grassbio.onfarm.trtdiff %>% as_tibble()




# RESEARCH STATION -------------------------------------------------------------

#Fit LMM
grassbio.research.lmer<-lmer(log(avgBiomass_Grass + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
     data = combined.data2 %>% filter(FieldType == "Research Station"))

# LOOK AT GLMM OUTPUT
summary(grassbio.research.lmer)
car::Anova(grassbio.research.lmer, type = "2")
plot(grassbio.research.lmer) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
grassbio.research.emmeans <- emmeans(grassbio.research.lmer, ~ Treatment | Site:Season, type = "response")
grassbio.research.emmeans %>% 
  as_tibble() %>% 
  filter(response > 0.0000000001) %>% 
  drop_na -> rsgrass

# PLOT ESTIMATED TREATMENT MEANS
grassbio_research_emmeansplot <- grassbio.research.emmeans %>%
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
grassbio_research_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
grassbio.research.trtdiff <- pairs(grassbio.research.emmeans, reverse = F)
grassbio.research.trtdiff %>% as_tibble()



# GGPLOTTING FIGURES --------------------------------------------------------------

#on farm
grassbio_onfarm_emmeansplot <- grass %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(strip.text.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,0.5),"Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
grassbio_onfarm_emmeansplot

#research station
grassbio_research_emmeansplot <- rsgrass %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,0.5),"Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
grassbio_research_emmeansplot

#put them together
grassbio_research_emmeansplot+grassbio_onfarm_emmeansplot+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Grass Biomass by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold")))-> gbio
gbio

gbiomass <- patchwork::patchworkGrob(gbio)
gridExtra::grid.arrange(gbiomass, left = textGrob(expression(paste("Estimated Grass Biomass grams DM m"^"-2")), rot = 90)) 
