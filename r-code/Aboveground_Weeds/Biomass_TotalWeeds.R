#TOTAL WEED BIOMASS

# LOAD LIBRARIES ---------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(ggplot2)
library(patchwork)
library(gridExtra)

# IMPORT COMBINED DATA ---------------------------------------------------------
combined.data2 <- read.csv(file = here("data","FINAL_DATA", "multi_combined.csv")) %>% 
  filter(Site != "Colfax") %>% #filter out Colfax County because no aboveground weeds found at any time, and the zeros mess up the model
  filter(Treatment != "VetchDNI")
summary(combined.data2)

#Apply factor levels
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))

# ON FARM ----------------------------------------------------------------------

#Log+1 transform the data to normalize the data, account for zeros in the dataset
#Fit linear mixed model
totbiomass.onfarm.lmer <- lmer(log(avgBiomass_Total + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                               data = combined.data2 %>% filter(FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(totbiomass.onfarm.lmer)
car::Anova(totbiomass.onfarm.lmer, type = "2") 
plot(totbiomass.onfarm.lmer) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.onfarm.emmeans <- emmeans(totbiomass.onfarm.lmer, ~ Treatment | Site:Season, type = "response")
totbiomass.onfarm.emmeans %>% 
  as_tibble() %>% 
  filter(Site != "Colfax")

# PLOT ESTIMATED TREATMENT MEANS
totbiomass.onfarm.plot <- totbiomass.onfarm.emmeans %>% 
  as_tibble() %>%
  filter(Site != "Colfax") %>% 
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  #mutate(lower.CL = ifelse(lower.CL < 0, 0, lower.CL)) %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  #ggtitle("On-Farm Total Weed Biomass")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  xlab("On Farm Treatments")+ 
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Total Weed Biomass g m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totbiomass.onfarm.plot

# COMPARE TREATMENTS WITHIN EACH SITE
totbiomass.onfarm.trtdiff <- pairs(totbiomass.onfarm.emmeans, reverse = F)
totbiomass.onfarm.trtdiff %>% as_tibble()

# RESEARCH STATION -------------------------------------------------------------

#FIT LINEAR MIXED MODEL
#Log+1 transform
totbiomass.research.lmer <- lmer(log(avgBiomass_Total + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                 data = combined.data2 %>% filter(FieldType == "Research Station"))

# LOOK AT GLMM OUTPUT
summary(totbiomass.research.lmer)
car::Anova(totbiomass.research.lmer, type = "2")
plot(totbiomass.research.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.research.emmeans <- emmeans(totbiomass.research.lmer, ~ Treatment | Site:Season, type = "response")
totbiomass.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totbiomass_research_emmeansplot <- totbiomass.research.emmeans %>% 
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  #mutate(lower.CL = ifelse(lower.CL < 0, 0, lower.CL)) %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  xlab("Research Station Treatments")+
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  #geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Total Weed Biomass g m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totbiomass_research_emmeansplot

totbiomass.research.trtdiff <- pairs(totbiomass.research.emmeans, reverse = F)
totbiomass.research.trtdiff %>% as_tibble()


# GGPLOTTING FIGURES --------------------------------------------------------------

#On-farm
totbiomass.onfarm.plot <- totbiomass.onfarm.emmeans %>% 
  as_tibble() %>%
  filter(Site != "Colfax") %>% 
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  #mutate(lower.CL = ifelse(lower.CL < 0, 0, lower.CL)) %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  #ggtitle("On-Farm Total Weed Biomass")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  xlab("On Farm Treatments")+ 
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
  scale_y_continuous(limits = c(0,11),expression(paste("Total Weed Biomass g m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totbiomass.onfarm.plot


#research stations
totbiomass_research_emmeansplot <- totbiomass.research.emmeans %>% 
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  #mutate(lower.CL = ifelse(lower.CL < 0, 0, lower.CL)) %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  xlab("Research Station Treatments")+
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  #geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,11),expression(paste("Total Weed Biomass g m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totbiomass_research_emmeansplot


totbiomass_research_emmeansplot+totbiomass.onfarm.plot+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Weed Biomass by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold")))-> bio
bio

biomass <- patchwork::patchworkGrob(bio)
gridExtra::grid.arrange(biomass, left = textGrob(expression(paste("Estimated Total Weed Biomass grams DM m"^"-2")), rot = 90)) -> densplot


