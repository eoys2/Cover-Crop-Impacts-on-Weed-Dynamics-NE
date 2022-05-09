#PIGWEED BIOMASS

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

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


# ON FARM ---------------------------------------------------------------------

#Fit lmm
pigbio.onfarm.lmer <- lmer(log(avgBiomass_Pigweed + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                               data = combined.data2 %>% filter(FieldType == "On Farm"))


# LOOK AT LMM OUTPUT
summary(pigbio.onfarm.lmer)
car::Anova(pigbio.onfarm.lmer, type = "2")
plot(pigbio.onfarm.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
pigbio.onfarm.emmeans <- emmeans(pigbio.onfarm.lmer, ~ Treatment | Site:Season, type = "response")
pigbio.onfarm.emmeans %>% 
  as_tibble() %>% 
  filter(response > 0.0000001) -> pigbio
pigbio



# PLOT ESTIMATED TREATMENT MEANS
pigbio_onfarm_emmeansplot <- pigbio%>% 
  as_tibble() %>%
  filter(Site != "Colfax") %>% 
  drop_na() %>% 
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Pigweed Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
pigbio_onfarm_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
pigbio.onfarm.trtdiff <- pairs(pigbio.onfarm.emmeans, reverse = F)
pigbio.onfarm.trtdiff %>% as_tibble()





# RESEARCH STATION -------------------------------------------------------------

#Fit lmm
pigbio.research.lmer <- lmer(log(avgBiomass_Pigweed + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                           data = combined.data2 %>% filter(FieldType == "Research Station"))

# LOOK AT GLMM OUTPUT
summary(pigbio.research.lmer)
car::Anova(pigbio.research.lmer, type ="2")
plot(pigbio.research.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
pigbio.research.emmeans <- emmeans(pigbio.research.lmer, ~ Treatment | Site:Season, type = "response")
pigbio.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
pigbio_research_emmeansplot <- pigbio.research.emmeans %>% 
  as_tibble() %>%
  drop_na() %>% 
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
pigbio_research_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
pigbio.research.trtdiff <- pairs(pigbio.research.emmeans, reverse = F)
pigbio.research.trtdiff %>% as_tibble()






# GGPLOTTING FIGURES --------------------------------------------------------------

#On farm
pigbio_onfarm_emmeansplot <- pigbio%>% 
  as_tibble() %>%
  filter(Site != "Colfax") %>% 
  drop_na() %>% 
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
  scale_y_continuous(limits = c(0,11),breaks = seq(0, 10, by = 2),"Total Pigweed Biomass") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
pigbio_onfarm_emmeansplot

#research station
pigbio_research_emmeansplot <- pigbio.research.emmeans %>% 
  as_tibble() %>%
  drop_na() %>% 
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
  scale_y_continuous(limits = c(0,11),breaks = seq(0, 10, by = 2), "Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
pigbio_research_emmeansplot

#put both together
pigbio_research_emmeansplot+pigbio_onfarm_emmeansplot+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Pigweed Biomass by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold")))-> pbio
pbio

pbiomass <- patchwork::patchworkGrob(pbio)
gridExtra::grid.arrange(pbiomass, left = textGrob(expression(paste("Estimated Pigweed Biomass grams DM m"^"-2")), rot = 90)) 





