#Broadleaf Biomass

# LOAD LIBRARIES ---------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(grid)
library(patchwork)
library(gridExtra)

# IMPORT COMBINED DATA ---------------------------------------------------------
combined.data2 <- read.csv(file = here("data","FINAL_DATA", "multi_combined.csv")) %>% 
  filter(Site != "Colfax") %>% #filter out Colfax County because no aboveground weeds found at any time, and the zeros mess up the model
  filter(Treatment != "VetchDNI")
summary(combined.data2)

#apply factor levels
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))

# ON FARM ---------------------------------------------------------------------

#FIT LMM
broadbio.onfarm.lmer <- lmer(log(avgBiomass_Broadleaf + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                           data = combined.data2 %>% filter(FieldType == "On Farm"))



# LOOK AT LMM OUTPUT
summary(broadbio.onfarm.lmer)
car::Anova(broadbio.onfarm.lmer, type = "2")
plot(broadbio.onfarm.lmer)  

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
broadbio.onfarm.emmeans <- emmeans(broadbio.onfarm.lmer, ~ Treatment | Site:Season, type = "response")
broadbio.onfarm.emmeans %>% 
  as_tibble() %>% 
  filter(Site != "Colfax") %>% 
  filter(response > 0.000000000000000000000001) -> broad #remove any small values of broadleaves induced by model
broad


# PLOT ESTIMATED TREATMENT MEANS
broadbio_onfarm_emmeansplot <- broad %>%
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

# COMPARE TREATMENTS WITHIN EACH SITE
broadbio.onfarm.trtdiff <- pairs(broadbio.onfarm.emmeans, reverse = F)
broadbio.onfarm.trtdiff %>% as_tibble()


# RESEARCH STATION -------------------------------------------------------------

#Fit LMM
broadbio.research.lmer<- lmer(log(avgBiomass_Broadleaf + 0.1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
     data = combined.data2 %>% filter(FieldType == "Research Station"))


# LOOK AT GLMM OUTPUT
summary(broadbio.research.lmer)
car::Anova(broadbio.research.lmer, type = "2")
plot(broadbio.research.lmer) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
broadbio.research.emmeans <- emmeans(broadbio.research.lmer, ~ Treatment | Site:Season, type = "response")
broadbio.research.emmeans %>% 
  as_tibble() %>% 
  drop_na() %>% 
  filter(response > 0.0000000000001) ->rsbroad


# PLOT ESTIMATED TREATMENT MEANS
broadbio_research_emmeansplot <- rsbroad %>% 
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
broadbio_research_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
broadbio.research.trtdiff <- pairs(broadbio.research.emmeans, reverse = F)
broadbio.research.trtdiff %>% as_tibble()






# GGPLOTTING FIGURES--------------------------------------------------------------

#on farm 
broadbio_onfarm_emmeansplot <- broad %>%
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
  scale_y_continuous(limits = c(0,0.7),"Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
grassbio_onfarm_emmeansplot


#research stations
broadbio_research_emmeansplot <- rsbroad %>% 
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
  scale_y_continuous(limits = c(0,0.7),"Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
broadbio_research_emmeansplot

#put them together
broadbio_research_emmeansplot+broadbio_onfarm_emmeansplot+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Broadleaf Biomass by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold")))-> bbio
bbio

bbiomass <- patchwork::patchworkGrob(bbio)
gridExtra::grid.arrange(bbiomass, left = textGrob(expression(paste("Estimated Broadleaf Biomass grams DM m"^"-2")), rot = 90)) 

