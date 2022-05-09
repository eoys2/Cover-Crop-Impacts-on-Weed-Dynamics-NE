# Grass DENSITY 

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

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


# ON FARM GRASS DENSITY ----------------------------------------------------------

# FIT LINEAR MIXED MODEL
grassdensity.onfarm.lmer.log <- lmer(log(sumGrass_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                    data = combined.data2 %>% filter(FieldType == "On Farm"))

# LOOK AT LMM OUTPUT for log data
summary(grassdensity.onfarm.lmer.log)
car::Anova(weeddensity.onfarm.lmer.log, type = "2")
plot(grassdensity.onfarm.lmer.log) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
grassdensity.onfarm.emmeans.log <- emmeans(grassdensity.onfarm.lmer.log, ~ Treatment | Site:Season, type = "response")
grassdensity.onfarm.emmeans.log %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
grassdensity_onfarm_emmeansplot.log <- grassdensity.onfarm.emmeans.log %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Emerged Grass Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
grassdensity_onfarm_emmeansplot.log

# COMPARE TREATMENTS WITHIN EACH SITE
grassdensity.onfarm.trtdiff.log <- pairs(grassdensity.onfarm.emmeans.log, reverse = F, type = "response")
grassdensity.onfarm.trtdiff.log %>% as_tibble()


# RESEARCH STATION GRASS DENSITY -------------------------------------------------------------

grassdensity.rs.lmer.log <- lmer(log(sumGrass_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                data = combined.data2 %>% filter(FieldType == "Research Station"))


# LOOK AT LMM OUTPUT
summary(grassdensity.rs.lmer.log)
car::Anova(weeddensity.onfarm.lmer.log, type = "2")
plot(grassdensity.rs.lmer.log)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
grassdensity.research.emmeans <- emmeans(grassdensity.rs.lmer.log, ~ Treatment | Site:Season, type = "response")
grass <- grassdensity.research.emmeans %>% 
  as_tibble() %>% 
  filter(response > 0.0001)
grass #Filtering out very small values for average number of grasses (there are 0 in the actual dataset at some sites, but the model leads to a very, very small value (i.e. we cant have 0.001 of a grass))

# PLOT ESTIMATED TREATMENT MEANS
grassdensity_research_emmeansplot <- grass %>% 
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Emerged Grass Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
grassdensity_research_emmeansplot
# ggsave(grassdensity_research_emmeansplot, file = here("results", "grassdensity_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
grassdensity.research.trtdiff <- pairs(grassdensity.research.emmeans, reverse = F, type = "response")
grassdensity.research.trtdiff %>% as_tibble() %>% arrange(Site, Season)




# GGPLOTTING FIGURES --------------------------------------------------------------

#on farm locations
grassdensity_onfarm_emmeansplot.log <- grassdensity.onfarm.emmeans.log %>%
  as_tibble() %>%
  filter(Site != "Colfax") %>% 
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
  scale_y_continuous(expression(paste("Emerged Grass Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
grassdensity_onfarm_emmeansplot.log


grassdensity_research_emmeansplot <- grass %>% 
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
  scale_y_continuous(limits = c(0,60),expression(paste("Emerged Grass Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
grassdensity_research_emmeansplot


grassdensity_research_emmeansplot+grassdensity_onfarm_emmeansplot.log+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Grass Density by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold")))-> dens
dens

density <- patchwork::patchworkGrob(dens)
gridExtra::grid.arrange(density, left = textGrob(expression(paste("Estimated Total Grass Density weeds m"^"-2")), rot = 90)) -> densplot


#Save as desired (change to your own pathway)
#Save 3/11/22
#ggsave(densplot, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\grassdensity.pdf", dpi = 600, width = 6, height = 6)
