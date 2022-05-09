#BROADLEAF DENSITY

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

# FIT LINEAR MIXED MODEL
broaddensity.onfarm.lmer.log <- lmer(log(sumBroadleaf_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                    data = combined.data2 %>% filter(FieldType == "On Farm"))

# LOOK AT LMM OUTPUT for log data
summary(broaddensity.onfarm.lmer.log)
car::Anova(weeddensity.onfarm.lmer.log, type = "2")
plot(broaddensity.onfarm.lmer.log) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
broaddensity.onfarm.emmeans.log <- emmeans(broaddensity.onfarm.lmer.log, ~ Treatment | Site:Season, type = "response")
broad <- broaddensity.onfarm.emmeans.log %>% 
  as_tibble() %>% 
  filter(response > 0.00001)

# PLOT ESTIMATED TREATMENT MEANS
broaddensity_onfarm_emmeansplot.log <- broad %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,100),expression(paste("Emerged Broadleaf Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
broaddensity_onfarm_emmeansplot.log

# COMPARE TREATMENTS WITHIN EACH SITE
broaddensity.onfarm.trtdiff.log <- pairs(broaddensity.onfarm.emmeans.log, reverse = F, type = "response")
broaddensity.onfarm.trtdiff.log %>% as_tibble()


# RESEARCH STATION -------------------------------------------------------------

# FIT LINEAR MIXED MODEL
broaddensity.rs.lmer.log <- lmer(log(sumBroadleaf_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                data = combined.data2 %>% filter(FieldType == "Research Station"))


# LOOK AT LMM OUTPUT
summary(broaddensity.rs.lmer.log)
car::Anova(broaddensity.rs.lmer.log, type = "2")
plot(broaddensity.rs.lmer.log) #looks good

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
broaddensity.research.emmeans <- emmeans(broaddensity.rs.lmer.log, ~ Treatment | Site:Season, type = "response")
broaddensity.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
broaddensity_research_emmeansplot <- broaddensity.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,100), expression(paste("Emerged Broadleaf Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
broaddensity_research_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
broaddensity.research.trtdiff <- pairs(broaddensity.research.emmeans, reverse = F)
broaddensity.research.trtdiff %>% as_tibble() %>% arrange(Site, Season)





# GGPLOTTING FIGURES --------------------------------------------------------------

broaddensity_onfarm_emmeansplot.log <- broad %>%
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
  scale_y_continuous(limits = c(0,100),expression(paste("Emerged Broadleaf Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
broaddensity_onfarm_emmeansplot.log



broaddensity_research_emmeansplot <- broaddensity.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +  
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,100), expression(paste("Emerged Broadleaf Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
broaddensity_research_emmeansplot


broaddensity_research_emmeansplot+broaddensity_onfarm_emmeansplot.log+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Broadleaf Density by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold")))-> broad
broad

broadleaf <- patchwork::patchworkGrob(broad)
gridExtra::grid.arrange(broadleaf, left = textGrob(expression(paste("Estimated Total Broadleaf Density weeds m"^"-2")), rot = 90)) -> densplot

