# TOTAL WEED SEEDS 
# This file analyzes the total number of weed seeds in the seedbank at each respective site (total seedbank density)
# Combined figures of on-farm and research stations are included at the bottom of this file. 

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
library(grid)

# IMPORT COMBINED DATA ---------------------------------------------------------

combined.data2 <- read.csv(file = here("data","FINAL_DATA", "multi_combined.csv")) %>% 
  filter(Treatment != "VetchDNI") #do not use vetch data from ENREC, this filters it out
summary(combined.data2)

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))

#NOTE: due to spatial, treatment, and experimental design differences, on-farm and research station location models are run independenly of each other

# ON FARM ----------------------------------------------------------------------

#glmer with nb
totseeds_weeds.onfarm.glmer <- glmer.nb(totSeeds_Weeds ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                        data = combined.data2 %>% filter(Season == "Early", FieldType == "On Farm"))
#Ignore 'season' filter, this does not mean anything to this analysis. It was added during the main data management period because weed seedbank and aboveground weed data are in one file together, so a specification was added at that time.

# LOOK AT LMM OUTPUT
summary(totseeds_weeds.onfarm.glmer)
car::Anova(totseeds_weeds.onfarm.glmer, type = "2") #use type II SS
plot(totseeds_weeds.onfarm.glmer)


# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totseeds_weeds.onfarm.emmeans <- emmeans(totseeds_weeds.onfarm.glmer, ~ Treatment | Site, type = "response") 
totseeds_weeds.onfarm.emmeans %>% as_tibble()


# PLOT ESTIMATED TREATMENT MEANS (converted to weeds/m2 estimations)
totseeds_weeds_onfarm_emmeansplot <- totseeds_weeds.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  #scale_y_continuous("Total Weed Seeds") +
  scale_y_continuous(limits = c(0,6000),expression(paste("Estimated Total Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totseeds_weeds_onfarm_emmeansplot

#Save as you desire
# ggsave(totseeds_weeds_onfarm_emmeansplot, file = here("results", "totseeds_weeds_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
#will compute P value
totseeds_weeds.onfarm.trtdiff <- pairs(totseeds_weeds.onfarm.emmeans, reverse = T)
totseeds_weeds.onfarm.trtdiff %>% 
  as_tibble()


# RESEARCH STATION -------------------------------------------------------------

totseeds_weeds.research.glmer <- glmer.nb(totSeeds_Weeds ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                          data = combined.data2 %>% filter(Season == "Early", FieldType == "Research Station"))


# LOOK AT LMM OUTPUT
summary(totseeds_weeds.research.glmer)
car::Anova(totseeds_weeds.research.glmer, type = "II")
plot(totseeds_weeds.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totseeds_weeds.research.emmeans <- emmeans(totseeds_weeds.research.glmer, ~ Treatment | Site, type = "response")
totseeds_weeds.research.emmeans %>% 
  as_tibble() %>% 
  drop_na


# PLOT ESTIMATED TREATMENT MEANS
# Note from materials and methods: Research stations utilized a JMC probe, therefore conversions of seeds per square meter will be different due to sampling device diameter. 

totseeds_weeds_research_emmeansplot <- totseeds_weeds.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,6000),expression(paste("Estimated Total Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totseeds_weeds_research_emmeansplot

# ggsave(totseeds_weeds_research_emmeansplot, file = here("results", "totseeds_weeds_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totseeds_weeds.research.trtdiff <- pairs(totseeds_weeds.research.emmeans, reverse = T)
totseeds_weeds.research.trtdiff %>% 
  as_tibble() %>% 
  drop_na()






# GGPLOT: Combine the on-farm and research station plots together ----------------------------------------------
totseeds_weeds_onfarm_emmeansplot <- totseeds_weeds.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw()+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  #theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  #theme(aspect.ratio = 1) +
  #scale_y_continuous("Total Weed Seeds") +
  scale_y_continuous(limits = c(0,6000),expression(paste("Estimated Total Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totseeds_weeds_onfarm_emmeansplot


# PLOT ESTIMATED TREATMENT MEANS
totseeds_weeds_research_emmeansplot <- totseeds_weeds.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  #theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(0,6000),expression(paste("Estimated Total Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totseeds_weeds_research_emmeansplot


totseeds_weeds_research_emmeansplot+totseeds_weeds_onfarm_emmeansplot+  plot_layout(ncol = 1, widths = c(1,1), heights = c(2,4))+
  plot_annotation(title = "Estimated Total Seedbank Density", theme = theme(plot.title = element_text(size = 18, hjust = 0.2)))-> newplot
newplot

gt <- patchwork::patchworkGrob(newplot)

gridExtra::grid.arrange(gt, left = textGrob(expression(paste("Estimated total seedbank density in weeds seeds m"^"-2")), rot = 90)) -> totalseedbank

#Save as desired
#Saved: 3/11/22
#ggsave(totalseedbank, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\totalseedbank.png", dpi = 600, width = 6, height = 6)




