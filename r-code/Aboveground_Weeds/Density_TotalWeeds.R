# WEED DENSITY (EMERGED WEEDS)
# Aboveground data
#NOTE: click on script number to expand code rows (rows with horizontal arrows are expandable)


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

#apply factor levels to treatments for graphs
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


# ON FARM ----------------------------------------------------------------------
# FIT LINEAR MIXED MODEL

# We want to run the log transformation with a normal distribution (aka lmer now)
# log(weed density + 1) = trt + site + season + trtxsite + trtxseason + seasonxsite + trtxseasonxsite + random repxsite + random trtxrepxsite
# By doing log(sumWeed_Density+1) in the lmer code, we can transform back to the "data scale" much easier in the emmeans function below.

weeddensity.onfarm.lmer.log <- lmer(log(sumWeed_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                         data = combined.data2 %>% filter(FieldType == "On Farm"))


# LOOK AT LMM OUTPUT for log data
summary(weeddensity.onfarm.lmer.log)
car::Anova(weeddensity.onfarm.lmer.log, type = "2")
plot(weeddensity.onfarm.lmer.log) 


# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
weeddensity.onfarm.emmeans.log <- emmeans(weeddensity.onfarm.lmer.log, ~ Treatment | Site:Season, type = "response")
weeddensity.onfarm.emmeans.log %>% 
  as_tibble() %>% 
  filter(Site != "Colfax")


# PLOT ESTIMATED TREATMENT MEANS
weeddensity_onfarm_emmeansplot.log <- weeddensity.onfarm.emmeans.log %>%
  as_tibble() %>%
  filter(Site != "Colfax") %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  #labs(title = "Total Weed Density", x = "On Farm Treatments")+
  xlab("On Farm Treatments")+
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,1000),expression(paste("Emerged Weed Density weeds m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
weeddensity_onfarm_emmeansplot.log

# COMPARE TREATMENTS WITHIN EACH SITE
weeddensity.onfarm.trtdiff.log <- pairs(weeddensity.onfarm.emmeans.log, reverse = F, type = "response")
weeddensity.onfarm.trtdiff.log %>% as_tibble()


# RESEARCH STATION -------------------------------------------------------------

weeddensity.rs.lmer.log <- lmer(log(sumWeed_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                    data = combined.data2 %>% filter(FieldType == "Research Station"))


# LOOK AT LMM OUTPUT
summary(weeddensity.rs.lmer.log)
car::Anova(weeddensity.rs.lmer.log, type = "2")
plot(weeddensity.rs.lmer.log) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
weeddensity.research.emmeans <- emmeans(weeddensity.rs.lmer.log, ~ Treatment | Site:Season, type = "response")
weeddensity.research.emmeans %>% as_tibble()


# PLOT ESTIMATED TREATMENT MEANS
weeddensity_research_emmeansplot <- weeddensity.research.emmeans %>% 
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  xlab("Research Station Treatments")+
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Emerged Weed Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
weeddensity_research_emmeansplot
# ggsave(weeddensity_research_emmeansplot, file = here("results", "weeddensity_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
weeddensity.research.trtdiff <- pairs(weeddensity.research.emmeans, reverse = T)
weeddensity.research.trtdiff %>% as_tibble() %>% arrange(Site, Season)





# GGPLOTTING FIGURES --------------------------------------------------------------

#on farm graph
weeddensity_onfarm_emmeansplot.log <- weeddensity.onfarm.emmeans.log %>%
  as_tibble() %>%
  filter(Site != "Colfax") %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  #labs(title = "Total Weed Density", x = "On Farm Treatments")+
  xlab("On Farm Treatments")+
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(strip.text.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,1000),expression(paste("Emerged Weed Density weeds m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
weeddensity_onfarm_emmeansplot.log



weeddensity_research_emmeansplot <- weeddensity.research.emmeans %>% 
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  xlab("Research Station Treatments")+
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
  scale_y_continuous(expression(paste("Emerged Weed Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
weeddensity_research_emmeansplot



weeddensity_research_emmeansplot+weeddensity_onfarm_emmeansplot.log+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Weed Density by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, hjust = 0.1, face = "bold")))-> dens
dens

density <- patchwork::patchworkGrob(dens)
gridExtra::grid.arrange(density, left = textGrob(expression(paste("Estimated Total Weed Density weeds m"^"-2")), rot = 90)) -> densplot


#Save 3/11/22
# ggsave(densplot, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\totalweeddensity.png", dpi = 600, width = 6, height = 6)
