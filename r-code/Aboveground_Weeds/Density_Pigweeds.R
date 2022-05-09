
# Pigweed DENSITY 

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

#apply factor levels for graphs
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


# ON FARM ----------------------------------------------------------------------

# FIT LINEAR MIXED MODEL
pigdensity.onfarm.lmer.log <- lmer(log(sumPigweed_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                    data = combined.data2 %>% filter(FieldType == "On Farm", Site != "Colfax"))

# LOOK AT LMM OUTPUT for log data
summary(pigdensity.onfarm.lmer.log)
anova(pigdensity.onfarm.lmer.log)
plot(pigdensity.onfarm.lmer.log) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
pigdensity.onfarm.emmeans.log <- emmeans(pigdensity.onfarm.lmer.log, ~ Treatment | Site:Season, type = "response")

#remove sites with extremeley low mean (i.e. in actual dataset, there are zero pigweeds but model causes response to be very very small (we cannot have 0.0001th of a weed))
pig <- pigdensity.onfarm.emmeans.log %>% 
  as_tibble() %>% 
  drop_na() %>% 
  filter(response > 0.0001)
  


# PLOT ESTIMATED TREATMENT MEANS
pigdensity_onfarm_emmeansplot.log <- pig %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  #filter(Site == "Greeley") %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Emerged Pigweed Density weeds m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
pigdensity_onfarm_emmeansplot.log

# COMPARE TREATMENTS WITHIN EACH SITE
pigdensity.onfarm.trtdiff.log <- pairs(pigdensity.onfarm.emmeans.log, reverse = F, type = "response")
pigdensity.onfarm.trtdiff.log %>% as_tibble()


# RESEARCH STATION -------------------------------------------------------------

pigdensity.rs.lmer.log <- lmer(log(sumPigweed_Density+1) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                data = combined.data2 %>% filter(FieldType == "Research Station"))



# LOOK AT LMM OUTPUT
summary(pigdensity.rs.lmer.log)
car::Anova(pigdensity.rs.lmer.log, type = 2)
plot(pigdensity.rs.lmer.log)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
pigdensity.research.emmeans <- emmeans(pigdensity.rs.lmer.log, ~ Treatment | Site:Season, type = "response")
pigdensity.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
pigdensity_research_emmeansplot <- pigdensity.research.emmeans %>% 
  as_tibble() %>%
  #filter(Site == "SCAL") %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Emerged Pigweed Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
pigdensity_research_emmeansplot
# ggsave(pigdensity_research_emmeansplot, file = here("results", "pigdensity_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
pigdensity.research.trtdiff <- pairs(pigdensity.research.emmeans, reverse = F, type = "response")
pigdensity.research.trtdiff %>% as_tibble() %>% arrange(Site, Season)





# GGPLOTTING FIGURES --------------------------------------------------------------

#on farm sites
pigdensity_onfarm_emmeansplot.log <- pig %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season, drop = TRUE) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(strip.text.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,1000),expression(paste("Emerged Pigweed Density weeds m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
pigdensity_onfarm_emmeansplot.log

#research station sites
pigdensity_research_emmeansplot <- pigdensity.research.emmeans %>% 
  as_tibble() %>%
  #filter(Site == "SCAL") %>% 
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
  scale_y_continuous(expression(paste("Emerged Pigweed Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
pigdensity_research_emmeansplot


#combine on farm and research stations
pigdensity_research_emmeansplot+pigdensity_onfarm_emmeansplot.log+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Total Pigweed Density by Site and Sample Period", theme = theme(plot.title = element_text(size = 18, face = "bold")))-> dens
dens

density <- patchwork::patchworkGrob(dens)
gridExtra::grid.arrange(density, left = textGrob(expression(paste("Estimated Total Pigweed Density weeds m"^"-2")), rot = 90)) -> densplot


#save as desired (change to your own pathway)
#Save 3/11/22
# ggsave(densplot, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\totalweeddensity.png", dpi = 600, width = 6, height = 6)
