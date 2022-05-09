# Pigweed seedbank density
# GGPLOTs of figures at bottom of the file

# LOAD LIBRARIES ---------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(grid)
library(patchwork)
library(scales)
library(gridExtra)


# IMPORT COMBINED DATA ---------------------------------------------------------
combined.data2 <- read.csv(file = here("data","FINAL_DATA", "multi_combined.csv")) %>% 
  filter(Treatment != "VetchDNI") #do not use vetch data from ENREC, this filters it out
summary(combined.data2)

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))



# ON FARM ----------------------------------------------------------------------

# FIT GLMM FOR ON FARM 
totpigs.onfarm.glmer <- glmer.nb(totSeeds_Pigweed ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                 data = combined.data2 %>% filter(Season == "Early", FieldType == "On Farm"))


# LOOK AT LMM OUTPUT
summary(totpigs.onfarm.glmer) 
car::Anova(totpigs.onfarm.glmer, type = "2")
plot(totpigs.onfarm.glmer) 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totpigs.onfarm.emmeans <- emmeans(totpigs.onfarm.glmer, ~ Treatment | Site, type = "response")
totpigs.onfarm.emmeans %>% as_tibble()

totpigs_onfarm_emmeansplot <- totpigs.onfarm.emmeans %>% 
  as_tibble() %>%
  #filter(Site == "Greeley") %>% 
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site, scales = "free") +
  theme_bw() +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  #theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  #theme(aspect.ratio = 1) +
  #labs(x = Treatment, y = expression(Estimated Pigweed Density m))
  scale_y_continuous(expression(paste("Estimated Pigweed Seedbank Density weeds m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totpigs_onfarm_emmeansplot
  

# COMPARE TREATMENTS WITHIN EACH SITE
totpigs.onfarm.trtdiff <- pairs(totpigs.onfarm.emmeans, reverse = T)
totpigs.onfarm.trtdiff %>% 
  as_tibble() 



# RESEARCH STATION -------------------------------------------------------------

# FIT GLMM FOR RESEARCH STATION
totpigs.research.glmer <- glmer.nb(totSeeds_Pigweed ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                   data = combined.data2 %>% filter(Season == "Early", FieldType == "Research Station"))

# LOOK AT LMM OUTPUT
summary(totpigs.research.glmer)
car::Anova(totpigs.research.glmer, type ="")
plot(totpigs.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totpigs.research.emmeans <- emmeans(totpigs.research.glmer, ~ Treatment | Site, type = "response")
totpigs.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totpigs_research_emmeansplot <- totpigs.research.emmeans %>% 
  as_tibble() %>%
  #filter(Site == "SCAL") %>% 
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.3) + 
  facet_wrap(~Site, scales = "free") +
  theme_bw() +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
#theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Estimated Pigweed Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totpigs_research_emmeansplot

# COMPARE TREATMENTS WITHIN EACH SITE
totpigs.research.trtdiff <- pairs(totpigs.research.emmeans, reverse = T)
totpigs.research.trtdiff %>% as_tibble() 



combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


# GGPLOTTING: combining on-farm and research station barcharts --------------------------------------------------------------
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


totpigs_onfarm_emmeansplot <- totpigs.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  #theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  #theme(aspect.ratio = 1) +
  #labs(x = Treatment, y = expression(Estimated Pigweed Density m))
  scale_y_continuous(limits = c(0,5000),expression(paste("Estimated Pigweed Seedbank Density weeds m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totpigs_onfarm_emmeansplot


totpigs_research_emmeansplot <- totpigs.research.emmeans %>% 
  as_tibble() %>%
  #filter(Site == "SCAL") %>% 
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.1) + 
  facet_wrap(~Site) +
  theme_bw() +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  #theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,5000), expression(paste("Estimated Pigweed Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("Check" = "darkgray", "Cereal Rye"="#B46504","Hairy Vetch"="#708D81"))
totpigs_research_emmeansplot


totpigs_research_emmeansplot+totpigs_onfarm_emmeansplot+
  plot_layout(ncol = 1, widths = c(1,1), heights = c(2,4.5))+
plot_annotation(title = "Estimated Pigweed Seedbank Density", theme = theme(plot.title = element_text(size = 18, hjust = 0.25)))-> newplot
newplot

gt <- patchwork::patchworkGrob(newplot)
gridExtra::grid.arrange(gt, left = textGrob(expression(paste("Estimated pigweed seedbank density in weeds seeds per acre")), rot = 90)) -> pigweedseedbank

#Save as desired (note: you will need to change the pathway to your own)
#Save 3/11/22
#ggsave(pigweedseedbank, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\UpdatedFigure_3.pdf", dpi = 950, width = 6, height = 6)


