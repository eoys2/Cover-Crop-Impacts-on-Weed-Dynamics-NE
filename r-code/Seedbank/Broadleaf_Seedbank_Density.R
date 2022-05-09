# Broadleaf seedbank density 

# LOAD LIBRARIES ---------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(gridExtra)
library(patchwork)


# IMPORT COMBINED DATA ---------------------------------------------------------

combined.data2 <- read.csv(file = here("data","FINAL_DATA", "multi_combined.csv")) %>% 
  filter(Treatment != "VetchDNI") #do not use vetch data from ENREC, this filters it out
summary(combined.data2)

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))

# ON FARM ----------------------------------------------------------------------

# FIT GLMM FOR ON FARM 
totbroad.onfarm.glmer <- glmer.nb(totSeeds_Otherbroad ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                  data = combined.data2 %>% filter(Season == "Early", FieldType == "On Farm"))


# LOOK AT LMM OUTPUT
summary(totbroad.onfarm.glmer) 
car::Anova(totbroad.onfarm.glmer, type = "") 
plot(totbroad.onfarm.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbroad.onfarm.emmeans <- emmeans(totbroad.onfarm.glmer, ~ Treatment | Site, type = "response")
totbroad.onfarm.emmeans %>% as_tibble()


# PLOT ESTIMATED TREATMENT MEANS
totbroad_onfarm_emmeansplot <- totbroad.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site, scales = "free")+
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Estimated Broadleaf Seedbank Density m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totbroad_onfarm_emmeansplot   ### Seeing differences in merrick county, more broadleaves in check
# ggsave(totpigs_onfarm_emmeansplot, file = here("results", "totspecies_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE, obtain p value 
totbroad.onfarm.trtdiff <- pairs(totbroad.onfarm.emmeans, reverse = T)
totbroad.onfarm.trtdiff %>% as_tibble()  


# RESEARCH STATION -------------------------------------------------------------

# FIT GLMM FOR RESEARCH STATION
totbroad.research.glmer <- glmer.nb(totSeeds_Otherbroad ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data2 %>% filter(Season == "Early", FieldType == "Research Station"))

# LOOK AT LMM OUTPUT
summary(totbroad.research.glmer)
car::Anova(totbroad.research.glmer, type = "2")
plot(totbroad.research.glmer) #not significant

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbroad.research.emmeans <- emmeans(totbroad.research.glmer, ~ Treatment | Site, type = "response")
totbroad.research.emmeans %>% 
  as_tibble() %>% 
  drop_na()

# PLOT ESTIMATED TREATMENT MEANS
totbroad_research_emmeansplot <- totbroad.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Estimated Broadleaf Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totbroad_research_emmeansplot
# ggsave(totpigs_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbroad.research.trtdiff <- pairs(totbroad.research.emmeans, reverse = T)
totbroad.research.trtdiff %>% as_tibble() #no sig results for other broads. 






# GGPLOT figures --------------------------------------------------------------

totbroad_onfarm_emmeansplot <- totbroad.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site)+
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  scale_y_continuous(expression(paste("Estimated Broadleaf Seedbank Density m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totbroad_onfarm_emmeansplot   #on farm edited 


totbroad_research_emmeansplot <- totbroad.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,3000)) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  #theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,3000), expression(paste("Estimated Pigweed Seedbank Density m"^"-2"))) +
  #scale_y_continuous(expression(paste("Estimated Broadleaf Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totbroad_research_emmeansplot #research station edited 

totbroad_research_emmeansplot+totbroad_onfarm_emmeansplot+
  plot_layout(ncol = 1, widths = c(1,1), heights = c(2,4.5))+
  plot_annotation(title = "Estimated Broadleaf Seedbank Density", theme = theme(plot.title = element_text(size = 18, hjust = 0.25)))-> newplot
newplot

gt <- patchwork::patchworkGrob(newplot)
gridExtra::grid.arrange(gt, left = textGrob(expression(paste("Estimated broadleaf seedbank density in weeds seeds m"^"-2")), rot = 90)) -> broadseedbank
broadseedbank


#Save as desired
#Saved 3/11/22
#ggsave(broadseedbank, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\NOASTERICKBROADseedbankpdf.pdf", dpi = 600, width = 6, height = 6)

