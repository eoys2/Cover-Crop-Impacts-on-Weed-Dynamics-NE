# Grass seedbank density
# Note: no grasses found at research stations in seedbanks

# LOAD LIBRARIES ---------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(ggplot2)


# IMPORT COMBINED DATA ---------------------------------------------------------

combined.data2 <- read.csv(file = here("data","FINAL_DATA", "multi_combined.csv")) %>% 
  filter(Treatment != "VetchDNI") #do not use vetch data from ENREC, this filters it out
summary(combined.data2)

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


# ON FARM ----------------------------------------------------------------------

# FIT GLMM FOR ON FARM 
totgrass.onfarm.glmer <- glmer.nb(totSeeds_Grasses ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                  data = combined.data2 %>% filter(Season == "Early", FieldType == "On Farm"))



# LOOK AT LMM OUTPUT
summary(totgrass.onfarm.glmer) 
car::Anova(totgrass.onfarm.glmer, type = "2") 
plot(totgrass.onfarm.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totgrass.onfarm.emmeans <- emmeans(totgrass.onfarm.glmer, ~ Treatment | Site, type = "response")
totgrass.onfarm.emmeans %>% as_tibble()


# PLOT ESTIMATED TREATMENT MEANS
totgrass_onfarm_emmeansplot <- totgrass.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(expression(paste("Estimated Grass Seedbank Density m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totgrass_onfarm_emmeansplot   

# COMPARE TREATMENTS WITHIN EACH SITE
totgrass.onfarm.trtdiff <- pairs(totgrass.onfarm.emmeans, reverse = T)
totgrass.onfarm.trtdiff %>% as_tibble() 



# RESEARCH STATION -------------------------------------------------------------

#Note: NO grasses found in the seedbanks at ENREC and SCAL!!! so this will not run.
# FIT GLMM FOR RESEARCH STATION
totgrass.research.glmer <- glmer.nb(totSeeds_Grasses ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data2 %>% filter(Season == "Early", FieldType == "Research Station"))



# GGPLOT for on-farm only  --------------------------------------
#no grasses at enrec/scal. 
totgrass_onfarm_emmeansplot <- totgrass.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  ggtitle("Estimated Grass Seedbank Density")+
  theme(plot.title = element_text(size=20, hjust = 0.5))+
  scale_y_continuous(expression(paste("Estimated grass seedbank density in weed seeds m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totgrass_onfarm_emmeansplot   ### This turned out weird?

#saved 3/11/22
#ggsave(totgrass_onfarm_emmeansplot, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\grassseedbank.png", dpi = 600, width = 6, height = 6)


