# Ran 11/3 by Elizabeth Oys
# Grasses 
# 

# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM

# ------------------------------------------------------------------------------
# IMPORT COMBINED DATA ---------------------------------------------------------
# ------------------------------------------------------------------------------
combined.data <- read.csv(file = here("data", "updated-combined-dataEO.csv"))
summary(combined.data)

# ------------------------------------------------------------------------------
# ON FARM ----------------------------------------------------------------------
# ------------------------------------------------------------------------------


#totSeeds_Grasses
# PLOT RAW DATA
totgrass_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Grasses, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Grasses") +
  scale_fill_manual(values = c("darkgray", "green4"))
totgrass_onfarm_rawplot 
# ggsave(totspecies_onfarm_rawplot, file = here("results", "totspecies_onfarm_rawplot.png"), width = 6, height = 6)



# FIT GLMM FOR ON FARM 
totgrass.onfarm.glmer <- glmer.nb(totSeeds_Grasses ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                  data = combined.data %>% filter(Season == "Early", FieldType == "On Farm", Site != "Colfax"))



# LOOK AT LMM OUTPUT
summary(totgrass.onfarm.glmer) # no sig results
car::Anova(totgrass.onfarm.glmer) #treatment was 0.08
plot(totgrass.onfarm.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totgrass.onfarm.emmeans <- emmeans(totgrass.onfarm.glmer, ~ Treatment | Site, type = "response")
totgrass.onfarm.emmeans %>% as_tibble()



# PLOT ESTIMATED TREATMENT MEANS
totgrass_onfarm_emmeansplot <- totgrass.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Grasses") +
  scale_fill_manual(values = c("darkgray", "green4"))
totgrass_onfarm_emmeansplot   ### This turned out weird?
# ggsave(totpigs_onfarm_emmeansplot, file = here("results", "totspecies_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totgrass.onfarm.trtdiff <- pairs(totgrass.onfarm.emmeans, reverse = T)
totgrass.onfarm.trtdiff %>% as_tibble() #p value = or < 0.05: at Merrick (increase) and Greeley (increase)

# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totgrass_research_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Grasses, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Grasses \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totgrass_research_rawplot
# ggsave(totspecies_research_rawplot, file = here("results", "totspecies_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM FOR RESEARCH STATION
totgrass.research.glmer <- glmer.nb(totSeeds_Grasses ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"))

# LOOK AT LMM OUTPUT
summary(totgrass.research.glmer)
anova(totgrass.research.glmer)
plot(totgrass.research.glmer) #not enough data for this for research stations

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totgrass.research.emmeans <- emmeans(totgrass.research.glmer, ~ Treatment | Site, type = "response")
totgrass.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totgrass_research_emmeansplot <- totgrass.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Grasses") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totgrass_research_emmeansplot
# ggsave(totpigs_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totgrass.research.trtdiff <- pairs(totgrass.research.emmeans, reverse = T)
totgrass.research.trtdiff %>% as_tibble() 





# ------------------------------------------------------------------------------
# CODE FOR GLMMadaptive --------------------------------------------------------
# ------------------------------------------------------------------------------
# library(GLMMadaptive)

# # FIT GLMM FOR RESEARCH STATION
# totspecies.research.glmmadapt <- mixed_model(fixed = Total_Species ~ Treatment + Site + Treatment:Site,
#                                              random ~ 1 | Rep:Site,
#                                       data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"),
#                                       family = poisson())
# 
# # LOOK AT GLMM OUTPUT
# summary(totspecies.research.glmmadapt)
# anova(totspecies.research.glmmadapt)
# plot(totspecies.research.glmmadapt)
# 
# # GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
# totspecies.research.emmeans <- emmeans(totspecies.research.glmmadapt, ~ Treatment | Site, type = "response")
# totspecies.research.emmeans %>% as_tibble()
# 
# # PLOT ESTIMATED TREATMENT MEANS
# totspecies_research_emmeansplot <- totspecies.research.emmeans %>% 
#   as_tibble() %>%
#   ggplot(aes(x = Treatment, y = exp(emmean), fill = Treatment)) +
#   geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
#   geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)), width = 0.3) +
#   facet_wrap(~Site) +
#   theme_bw() +
#   theme(aspect.ratio = 1) +
#   scale_y_continuous("Total Weed Seeds") +
#   scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
# totspecies_research_emmeansplot
# # ggsave(totspecies_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)
# 
# # COMPARE TREATMENTS WITHIN EACH SITE
# totspecies.research.trtdiff <- pairs(totspecies.research.emmeans, reverse = T)
# totspecies.research.trtdiff %>% as_tibble()
