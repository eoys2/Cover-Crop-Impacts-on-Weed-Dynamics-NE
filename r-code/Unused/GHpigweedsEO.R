#Ran 11/3 Elizabeth Oys
# Pigweeds 
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



combined.data2 <- read.csv(file = here("data", "multi_combined.csv")) %>% 
  #filter(Site != "Colfax") %>% 
  filter(Treatment != "VetchDNI")
summary(combined.data)


# ------------------------------------------------------------------------------
# ON FARM ----------------------------------------------------------------------
# ------------------------------------------------------------------------------


#totSeeds_Pigweed
# PLOT RAW DATA
totpigs_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Pigweed, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Pigweeds") +
  scale_fill_manual(values = c("darkgray", "green4"))
totpigs_onfarm_rawplot 
# ggsave(totspecies_onfarm_rawplot, file = here("results", "totspecies_onfarm_rawplot.png"), width = 6, height = 6)



# FIT GLMM FOR ON FARM 
totpigs.onfarm.glmer <- glmer.nb(totSeeds_Pigweed ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                 data = combined.data2 %>% filter(Season == "Early", FieldType == "On Farm"))




# LOOK AT LMM OUTPUT
summary(totpigs.onfarm.glmer) # overall across the 4 sites, p value for treatments was 0.03996. But I think in this case it means that pigweeds increased in the seedbank at these sites??
car::Anova(totpigs.onfarm.glmer)
plot(totpigs.onfarm.glmer) #not super well dispersed?

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totpigs.onfarm.emmeans <- emmeans(totpigs.onfarm.glmer, ~ Treatment | Site, type = "response")
totpigs.onfarm.emmeans %>% as_tibble()



# PLOT ESTIMATED TREATMENT MEANS
totpigs_onfarm_emmeansplot <- totpigs.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.3) +
  facet_wrap(~Site, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Pigweeds") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totpigs_onfarm_emmeansplot



# ggsave(totpigs_onfarm_emmeansplot, file = here("results", "totspecies_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totpigs.onfarm.trtdiff <- pairs(totpigs.onfarm.emmeans, reverse = T)
totpigs.onfarm.trtdiff %>% as_tibble() 

# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totpigs_research_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Pigweed, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Pigweeds \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totpigs_research_rawplot
# ggsave(totspecies_research_rawplot, file = here("results", "totspecies_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM FOR RESEARCH STATION
totpigs.research.glmer <- glmer.nb(totSeeds_Pigweed ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                   data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"))


# LOOK AT LMM OUTPUT
summary(totpigs.research.glmer)
anova(totpigs.research.glmer)
plot(totpigs.research.glmer) #no significant results

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totpigs.research.emmeans <- emmeans(totpigs.research.glmer, ~ Treatment | Site, type = "response")
totpigs.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totpigs_research_emmeansplot <- totpigs.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Pigweeds") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totpigs_research_emmeansplot
# ggsave(totpigs_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totpigs.research.trtdiff <- pairs(totpigs.research.emmeans, reverse = T)
totpigs.research.trtdiff %>% as_tibble() 





# ------------------------------------------------------------------------------
# CODE FOR GLMMadaptive #
#------------------------------------------------------
# ------------------------------------------------------------------------------
#install.packages('GLMMadaptive')
library(GLMMadaptive)

# # FIT GLMM FOR RESEARCH STATION
totspecies.research.glmmadapt <- mixed_model(fixed = Total_Species ~ Treatment + Site + Treatment:Site, random ~ 1 | Rep:Site, data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"), family = poisson())
# 
# # LOOK AT GLMM OUTPUT
summary(totspecies.research.glmmadapt)
anova(totspecies.research.glmmadapt)
plot(totspecies.research.glmmadapt)
# 
# # GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totspecies.research.emmeans <- emmeans(totspecies.research.glmmadapt, ~ Treatment | Site, type = "response")
totspecies.research.emmeans %>% as_tibble()
# 
# # PLOT ESTIMATED TREATMENT MEANS
totspecies_research_emmeansplot <- totspecies.research.emmeans %>% 
as_tibble() %>%
ggplot(aes(x = Treatment, y = exp(emmean), fill = Treatment)) +
geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)), width = 0.3) +
facet_wrap(~Site) +
theme_bw() +
theme(aspect.ratio = 1) +
scale_y_continuous("Total Weed Species") +
scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totspecies_research_emmeansplot
# # ggsave(totspecies_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)
# 
# # COMPARE TREATMENTS WITHIN EACH SITE
totspecies.research.trtdiff <- pairs(totspecies.research.emmeans, reverse = T)
totspecies.research.trtdiff %>% as_tibble()
