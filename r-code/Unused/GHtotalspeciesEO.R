#Ran 11/3 Elizabeth Oys


# TOTAL SPECIES 
# Recalculated based on both seed buckets for one measure per rep
# Note: only 1 measure so we filter for plots and models on Season == "Early"
# Count ~ Negative Binomial

# Questions:
# These models actually seem to fit pretty well!

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

# PLOT RAW DATA
totspecies_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = Total_Species, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Species") +
  scale_fill_manual(values = c("darkgray", "green4"))
totspecies_onfarm_rawplot 
# ggsave(totspecies_onfarm_rawplot, file = here("results", "totspecies_onfarm_rawplot.png"), width = 6, height = 6)

# FIT GLMM FOR ON FARM 
totspecies.onfarm.glmer <- glmer.nb(Total_Species ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))


# LOOK AT LMM OUTPUT
summary(totspecies.onfarm.glmer)
car::Anova(totspecies.onfarm.glmer)
plot(totspecies.onfarm.glmer) #THIS DOESNT LOOK WELL DISPERSED

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totspecies.onfarm.emmeans <- emmeans(totspecies.onfarm.glmer, ~ Treatment | Site, type = "response")
totspecies.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totspecies_onfarm_emmeansplot <- totspecies.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Species") +
  scale_fill_manual(values = c("darkgray", "green4"))
totspecies_onfarm_emmeansplot
# ggsave(totspecies_onfarm_emmeansplot, file = here("results", "totspecies_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totspecies.onfarm.trtdiff <- pairs(totspecies.onfarm.emmeans, reverse = T)
totspecies.onfarm.trtdiff %>% as_tibble()

# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totspecies_research_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = Total_Species, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Species \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totspecies_research_rawplot
# ggsave(totspecies_research_rawplot, file = here("results", "totspecies_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM FOR RESEARCH STATION
totspecies.research.glmer <- glmer.nb(Total_Species ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                      data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"))

# LOOK AT LMM OUTPUT
summary(totspecies.research.glmer)
anova(totspecies.research.glmer)
plot(totspecies.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totspecies.research.emmeans <- emmeans(totspecies.research.glmer, ~ Treatment | Site, type = "response")
totspecies.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totspecies_research_emmeansplot <- totspecies.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Species") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totspecies_research_emmeansplot
# ggsave(totspecies_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totspecies.research.trtdiff <- pairs(totspecies.research.emmeans, reverse = T)
totspecies.research.trtdiff %>% as_tibble()

# ----
#For total species

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
