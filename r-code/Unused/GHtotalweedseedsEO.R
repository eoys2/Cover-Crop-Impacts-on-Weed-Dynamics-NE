

#Ran 11/3 Elizabeth Oys 
# TOTAL WEED SEEDS 
# Count - summed across buckets for research stations
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
library(ggplot2)

# ------------------------------------------------------------------------------
# IMPORT COMBINED DATA ---------------------------------------------------------
# ------------------------------------------------------------------------------

combined.data2 <- read.csv(file = here("data", "convupdated-combined-dataEO.csv"))

# make sure columns are formatted proper
factorCols <- c("Site", "Treatment", "Rep", "Sample_Date", "Season", "Current_crop", "Previous_crop", "Crop_Stage", "FieldType")
combined.data2[,factorCols] <- lapply(combined.data[,factorCols], factor)

summary(combined.data2)


combined.data <- read.csv(file = here("data", "updated-combined-dataEO.csv"))
#summary(combined.data)



# ------------------------------------------------------------------------------
# ON FARM ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totseeds_weeds_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Weeds, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weed Seeds per m2") +
  scale_fill_manual(values = c("darkgray", "green4"))
totseeds_weeds_onfarm_rawplot 
# ggsave(totseeds_weeds_onfarm_rawplot, file = here("results", "totseeds_weeds_onfarm_rawplot.png"), width = 6, height = 6)


hist(combined.data2$totSeeds_Weeds)


#now when i use the converted data it doesnt work with the glmer

# FIT GLMM (total weed seeds = trt + site + trtxsite + random repxsite)

#glmer with nb
totseeds_weeds.onfarm.glmer <- glmer.nb(totSeeds_Weeds ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                        data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))
#fits actual neg binomial distribution for non-normal data, log-link fxn
#change this to log+1 transformation like we have with the logweeddensity file


#log transformed lmer
# totseeds_weeds.onfarm.glmer2 <- lmer(log(totSeeds_Weeds+1) ~ Treatment*Site + (1 | Rep:Site),
#      data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))



# LOOK AT LMM OUTPUT
summary(totseeds_weeds.onfarm.glmer)
car::Anova(totseeds_weeds.onfarm.glmer)
plot(totseeds_weeds.onfarm.glmer)



# summary(totseeds_weeds.onfarm.glmer2)
# car::Anova(totseeds_weeds.onfarm.glmer2)
# plot(totseeds_weeds.onfarm.glmer2)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totseeds_weeds.onfarm.emmeans <- emmeans(totseeds_weeds.onfarm.glmer, ~ Treatment | Site, type = "response") #responses should actually be the actual mean + 1
totseeds_weeds.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS (converted to weeds/m2 estimations)
totseeds_weeds_onfarm_emmeansplot <- totseeds_weeds.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (repsonse+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weed Seeds") +
  scale_fill_manual(values = c("darkgray", "red2"))
totseeds_weeds_onfarm_emmeansplot
# ggsave(totseeds_weeds_onfarm_emmeansplot, file = here("results", "totseeds_weeds_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totseeds_weeds.onfarm.trtdiff <- pairs(totseeds_weeds.onfarm.emmeans, reverse = T)
totseeds_weeds.onfarm.trtdiff %>% as_tibble()

# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totseeds_weeds_research_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Weeds, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weed Seeds \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totseeds_weeds_research_rawplot
# ggsave(totseeds_weeds_research_rawplot, file = here("results", "totseeds_weeds_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (total weed seeds = trt + site + trtxsite + random repxsite)
totseeds_weeds.research.glmer <- glmer.nb(totSeeds_Weeds ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                          data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"))

# LOOK AT LMM OUTPUT
summary(totseeds_weeds.research.glmer)
anova(totseeds_weeds.research.glmer)
plot(totseeds_weeds.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totseeds_weeds.research.emmeans <- emmeans(totseeds_weeds.research.glmer, ~ Treatment | Site, type = "response")
totseeds_weeds.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totseeds_weeds_research_emmeansplot <- totseeds_weeds.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weed Seeds") +
  scale_fill_manual(values = c("darkgray", "red2", "indianred3"))
totseeds_weeds_research_emmeansplot
# ggsave(totseeds_weeds_research_emmeansplot, file = here("results", "totseeds_weeds_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totseeds_weeds.research.trtdiff <- pairs(totseeds_weeds.research.emmeans, reverse = T)
totseeds_weeds.research.trtdiff %>% as_tibble()







# All Together ------------------------------------------------------------

# PLOT RAW DATA
totseeds_weeds_all_rawplot <- combined.data %>%
  filter(Season == "Early") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Weeds, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weed Seeds \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue","red"))
totseeds_weeds_all_rawplot

#glmer
totseeds_weeds.all.glmer <- glmer.nb(totSeeds_Weeds ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                     data = combined.data %>% filter(Season == "Early"))


#Bargraph emmeans
totseeds_weeds.all.emmeans <- emmeans(totseeds_weeds.all.glmer, ~ Treatment | Site, type = "response")
totseeds_weeds.all.emmeans %>% as_tibble()











totseeds_weeds_all_emmeansplot <- totseeds_weeds.all.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weed Seeds") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue","red"))
totseeds_weeds_all_emmeansplot
#doesnt really work so try patching together instead using patchwork 

#install.packages('patchwork')
library(patchwork)
totseeds_weeds_research_emmeansplot/totseeds_weeds_onfarm_emmeansplot + plot_layout(guides="collect",ncol = 1)



# GLMM adaptive for total weed seeds --------------------------------------


# ------------------------------------------------------------------------------
# CODE FOR GLMMadaptive #
#------------------------------------------------------
# ------------------------------------------------------------------------------
#install.packages('GLMMadaptive')
library(GLMMadaptive)
library(tidyverse)

# # FIT GLMM FOR RESEARCH STATION
totweeds.research.glmmadapt <- mixed_model(fixed = totSeeds_Weeds ~ Treatment + Site + Treatment:Site, random ~ 1 | Rep:Site, data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"), family = poisson())


totweeds.research.glmmadapt2 <- mixed_model(fixed = totSeeds_Weeds ~ Treatment + Site + Treatment:Site, random ~ 1 | Rep:Site, data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"), family = negative.binomial()) #use nb instead of poisson (because of the distribution)

# 
# # LOOK AT GLMM OUTPUT
summary(totweeds.research.glmmadapt)

summary(totweeds.research.glmmadapt2)


anova(totweeds.research.glmmadapt)
plot(totweeds.research.glmmadapt)
# 
# # GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totweeds.research.emmeans <- emmeans(totweeds.research.glmmadapt2, ~ Treatment | Site, type = "response")
totweeds.research.emmeans %>% as_tibble()
# 
# # PLOT ESTIMATED TREATMENT MEANS
totweeds_research_emmeansplot <- totweeds.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = exp(emmean), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weeds") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totweeds_research_emmeansplot
# # ggsave(totspecies_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)
# 
# # COMPARE TREATMENTS WITHIN EACH SITE
totweeds.research.trtdiff <- pairs(totweeds.research.emmeans, reverse = T)
totweeds.research.trtdiff %>% as_tibble()





#ON FARM GLMM ADAPTIVE (wont run for me)

# # FIT GLMM FOR RESEARCH STATION
totweeds.farm.glmmadapt <- mixed_model(fixed = totSeeds_Weeds ~ Treatment + Site + Treatment:Site, random ~ 1 | Rep:Site, data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"), family = poisson())

totweeds.farm.glmmadapt2 <- mixed_model(fixed = totSeeds_Weeds ~ Treatment + Site + Treatment:Site, random ~ 1 | Rep:Site, data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"), family = negative.binomial())


library(GLMMadaptive)
# 
# # LOOK AT GLMM OUTPUT
summary(totweeds.farm.glmmadapt)
summary(totweeds.farm.glmmadapt2)

anova(totweeds.farm.glmmadapt)
plot(totweeds.farm.glmmadapt)
# 
# # GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totweeds.farm.emmeans <- emmeans(totweeds.farm.glmmadapt, ~ Treatment | Site, type = "response")
totweeds.farm.emmeans %>% as_tibble()
# 
# # PLOT ESTIMATED TREATMENT MEANS
totweeds_farm_emmeansplot <- totweeds.farm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = exp(emmean), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Weeds") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totweeds_farm_emmeansplot
# # ggsave(totspecies_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)
# 
# # COMPARE TREATMENTS WITHIN EACH SITE
totweeds.farm.trtdiff <- pairs(totweeds.research.emmeans, reverse = T)
totweeds.farm.trtdiff %>% as_tibble()

