#Ran 11/8 Elizabeth Oys 


# WEED DENSITY 
# Summed over the 2 samples
# Note: we now have 2 measures (Early / Late)
# Count ~ Negative Binomial

# Questions:
# Missing data for Merrick Late Season?
# Quite a variety in the number of weed plants (i.e. weed density) across the locations.
# ENREC Early has an observation where over 2000 weeds were counted?? This is quite a bit higher than the others.
# Scaled the data by dividing by 100 in the GLMM. Maybe seem to fit okay?
# Again, let's check the 0's, is this common?

# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM


# New packages for ZINB ---------------------------------------------------

library(pscl)

# ------------------------------------------------------------------------------
# IMPORT COMBINED DATA ---------------------------------------------------------
# ------------------------------------------------------------------------------
combined.data <- read.csv(file = here("data", "updated-combined-dataEO.csv"))
summary(combined.data)

#for zinb
zinb <- read.csv(file = here("data", "updated-combined-dataEO.csv"))
farmzinb <- within(zinb, {
  trtmt <- as.factor(Treatment)
  farmsites <- as.factor(Site)
  szn <- as.factor(Season)
})
summary(farmzinb)

# ------------------------------------------------------------------------------
# ON FARM ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
weeddensity_onfarm_rawplot <- combined.data %>%
  filter(FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = sumWeed_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_y") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Weed Density \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "firebrick4"))
weeddensity_onfarm_rawplot 
# ggsave(weeddensity_onfarm_rawplot, file = here("results", "weeddensity_onfarm_rawplot.png"), width = 3, height = 12)

# FIT GLMM (weed density = trt + site + season + trtxsite + trtxseason + seasonxsite + trtxseasonxsite + random repxsite + random trtxrepxsite)
weeddensity.onfarm.glmer <- glmer.nb(sumWeed_Density/100 ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                                     data = combined.data %>% filter(FieldType == "On Farm"))



weeddensity.g.glmer <- glmer.nb(sumWeed_Density/100 ~ Treatment*Season + (1 + Treatment | Rep),
                                     data = combined.data %>% 
                                    filter(Site %in% "Greeley")) #runs but havent done anything with emmeans yet. 






# # trying a zinb -----------------------------------------------------------
# 
# farms <- combined.data %>% 
#   filter(FieldType == "On Farm")
# 
# onfarm.zinb <- zeroinfl(sumWeed_Density/100 ~ trtmt*farmsites*szn + (Rep:farmsites),
#                         data = farmzinb, dist = "negbin", EM = TRUE) #not recognizing the non numerical parts of the argument...
#this is not working....




# LOOK AT LMM OUTPUT
summary(weeddensity.onfarm.glmer)
anova(weeddensity.onfarm.glmer)
plot(weeddensity.onfarm.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
weeddensity.onfarm.emmeans <- emmeans(weeddensity.onfarm.glmer, ~ Treatment | Site:Season, type = "response")
weeddensity.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
weeddensity_onfarm_emmeansplot <- weeddensity.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*100, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL*100, ymax = asymp.UCL*100), width = 0.3) +
  facet_grid(Site~Season, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Weed Density") +
  scale_fill_manual(values = c("darkgray", "firebrick4"))
weeddensity_onfarm_emmeansplot
# ggsave(weeddensity_onfarm_emmeansplot, file = here("results", "weeddensity_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
weeddensity.onfarm.trtdiff <- pairs(weeddensity.onfarm.emmeans, reverse = T)
weeddensity.onfarm.trtdiff %>% as_tibble()

# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
weeddensity_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = sumWeed_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_y") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Weed Density \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
weeddensity_research_rawplot
# ggsave(weeddensity_research_rawplot, file = here("results", "weeddensity_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (weed density = trt + site + season + trtxsite + trtxseason + seasonxsite + trtxseasonxsite + random repxsite + random trtxrepxsite)
weeddensity.research.glmer <- glmer.nb(sumWeed_Density/100 ~ Treatment*Site*Season + (1 +Treatment | Rep:Site),
                                       data = combined.data %>% filter(FieldType == "Research Station"))

# LOOK AT LMM OUTPUT
summary(weeddensity.research.glmer)
car::Anova(weeddensity.research.glmer)
plot(weeddensity.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
weeddensity.research.emmeans <- emmeans(weeddensity.research.glmer, ~ Treatment | Site:Season, type = "response")
weeddensity.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
weeddensity_research_emmeansplot <- weeddensity.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*100, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL*100, ymax = asymp.UCL*100), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Weed Density") +
  scale_fill_manual(values = c("darkgray", "firebrick4", "red3"))
weeddensity_research_emmeansplot
# ggsave(weeddensity_research_emmeansplot, file = here("results", "weeddensity_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
weeddensity.research.trtdiff <- pairs(weeddensity.research.emmeans, reverse = T)
weeddensity.research.trtdiff %>% as_tibble() %>% arrange(Site, Season)
