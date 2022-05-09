#FIELD DATA - PIGWEEDS :)
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
# ON FARM - early ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
fieldpig_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = sumPigweed_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Pigweeds \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldpig_onfarm_rawplot 
# ggsave(fieldpig_onfarm_rawplot, file = here("results", "fieldpig_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
fieldpig.onfarm.lmer <- lmer(sumPigweed_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                             data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(fieldpig.onfarm.lmer)
anova(fieldpig.onfarm.lmer)
plot(fieldpig.onfarm.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldpig.onfarm.emmeans <- emmeans(fieldpig.onfarm.lmer, ~ Treatment | Site)
fieldpig.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldpig_onfarm_emmeansplot <- fieldpig.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldpig_onfarm_emmeansplot
# ggsave(fieldpig_onfarm_emmeansplot, file = here("results", "fieldpig_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldpig.onfarm.trtdiff <- pairs(fieldpig.onfarm.emmeans, reverse = T)
fieldpig.onfarm.trtdiff %>% as_tibble()







# ------------------------------------------------------------------------------
# ON FARM - late ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
fieldpig_onfarm_rawplot <- combined.data %>%
  filter(Season == "Late", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = sumPigweed_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Pigweeds \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldpig_onfarm_rawplot 
# ggsave(fieldpig_onfarm_rawplot, file = here("results", "fieldpig_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
fieldpig.onfarm.lmer.late <- lmer(sumPigweed_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                             data = combined.data %>% filter(Season == "Late", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(fieldpig.onfarm.lmer.late)
anova(fieldpig.onfarm.lmer.late)
plot(fieldpig.onfarm.lmer.late)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldpig.onfarm.emmeans.late <- emmeans(fieldpig.onfarm.lmer.late, ~ Treatment | Site)
fieldpig.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldpig_onfarm_emmeansplot <- fieldpig.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldpig_onfarm_emmeansplot
# ggsave(fieldpig_onfarm_emmeansplot, file = here("results", "fieldpig_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldpig.onfarm.trtdiff.late <- pairs(fieldpig.onfarm.emmeans.late, reverse = T)
fieldpig.onfarm.trtdiff.late %>% as_tibble()
# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
fieldpig_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = sumPigweed_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
fieldpig_research_rawplot
# ggsave(fieldpig_research_rawplot, file = here("results", "fieldpig_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (gamma)
fieldpig.research.glmer <- glmer(sumPigweed_Density ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
                                 data = combined.data %>% filter(FieldType == "Research Station"),
                                 family = "Gamma")

# LOOK AT GLMM OUTPUT
summary(fieldpig.research.glmer)
anova(fieldpig.research.glmer)
plot(fieldpig.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldpig.research.emmeans <- emmeans(fieldpig.research.glmer, ~ Treatment | Site:Season, type = "response")
fieldpig.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldpig_research_emmeansplot <- fieldpig.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
fieldpig_research_emmeansplot
# ggsave(fieldpig_research_emmeansplot, file = here("results", "fieldpig_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldpig.research.trtdiff <- pairs(fieldpig.research.emmeans, reverse = T)
fieldpig.research.trtdiff %>% as_tibble()
