# Ran 11/8 by Elizabeth Oys


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
combined.data <- read.csv(file = here("data", "updated-combined-dataEO.csv")) %>% 
  filter(Site != "Colfax")
summary(combined.data)

# ------------------------------------------------------------------------------
# ON FARM - Early #----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totbiomass_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Total, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbiomass_onfarm_rawplot 
# ggsave(totbiomass_onfarm_rawplot, file = here("results", "totbiomass_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
# totbiomass.onfarm.lmer <- lmer(avgBiomass_Total ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
#                                data = combined.data %>% filter(Season == "Early", FieldType == "On Farm")) #change season depending on what you want to look at

#may also consider doing log transformation here
totbiomass.onfarm.lmer <- lmer(log(avgBiomass_Total+1) ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                               data = combined.data %>% filter(Season == "Early", FieldType == "On Farm", Site != "Colfax")) #change season depending on what you want to look at


# LOOK AT LMM OUTPUT
summary(totbiomass.onfarm.lmer)
anova(totbiomass.onfarm.lmer)
plot(totbiomass.onfarm.lmer)


# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.onfarm.emmeans <- emmeans(totbiomass.onfarm.lmer, ~ Treatment | Site, type = "response")
totbiomass.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totbiomass_onfarm_emmeansplot <- totbiomass.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbiomass_onfarm_emmeansplot
# ggsave(totbiomass_onfarm_emmeansplot, file = here("results", "totbiomass_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbiomass.onfarm.trtdiff <- pairs(totbiomass.onfarm.emmeans, reverse = T)
totbiomass.onfarm.trtdiff %>% as_tibble()



# ------------------------------------------------------------------------------
# ON FARM - Late #----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totbiomass_onfarm_rawplot <- combined.data %>%
  filter(Season == "Late", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Total, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbiomass_onfarm_rawplot 
# ggsave(totbiomass_onfarm_rawplot, file = here("results", "totbiomass_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
totbiomass.onfarm.lmer.late <- lmer(log(avgBiomass_Total+1) ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data %>% filter(Season == "Late", FieldType == "On Farm", Site != "Colfax"))


# LOOK AT LMM OUTPUT
summary(totbiomass.onfarm.lmer.late)
anova(totbiomass.onfarm.lmer.late)
plot(totbiomass.onfarm.lmer.late)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.onfarm.emmeans.late <- emmeans(totbiomass.onfarm.lmer.late, ~ Treatment | Site)
totbiomass.onfarm.emmeans.late %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totbiomass_onfarm_emmeansplot <- totbiomass.onfarm.emmeans.late %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbiomass_onfarm_emmeansplot
# ggsave(totbiomass_onfarm_emmeansplot, file = here("results", "totbiomass_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbiomass.onfarm.trtdiff <- pairs(totbiomass.onfarm.emmeans.late, reverse = T)
totbiomass.onfarm.trtdiff %>% as_tibble()






# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totbiomass_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Total, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totbiomass_research_rawplot
# ggsave(totbiomass_research_rawplot, file = here("results", "totbiomass_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (gamma)
totbiomass.research.glmer <- glmer(avgBiomass_Total ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
                                   data = combined.data %>% filter(FieldType == "Research Station"),
                                   family = "Gamma")




lmer(log(avgBiomass_Total+1) ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
     data = combined.data %>% filter(FieldType == "Research Station")




# LOOK AT GLMM OUTPUT
summary(totbiomass.research.glmer)
anova(totbiomass.research.glmer)
plot(totbiomass.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbiomass.research.emmeans <- emmeans(totbiomass.research.glmer, ~ Treatment | Site:Season, type = "response")
totbiomass.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totbiomass_research_emmeansplot <- totbiomass.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season, scales = "free") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totbiomass_research_emmeansplot
# ggsave(totbiomass_research_emmeansplot, file = here("results", "totbiomass_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbiomass.research.trtdiff <- pairs(totbiomass.research.emmeans, reverse = T)
totbiomass.research.trtdiff %>% as_tibble()
