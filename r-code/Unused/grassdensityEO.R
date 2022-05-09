#Ran 11/8 by Elizabeth Oys
#Grass Densities - Field Data


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
fieldgrass_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = sumGrass_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldgrass_onfarm_rawplot 
# ggsave(fieldgrass_onfarm_rawplot, file = here("results", "fieldgrass_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
fieldgrass.onfarm.lmer <- lmer(sumGrass_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                               data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(fieldgrass.onfarm.lmer)
anova(fieldgrass.onfarm.lmer)
plot(fieldgrass.onfarm.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldgrass.onfarm.emmeans <- emmeans(fieldgrass.onfarm.lmer, ~ Treatment | Site)
fieldgrass.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldgrass_onfarm_emmeansplot <- fieldgrass.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldgrass_onfarm_emmeansplot
# ggsave(fieldgrass_onfarm_emmeansplot, file = here("results", "fieldgrass_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldgrass.onfarm.trtdiff <- pairs(fieldgrass.onfarm.emmeans, reverse = T)
fieldgrass.onfarm.trtdiff %>% as_tibble()




# ------------------------------------------------------------------------------
# ON FARM - late ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
fieldgrass_onfarm_rawplot <- combined.data %>%
  filter(Season == "Late", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = sumGrass_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldgrass_onfarm_rawplot 
# ggsave(fieldgrass_onfarm_rawplot, file = here("results", "fieldgrass_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
fieldgrass.onfarm.lmer.late <- lmer(sumGrass_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                               data = combined.data %>% filter(Season == "Late", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(fieldgrass.onfarm.lmer.late)
anova(fieldgrass.onfarm.lmer.late)
plot(fieldgrass.onfarm.lmer.late)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldgrass.onfarm.emmeans.late <- emmeans(fieldgrass.onfarm.lmer.late, ~ Treatment | Site)
fieldgrass.onfarm.emmeans.late %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldgrass_onfarm_emmeansplot.late <- fieldgrass.onfarm.emmeans.late %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Grass Density") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldgrass_onfarm_emmeansplot.late
# ggsave(fieldgrass_onfarm_emmeansplot, file = here("results", "fieldgrass_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldgrass.onfarm.trtdiff.late <- pairs(fieldgrass.onfarm.emmeans.late, reverse = T)
fieldgrass.onfarm.trtdiff.late %>% as_tibble()



# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
fieldgrass_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = sumGrass_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
fieldgrass_research_rawplot
# ggsave(fieldgrass_research_rawplot, file = here("results", "fieldgrass_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (gamma)
#fieldgrass.research.glmer <- glmer(sumGrass_Density ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
#data = combined.data %>% filter(FieldType == "Research Station"),
#family = "Gamma")

#fieldgrass.research.lmer <- lmer(sumGrass_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                 #data = combined.data %>% filter(FieldType == "Research Station"))


fieldgrass.research.glmer <- glmer.nb(sumGrass_Density ~ Treatment*Site*Season + (1 +Treatment | Rep:Site),
                                       data = combined.data %>% filter(FieldType == "Research Station"))



# LOOK AT GLMM OUTPUT
summary(fieldgrass.research.glmer)
anova(fieldgrass.research.glmer)
plot(fieldgrass.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldgrass.research.emmeans <- emmeans(fieldgrass.research.glmer, ~ Treatment | Site:Season, type = "response")
fieldgrass.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldgrass_research_emmeansplot <- fieldgrass.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
fieldgrass_research_emmeansplot
# ggsave(fieldgrass_research_emmeansplot, file = here("results", "fieldgrass_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldgrass.research.trtdiff <- pairs(fieldgrass.research.emmeans, reverse = T)
fieldgrass.research.trtdiff %>% as_tibble()
