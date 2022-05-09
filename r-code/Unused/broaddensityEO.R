#Ran 11/8 by Elizabeth Oys
#Broadleaf Densities - Field Data


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
fieldbroad_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = sumBroadleaf_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldbroad_onfarm_rawplot 
# ggsave(fieldbroad_onfarm_rawplot, file = here("results", "fieldbroad_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
fieldbroad.onfarm.lmer <- lmer(sumBroadleaf_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                               data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(fieldbroad.onfarm.lmer)
anova(fieldbroad.onfarm.lmer)
plot(fieldbroad.onfarm.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldbroad.onfarm.emmeans <- emmeans(fieldbroad.onfarm.lmer, ~ Treatment | Site)
fieldbroad.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldbroad_onfarm_emmeansplot <- fieldbroad.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldbroad_onfarm_emmeansplot
# ggsave(fieldbroad_onfarm_emmeansplot, file = here("results", "fieldbroad_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldbroad.onfarm.trtdiff <- pairs(fieldbroad.onfarm.emmeans, reverse = T)
fieldbroad.onfarm.trtdiff %>% as_tibble()




# ------------------------------------------------------------------------------
# ON FARM - late ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
fieldbroad_onfarm_rawplot <- combined.data %>%
  filter(Season == "Late", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = sumBroadleaf_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldbroad_onfarm_rawplot 
# ggsave(fieldbroad_onfarm_rawplot, file = here("results", "fieldbroad_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
fieldbroad.onfarm.lmer.late <- lmer(sumBroadleaf_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data %>% filter(Season == "Late", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(fieldbroad.onfarm.lmer.late)
anova(fieldbroad.onfarm.lmer.late)
plot(fieldbroad.onfarm.lmer.late)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldbroad.onfarm.emmeans.late <- emmeans(fieldbroad.onfarm.lmer.late, ~ Treatment | Site)
fieldbroad.onfarm.emmeans.late %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldbroad_onfarm_emmeansplot.late <- fieldbroad.onfarm.emmeans.late %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Grass Density") +
  scale_fill_manual(values = c("darkgray", "green4"))
fieldbroad_onfarm_emmeansplot.late
# ggsave(fieldbroad_onfarm_emmeansplot, file = here("results", "fieldbroad_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldbroad.onfarm.trtdiff.late <- pairs(fieldbroad.onfarm.emmeans.late, reverse = T)
fieldbroad.onfarm.trtdiff.late %>% as_tibble()



# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
fieldbroad_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = sumBroadleaf_Density, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
fieldbroad_research_rawplot
# ggsave(fieldbroad_research_rawplot, file = here("results", "fieldbroad_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (gamma)
#fieldbroad.research.glmer <- glmer(sumBroadleaf_Density ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
#data = combined.data %>% filter(FieldType == "Research Station"),
#family = "Gamma")

#fieldbroad.research.lmer <- lmer(sumBroadleaf_Density ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
#data = combined.data %>% filter(FieldType == "Research Station"))


fieldbroad.research.glmer <- glmer.nb(sumBroadleaf_Density ~ Treatment*Site*Season + (1 +Treatment | Rep:Site),
                                      data = combined.data %>% filter(FieldType == "Research Station"))



# LOOK AT GLMM OUTPUT
summary(fieldbroad.research.glmer)
anova(fieldbroad.research.glmer)
plot(fieldbroad.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
fieldbroad.research.emmeans <- emmeans(fieldbroad.research.glmer, ~ Treatment | Site:Season, type = "response")
fieldbroad.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
fieldbroad_research_emmeansplot <- fieldbroad.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
fieldbroad_research_emmeansplot
# ggsave(fieldbroad_research_emmeansplot, file = here("results", "fieldbroad_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
fieldbroad.research.trtdiff <- pairs(fieldbroad.research.emmeans, reverse = T)
fieldbroad.research.trtdiff %>% as_tibble()
