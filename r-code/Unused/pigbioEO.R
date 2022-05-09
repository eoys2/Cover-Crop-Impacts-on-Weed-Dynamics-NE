# Ran 11/8 by Elizabeth Oys



# Pigweed Biomass


# Averaged over the 2 samples
# Note: only 1 measure so we filter for plots and models on Season == "Early"
# Right skewed yeild ~ Gamma (kind of)

# Questions:
# How is the total biomass less than the cover crop biomass? Should this not be everything summed together or is this the weed biomass only? (see raw plots)
# Again, sometimes there is 0 biomass? So nothing was collected? (see raw plots)
# ENREC has one observation in the check with a biomass quite a bit larger than all the others?
# Nonconstant variance issues in the LMM -> use gamma? what about the 0's then?
# Gamma fits well for the research stations! :)

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
  filter(Site != "Colfax") %>% 
  filter(Treatment != "VetchDNI")
summary(combined.data)

# ------------------------------------------------------------------------------
# ON FARM - Early #----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
pigbio_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Pigweed, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Pigweed Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
pigbio_onfarm_rawplot 
# ggsave(pigbio_onfarm_rawplot, file = here("results", "pigbio_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
#pigbio.onfarm.lmer <- lmer(avgBiomass_Pigweed ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                               data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))

#pigbio.onfarm.lmer <- lmer(log(avgBiomass_Pigweed+1) ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                           data = combined.data %>% filter(Season == "Early", FieldType == "On Farm", Site != "Colfax"))

pigbio.onfarm.lmer <- lmer(log(avgBiomass_Pigweed + ) ~ Treatment*Site*Season + (1 + Treatment | Rep:Site),
                               data = combined.data %>% filter(FieldType == "On Farm"))


# LOOK AT LMM OUTPUT
summary(pigbio.onfarm.lmer)
anova(pigbio.onfarm.lmer)
plot(pigbio.onfarm.lmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
pigbio.onfarm.emmeans <- emmeans(pigbio.onfarm.lmer, ~ Treatment | Site:Season, type = "response")
pigbio.onfarm.emmeans %>% as_tibble()



# PLOT ESTIMATED TREATMENT MEANS
pigbio_onfarm_emmeansplot <- pigbio.onfarm.emmeans %>% 
  as_tibble() %>%
  mutate(lower.CL = ifelse(lower.CL < 0, 0, lower.CL)) %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
pigbio_onfarm_emmeansplot
# ggsave(pigbio_onfarm_emmeansplot, file = here("results", "pigbio_onfarm_emmeansplot.png"), width = 6, height = 6)


# COMPARE TREATMENTS WITHIN EACH SITE
pigbio.onfarm.trtdiff <- pairs(pigbio.onfarm.emmeans, reverse = T)
pigbio.onfarm.trtdiff %>% as_tibble()



# ------------------------------------------------------------------------------
# ON FARM - Late #----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
pigbio_onfarm_rawplot <- combined.data %>%
  filter(Season == "Late", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Pigweed, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
pigbio_onfarm_rawplot 
# ggsave(pigbio_onfarm_rawplot, file = here("results", "pigbio_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
pigbio.onfarm.lmer.late <- lmer(avgBiomass_Pigweed ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data %>% filter(Season == "Late", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(pigbio.onfarm.lmer.late)
anova(pigbio.onfarm.lmer.late)
plot(pigbio.onfarm.lmer.late)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
pigbio.onfarm.emmeans.late <- emmeans(pigbio.onfarm.lmer.late, ~ Treatment | Site)
pigbio.onfarm.emmeans.late %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
pigbio_onfarm_emmeansplot <- pigbio.onfarm.emmeans.late %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
pigbio_onfarm_emmeansplot
# ggsave(pigbio_onfarm_emmeansplot, file = here("results", "pigbio_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
pigbio.onfarm.trtdiff <- pairs(pigbio.onfarm.emmeans.late, reverse = T)
pigbio.onfarm.trtdiff %>% as_tibble()






# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
pigbio_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Pigweed, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
pigbio_research_rawplot
# ggsave(pigbio_research_rawplot, file = here("results", "pigbio_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (gamma)
pigbio.research.glmer <- glmer.nb(avgBiomass_Pigweed ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
                                   data = combined.data %>% filter(FieldType == "Research Station"))


# LOOK AT GLMM OUTPUT
summary(pigbio.research.glmer)
anova(pigbio.research.glmer)
plot(pigbio.research.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
pigbio.research.emmeans <- emmeans(pigbio.research.glmer, ~ Treatment | Site:Season, type = "response")
pigbio.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
pigbio_research_emmeansplot <- pigbio.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
pigbio_research_emmeansplot
# ggsave(pigbio_research_emmeansplot, file = here("results", "pigbio_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
pigbio.research.trtdiff <- pairs(pigbio.research.emmeans, reverse = T)
pigbio.research.trtdiff %>% as_tibble()
