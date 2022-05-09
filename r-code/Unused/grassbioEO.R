# Ran 11/8 by Elizabeth Oys

# Grass Biomass

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
# ON FARM - Early #----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
grassbio_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Grass, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Grass Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
grassbio_onfarm_rawplot 
# ggsave(grassbio_onfarm_rawplot, file = here("results", "grassbio_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
grassbio.onfarm.lmer <- lmer(avgBiomass_Grass ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                           data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(grassbio.onfarm.lmer)
anova(grassbio.onfarm.lmer)
plot(grassbio.onfarm.lmer)  #for grass biomass, this looks a little weird.

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
grassbio.onfarm.emmeans <- emmeans(grassbio.onfarm.lmer, ~ Treatment | Site)
grassbio.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
grassbio_onfarm_emmeansplot <- grassbio.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
grassbio_onfarm_emmeansplot
# ggsave(grassbio_onfarm_emmeansplot, file = here("results", "grassbio_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
grassbio.onfarm.trtdiff <- pairs(grassbio.onfarm.emmeans, reverse = T)
grassbio.onfarm.trtdiff %>% as_tibble()



# ------------------------------------------------------------------------------
# ON FARM - Late #----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
grassbio_onfarm_rawplot <- combined.data %>%
  filter(Season == "Late", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Grass, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
grassbio_onfarm_rawplot 
# ggsave(grassbio_onfarm_rawplot, file = here("results", "grassbio_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
grassbio.onfarm.lmer.late <- lmer(avgBiomass_Grass ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                data = combined.data %>% filter(Season == "Late", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(grassbio.onfarm.lmer.late)
anova(grassbio.onfarm.lmer.late)
plot(grassbio.onfarm.lmer.late) #also looks like there is grouping

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
grassbio.onfarm.emmeans.late <- emmeans(grassbio.onfarm.lmer.late, ~ Treatment | Site)
grassbio.onfarm.emmeans.late %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
grassbio_onfarm_emmeansplot <- grassbio.onfarm.emmeans.late %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
grassbio_onfarm_emmeansplot
# ggsave(grassbio_onfarm_emmeansplot, file = here("results", "grassbio_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
grassbio.onfarm.trtdiff <- pairs(grassbio.onfarm.emmeans.late, reverse = T)
grassbio.onfarm.trtdiff %>% as_tibble()






# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
grassbio_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Grass, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
grassbio_research_rawplot
# ggsave(grassbio_research_rawplot, file = here("results", "grassbio_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (gamma)
grassbio.research.glmer <- glmer.nb(avgBiomass_Grass ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
                                  data = combined.data %>% filter(FieldType == "Research Station"))


# LOOK AT GLMM OUTPUT
summary(grassbio.research.glmer)
anova(grassbio.research.glmer)
plot(grassbio.research.glmer) #appears to be grouping here too. 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
grassbio.research.emmeans <- emmeans(grassbio.research.glmer, ~ Treatment | Site:Season, type = "response")
grassbio.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
grassbio_research_emmeansplot <- grassbio.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
grassbio_research_emmeansplot
# ggsave(grassbio_research_emmeansplot, file = here("results", "grassbio_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
grassbio.research.trtdiff <- pairs(grassbio.research.emmeans, reverse = T)
grassbio.research.trtdiff %>% as_tibble()
