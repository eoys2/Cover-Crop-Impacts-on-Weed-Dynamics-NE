# Ran 11/12 by Elizabeth Oys

# Broadleaf Biomass

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
broadbio_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Broadleaf, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Broadleaf Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
broadbio_onfarm_rawplot 
# ggsave(broadbio_onfarm_rawplot, file = here("results", "broadbio_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
broadbio.onfarm.lmer <- lmer(avgBiomass_Broadleaf ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                             data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(broadbio.onfarm.lmer)
anova(broadbio.onfarm.lmer)
plot(broadbio.onfarm.lmer)  #for broadleaf biomass, this looks a little weird.

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
broadbio.onfarm.emmeans <- emmeans(broadbio.onfarm.lmer, ~ Treatment | Site)
broadbio.onfarm.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
broadbio_onfarm_emmeansplot <- broadbio.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Broadleaf Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
broadbio_onfarm_emmeansplot
# ggsave(broadbio_onfarm_emmeansplot, file = here("results", "broadbio_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
broadbio.onfarm.trtdiff <- pairs(broadbio.onfarm.emmeans, reverse = T)
broadbio.onfarm.trtdiff %>% as_tibble()



# ------------------------------------------------------------------------------
# ON FARM - Late #----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
broadbio_onfarm_rawplot <- combined.data %>%
  filter(Season == "Late", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Broadleaf, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Broadleaf Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "green4"))
broadbio_onfarm_rawplot 
# ggsave(broadbio_onfarm_rawplot, file = here("results", "broadbio_onfarm_rawplot.png"), width = 6, height = 6)

# FIT LMM FOR ON FARM 
broadbio.onfarm.lmer.late <- lmer(avgBiomass_Broadleaf ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                  data = combined.data %>% filter(Season == "Late", FieldType == "On Farm"))

# LOOK AT LMM OUTPUT
summary(broadbio.onfarm.lmer.late)
anova(broadbio.onfarm.lmer.late)
plot(broadbio.onfarm.lmer.late) #looks like severe grouping for broad late on farm here...

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
broadbio.onfarm.emmeans.late <- emmeans(broadbio.onfarm.lmer.late, ~ Treatment | Site)
broadbio.onfarm.emmeans.late %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
broadbio_onfarm_emmeansplot <- broadbio.onfarm.emmeans.late %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Broadleaf Biomass") +
  scale_fill_manual(values = c("darkgray", "green4"))
broadbio_onfarm_emmeansplot
# ggsave(broadbio_onfarm_emmeansplot, file = here("results", "broadbio_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
broadbio.onfarm.trtdiff <- pairs(broadbio.onfarm.emmeans.late, reverse = T)
broadbio.onfarm.trtdiff %>% as_tibble()






# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
broadbio_research_rawplot <- combined.data %>%
  filter(FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = avgBiomass_Broadleaf, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_grid(Site~Season, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Broadleaf Biomass \n (Averaged over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
broadbio_research_rawplot
# ggsave(broadbio_research_rawplot, file = here("results", "broadbio_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM (gamma)
broadbio.research.glmer <- glmer.nb(avgBiomass_Broadleaf ~ Treatment*Site*Season + (1 + Treatment| Rep:Site),
                                    data = combined.data %>% filter(FieldType == "Research Station"))


# LOOK AT GLMM OUTPUT
summary(broadbio.research.glmer)
anova(broadbio.research.glmer)
plot(broadbio.research.glmer) #appears to be grouping here too, but not as severe as above. 

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
broadbio.research.emmeans <- emmeans(broadbio.research.glmer, ~ Treatment | Site:Season, type = "response")
broadbio.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
broadbio_research_emmeansplot <- broadbio.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3) +
  facet_grid(Site~Season) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Broadleaf Biomass") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
broadbio_research_emmeansplot
# ggsave(broadbio_research_emmeansplot, file = here("results", "broadbio_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
broadbio.research.trtdiff <- pairs(broadbio.research.emmeans, reverse = T)
broadbio.research.trtdiff %>% as_tibble()
