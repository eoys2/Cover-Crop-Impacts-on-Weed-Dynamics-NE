#Ran 11/3 by Elizabeth Oys
# Broadleaf 
# 

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
combined.data <- read.csv(file = here("data", "updated-combined-dataEO.csv"))%>% 
  filter(Treatment != "VetchDNI")
summary(combined.data)

# ------------------------------------------------------------------------------
# ON FARM ----------------------------------------------------------------------
# ------------------------------------------------------------------------------


#totSeeds_Broad
# PLOT RAW DATA
totbroad_onfarm_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "On Farm") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Otherbroad, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Broadleaves") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbroad_onfarm_rawplot 
# ggsave(totspecies_onfarm_rawplot, file = here("results", "totspecies_onfarm_rawplot.png"), width = 6, height = 6)



# FIT GLMM FOR ON FARM 
totbroad.onfarm.glmer <- glmer.nb(totSeeds_Otherbroad ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                  data = combined.data %>% filter(Season == "Early", FieldType == "On Farm"))


# totbroad.onfarm.glmer2 <- glmer.nb(totSeeds_Otherbroad ~ Treatment + Site + (1 | Rep),
#                                    data = combined.data %>% filter(Season == "Early", FieldType == "On Farm")) #removing site as a factor
# summary(totbroad.onfarm.glmer2) #edited to remove site
# car::Anova(totbroad.onfarm.glmer2)
# plot(totbroad.onfarm.glmer2)





# LOOK AT LMM OUTPUT
summary(totbroad.onfarm.glmer) # no sig results
car::Anova(totbroad.onfarm.glmer) #treatment was 0.08
plot(totbroad.onfarm.glmer)

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbroad.onfarm.emmeans <- emmeans(totbroad.onfarm.glmer, ~ Treatment | Site, type = "response")
totbroad.onfarm.emmeans %>% as_tibble()



# PLOT ESTIMATED TREATMENT MEANS
totbroad_onfarm_emmeansplot <- totbroad.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = asymp.UCL*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Broadleaves") +
  scale_fill_manual(values = c("darkgray", "green4"))
totbroad_onfarm_emmeansplot   ### Seeing differences in merrick county, more broadleaves in check
# ggsave(totpigs_onfarm_emmeansplot, file = here("results", "totspecies_onfarm_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbroad.onfarm.trtdiff <- pairs(totbroad.onfarm.emmeans, reverse = T)
totbroad.onfarm.trtdiff %>% as_tibble()  

# ------------------------------------------------------------------------------
# RESEARCH STATION -------------------------------------------------------------
# ------------------------------------------------------------------------------

# PLOT RAW DATA
totbroad_research_rawplot <- combined.data %>%
  filter(Season == "Early", FieldType == "Research Station") %>%
  ggplot(aes(x = Treatment, y = totSeeds_Otherbroad, fill = Treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             alpha = 0.7, show.legend = F, shape = 21, color = "black", size = 2) +
  facet_wrap(~Site, scales = "free_x") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Broadleaves \n (Summed over samples)") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totbroad_research_rawplot
# ggsave(totspecies_research_rawplot, file = here("results", "totspecies_research_rawplot.png"), width = 6, height = 6)

# FIT GLMM FOR RESEARCH STATION
totbroad.research.glmer <- glmer.nb(totSeeds_Otherbroad ~ Treatment + Site + Treatment:Site + (1 | Rep:Site),
                                    data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"))

# LOOK AT LMM OUTPUT
summary(totbroad.research.glmer)
anova(totbroad.research.glmer)
plot(totbroad.research.glmer) #not significant

# GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
totbroad.research.emmeans <- emmeans(totbroad.research.glmer, ~ Treatment | Site, type = "response")
totbroad.research.emmeans %>% as_tibble()

# PLOT ESTIMATED TREATMENT MEANS
totbroad_research_emmeansplot <- totbroad.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = asymp.UCL*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.3) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_y_continuous("Total Broadleaves") +
  scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
totbroad_research_emmeansplot
# ggsave(totpigs_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)

# COMPARE TREATMENTS WITHIN EACH SITE
totbroad.research.trtdiff <- pairs(totbroad.research.emmeans, reverse = T)
totbroad.research.trtdiff %>% as_tibble() #no sig results for other broads. 




# ------------------------------------------------------------------------------
# CODE FOR GLMMadaptive --------------------------------------------------------
# ------------------------------------------------------------------------------
# library(GLMMadaptive)

# # FIT GLMM FOR RESEARCH STATION
# totspecies.research.glmmadapt <- mixed_model(fixed = Total_Species ~ Treatment + Site + Treatment:Site,
#                                              random ~ 1 | Rep:Site,
#                                       data = combined.data %>% filter(Season == "Early", FieldType == "Research Station"),
#                                       family = poisson())
# 
# # LOOK AT GLMM OUTPUT
# summary(totspecies.research.glmmadapt)
# anova(totspecies.research.glmmadapt)
# plot(totspecies.research.glmmadapt)
# 
# # GET TREATMENT MEANS OUT OF THE MODEL AND PLOT
# totspecies.research.emmeans <- emmeans(totspecies.research.glmmadapt, ~ Treatment | Site, type = "response")
# totspecies.research.emmeans %>% as_tibble()
# 
# # PLOT ESTIMATED TREATMENT MEANS
# totspecies_research_emmeansplot <- totspecies.research.emmeans %>% 
#   as_tibble() %>%
#   ggplot(aes(x = Treatment, y = exp(emmean), fill = Treatment)) +
#   geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
#   geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)), width = 0.3) +
#   facet_wrap(~Site) +
#   theme_bw() +
#   theme(aspect.ratio = 1) +
#   scale_y_continuous("Total Weed Seeds") +
#   scale_fill_manual(values = c("darkgray", "darkorange", "steelblue"))
# totspecies_research_emmeansplot
# # ggsave(totspecies_research_emmeansplot, file = here("results", "totspecies_research_emmeansplot.png"), width = 6, height = 6)
# 
# # COMPARE TREATMENTS WITHIN EACH SITE
# totspecies.research.trtdiff <- pairs(totspecies.research.emmeans, reverse = T)
# totspecies.research.trtdiff %>% as_tibble()
