# WEED DENSITY 
# log transformed

# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------

library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner
library(lme4)      # fits our models (LMM/GLMM)
library(lmerTest)  # helps provide ANOVA table from our LMM
library(emmeans)   # helps output our treatment means from our LMM
library(ggsignif)
library(corrplot)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(ggplot2)
library(devtools)
library(coda)
library(agricolae)
library(lme4)
library(lmerTest)
library(emmeans)
library(nlme)
library(car)
library(Hmisc)
library(ggfortify)
library(readxl)
library(candisc)
library(readxl)
library(grid)
library(ggplotify)
library(lsmeans)

# ------------------------------------------------------------------------------
# IMPORT COMBINED DATA ---------------------------------------------------------
# ------------------------------------------------------------------------------
combined.data2 <- read.csv(file = here("data", "multi_combined.csv")) %>% 
  #filter(Site != "Colfax") %>% 
  filter(Treatment != "VetchDNI")
summary(combined.data2)

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))
combined.data2$Site <- factor(combined.data2$Site, levels = c("Colfax","Greeley","Howard","Merrick","ENREC","SCAL"))
combined.data2$Season <- factor(combined.data2$Season, levels = c("Early","Late"))


weeddensity.onfarm.emmeans.log %>% 
  as_data_frame() %>% 
  select(c(Treatment,Site,Season,response,SE)) %>%
  group_by(Site, Treatment, Season) %>% 
  pivot_longer(response:SE, names_to = "functional_group", values_to = "biomass") -> biocombined
           




pigdensity.onfarm.emmeans.log %>% 
  as_data_frame() %>% 
  select(c(Treatment,Site,Season,response,SE)) %>% 
  mutate(FunctionalGroup = "Pigweed")-> pigdens
  

grassdensity.onfarm.emmeans.log %>% 
  as_tibble() %>% 
  select(c(Treatment,Site,Season,response,SE)) %>% 
mutate(FunctionalGroup = "Grass")-> grassdens


broaddensity.onfarm.emmeans.log %>% 
  as_tibble() %>% 
  select(c(Treatment,Site,Season,response,SE)) %>% 
mutate(FunctionalGroup = "Broadleaf") -> broaddens

rbind(pigdens, grassdens, broaddens) %>% 
  #na.omit() %>% 
  filter(Site != "Colfax")-> df3

df3$FunctionalGroup <- factor(df3$FunctionalGroup, levels = c("Pigweed","Grass","Broadleaf"))

ggplot(data = df3, aes(y = response, x = Treatment, fill = FunctionalGroup))+
  geom_bar(stat = "identity")+
  #geom_errorbar(aes(ymin = response-SE, ymax = response+SE), position = "identity", width = 0.1)+
  facet_wrap(Site~Season, ncol = 2)+
  theme_bw()+
  scale_fill_manual(values = c("#156064","#ffba08", "#9d0208"))

"#156064","#ffba08", "#9d0208" 
  



#plotting graph RSH bxp1<-
ggplot(data=infiltration_data,aes(y=RSH,x=Soil_management, fill = Soil_property)) +
  geom_bar(stat="identity")+ 
  geom_text(aes(label = letters, y=Y_pos-0.3),position = "identity", size=5)+
  geom_hline(yintercept = 7, color="black", linetype = "dashed")+
  geom_signif(comparisons=list(c("cover cropping", "no cover cropping")), annotations="", y_position = 6.0, tip_length = 0.02, vjust=0.4) +
  geom_errorbar(aes(ymin=Y_pos-SEM, ymax=Y_pos+SEM), position = "identity",width=.08,color="darkgray")+
  facet_wrap(~County) +
  scale_fill_brewer(palette="Dark2", name="Soil property") + theme_minimal()+
  ylab("Relative soil health (RSH)") + 
  xlab("Location") +
  theme(legend.position="bottom")




