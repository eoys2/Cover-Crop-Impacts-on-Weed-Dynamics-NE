#Cover Crop Biomass BarCharts
# Libraries ---------------------------------------------------------------

library(readr)     
library(tidyverse) 
library(here)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(dplyr)


# Load Data  ------------------------------------------------------

combined.data2 <- read.csv(file = here("data", "multi_combined.csv")) %>% 
  filter(Treatment != "VetchDNI") #do not use Vetch data from ENREC
summary(combined.data2)

#apply factor levels for sites and treatments
combined.data2$Site <- factor(combined.data2$Site, levels = c("SCAL","ENREC","Greeley","Howard","Merrick","Colfax"))
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Hairy Vetch","Cereal Rye","Multi Species"))



# Cover Crop Biomass Bar Charts -------------------------------------------
combined.data2 %>% 
  select(c(Site,Treatment,avgCoverCrop_Biomass)) %>% 
  filter(Treatment != "Check") %>% #filter out check (since there is no cover crop in these plots)
  group_by(Site) %>% 
  filter(Site != "SCAL") %>% #SCAL has three treatments, so filter out initially
  mutate(meanCover = mean(avgCoverCrop_Biomass))-> noscal

combined.data2 %>% #repeat same process for scal
  select(c(Site,Treatment,avgCoverCrop_Biomass)) %>% 
  filter(Treatment != "Check") %>% 
  group_by(Treatment) %>% 
  filter(Site == "SCAL") %>% 
  mutate(meanCover = mean(avgCoverCrop_Biomass))-> scal

#join the datasets with dplyr
cover <- full_join(noscal, scal)
summary(cover)
cover$Site <- factor(cover$Site, levels = c("SCAL","ENREC","Colfax", "Greeley","Howard","Merrick")) #reapply factor levels
cover
#Units are in grams of dry plant matter per square meter

#Create ggplot barplot
cover %>%
  ggplot(aes(x=Site, y=meanCover))+ 
  geom_bar(stat = "identity", aes(fill = Treatment), position = position_dodge(width = 1))+
  scale_fill_manual(values = c("Multi Species" = "#9d0208",
                               "Cereal Rye"="#B46504",
                               "Hairy Vetch"="#708D81"))+
  labs(title = "Winter Cover Crop Biomass by Location")+
  ylab(bquote('Cover Crop Biomass grams DM m'^-2))+
  theme_minimal()+
  theme(plot.title = element_text(size =25, face = "bold", hjust = 0.5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 20))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(legend.key.size = unit(1.5, 'cm'))+
  theme(legend.text = element_text(size = 15))+
  theme(legend.title=element_text(size = 19))-> coverplot

coverplot

#Save as desired.
#ggsave(coverplot, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\CropWatch Figures\\UpdatedFigure5.pdf", dpi = 950, width = 12, height = 7)

