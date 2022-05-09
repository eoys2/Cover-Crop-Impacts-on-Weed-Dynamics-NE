#Linear Regressions (for FUN only and to observe total trends across all sites and treatments, this is not statistically accurate but we wanted to see what happens across all sites!)

# Libraries ---------------------------------------------------------------

library(readr)     
library(tidyverse) 
library(here)
library(ggplot2)
library(gridExtra)
library(patchwork)

# Data  ------------------------------------------------------
combined.data2 <- read.csv(file = here("data", "multi_combined.csv")) %>% 
  #filter(Site != "Colfax") %>% 
  filter(Treatment != "VetchDNI")
summary(combined.data2)

combined.data2$Site <- factor(combined.data2$Site, levels = c("SCAL","ENREC","Greeley","Howard","Merrick"))
combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Hairy Vetch","Cereal Rye","Multi Species"))



# For thesis: CC biomass vs early weed bio --------------------------------
combined.data2 %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,avgBiomass_Total,avgBiomass_Pigweed,avgBiomass_Grass)) %>% 
  filter(Site != "Colfax") %>% 
  filter(Season == "Early") %>% 
  filter(Treatment %in% c("Multi Species","Hairy Vetch","Cereal Rye")) %>% 
  mutate(CCbio = (log(avgCoverCrop_Biomass))) %>% 
  mutate(logBio = (log(avgBiomass_Total+1)))-> EarlyWeedBio

TotalWeedBio <- lm(logBio~CCbio+Site+Treatment, data = EarlyWeedBio)
#TotalWeedBio <- lm(logBio~CCbio, data = EarlyWeedBio)

print(TotalWeedBio)
print(summary(TotalWeedBio))
anova(TotalWeedBio)

#R2 = 0.7808 with site and treatment factors
# Site and treatment had significant effects


ggplot(EarlyWeedBio, aes(x = CCbio, y = logBio))+
  geom_point(aes(color = Treatment, shape = Site),size =6)+
  #scale_shape_manual(values=c(0, 1, 2,3,4), color = Treatment)+
  scale_color_manual(values = c("Multi Species" = "#9d0208",
                                "Cereal Rye"="#B46504",
                                "Hairy Vetch"="#708D81"))+
  geom_smooth(method = "lm", col = "black")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title = "Early")+
  xlab(bquote('Log Cover Crop Biomass grams DM m'^-2))+
  ylab(bquote('Log Weed Biomass grams DM m'^-2))+
  scale_y_continuous(limits = c(-0.5,1), breaks = seq(-0.5,1, by = 0.5))+
  #xlab("Log Cover Crop Biomass (g m-2)")+
  #ylab("Log Weed Biomass (g m-2)")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 15))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())-> RegEarlyBio
  #theme(legend.position = "none")+
  #annotate("text", x = 6, y = 0.9, label = bquote("R^2 == 0.78"), size = 5, parse = TRUE) 
RegEarlyBio



# For thesis: cc bio vs late total weed biomass ---------------------------

combined.data2 %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,avgBiomass_Total,avgBiomass_Pigweed,avgBiomass_Grass)) %>% 
  filter(Site != "Colfax") %>% 
  filter(Season == "Late") %>% 
  filter(Treatment %in% c("Multi Species","Hairy Vetch","Cereal Rye")) %>% 
  mutate(CCbio = (log(avgCoverCrop_Biomass))) %>% 
  mutate(logBio = (log(avgBiomass_Total+1)))-> LateWeedBio

#WeedBio2 <- lm(logBio~CCbio, data = LateWeedBio)

WeedBio2 <- lm(logBio~CCbio+Site+Treatment, data = LateWeedBio)
#WeedBio2 <- lm(logBio~CCbio, data = LateWeedBio)

print(WeedBio2)
print(summary(WeedBio2))
anova(WeedBio2)
#R2 = 0.484
# Only site had a signficant effect late season

ggplot(LateWeedBio, aes(x = CCbio, y = logBio))+
  geom_point(aes(color = Treatment, shape = Site),size =6)+
  #scale_shape_manual(values=c(0, 1, 2,3,4), color = Treatment)+
  scale_color_manual(values = c("Multi Species" = "#9d0208",
                                "Cereal Rye"="#B46504",
                                "Hairy Vetch"="#708D81"))+
  geom_smooth(method = "lm", col = "black")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title = "Late")+
  xlab(bquote('Log Cover Crop Biomass grams DM m'^-2))+
  ylab(bquote('Log Weed Biomass grams DM m'^-2))+
  scale_y_continuous(limits=c(-0.5, 2.75),breaks = seq(-0.5, 2.75, by = 1))+
  #xlab("og Cover Crop Biomass (g m-2)")+
  #ylab("Log Weed Biomass (g m-2)")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 15))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_blank())-> RegLateBio
  #theme(plot.title = element_blank())+
  
  #annotate("text", x = 5.1, y = 2.7, label = bquote("R^2 == 0.48"), size = 5, parse = TRUE) 
RegLateBio



library(patchwork)

RegEarlyBio+RegLateBio+
  plot_layout(ncol = 1)+
  plot_annotation(title = "Cover Crop Biomass vs Weed Biomass", theme = theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.2))) -> figureBio
figureBio

LinBio <- patchwork::patchworkGrob(figureBio)
#library(gridExtra)
gridExtra::grid.arrange(LinBio, left = textGrob(expression(paste("Log Weed Biomass grams DM m"^"-2")),gp=gpar(fontsize=15), rot = 90)) -> lrbio
lrbio

ggsave(lrbio, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\UPDATEDLinRegBio.png", dpi = 750, width = 9, height = 7)


# For thesis: CC bio vs early weed density--------------------------------------------------

summary(combined.data2)
combined.data2 %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,sumWeed_Density)) %>% 
  filter(Site != "Colfax") %>% 
  filter(Season == "Early") %>% 
  filter(Treatment %in% c("Multi Species","Hairy Vetch","Cereal Rye")) %>% 
  mutate(logDensity = (log(sumWeed_Density+1))) %>% 
  mutate(CCbio = (log(avgCoverCrop_Biomass)))-> DensityEarly


DensityEarly

dens <- lm(logDensity~CCbio+Site+Treatment, data = DensityEarly)

dens <- lm(logDensity~CCbio, data = DensityEarly)


print(dens)
print(summary(dens))
anova(dens)
#R2 = 0.91?? this seems really high 
#Site was the only significant effect - not treatment here


ggplot(DensityEarly, aes(x = CCbio, y = logDensity))+
  geom_point(aes(color = Treatment, shape = Site),size =6)+
  #scale_shape_manual(values=c(0, 1, 2,3,4), color = Treatment)+
  scale_color_manual(values = c("Multi Species" = "#9d0208",
                                "Cereal Rye"="#B46504",
                                "Hairy Vetch"="#708D81"))+
  geom_smooth(method = "lm", col = "black")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title = "Early")+
  xlab(bquote('Log Cover Crop Biomass grams DM m'^-2))+
  ylab(bquote('Log Weed Density weeds m'^-2))+
  #scale_y_continuous(limits = c(-0.5,1), breaks = seq(-0.5,1, by = 0.5))+
  #xlab("Log Cover Crop Biomass (g m-2)")+
  #ylab("Log Weed Biomass (g m-2)")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 15))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())-> EarlyDensityPlot
  #theme(legend.position = "none")+
  #annotate("text", x = 6, y = 0.9, label = bquote("R^2 == 0.78"), size = 5, parse = TRUE) 
EarlyDensityPlot



# For thesis: CC bio vs late weed density -------------------------------------------------------------

summary(combined.data2)
combined.data2 %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,sumWeed_Density)) %>% 
  filter(Site != "Colfax") %>% 
  filter(Season == "Late") %>% 
  filter(Treatment %in% c("Multi Species","Hairy Vetch","Cereal Rye")) %>% 
  mutate(logDensity = (log(sumWeed_Density+1))) %>% 
  mutate(CCbio = (log(avgCoverCrop_Biomass)))-> DensityLate


DensityLate

dens2 <- lm(logDensity~CCbio+Site+Treatment, data = DensityLate)

#dens <- lm(logDensity~CCbio, data = DensityEarly)


print(dens2)
print(summary(dens2))
anova(dens2)
#R2 = 0.696
#Site was the only significant effect - not treatment here

ggplot(DensityLate, aes(x = CCbio, y = logDensity))+
  geom_point(aes(color = Treatment, shape = Site),size =6)+
  #scale_shape_manual(values=c(0, 1, 2,3,4), color = Treatment)+
  scale_color_manual(values = c("Multi Species" = "#9d0208",
                                "Cereal Rye"="#B46504",
                                "Hairy Vetch"="#708D81"))+
  stat_smooth(method = "lm", col = "black")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title = "Late")+
  xlab(bquote('Log Cover Crop Biomass grams DM m'^-2))+
  ylab(bquote('Log Weed Biomass grams DM m'^-2))+
  #scale_y_continuous(limits=c(-0.5, 2.75),breaks = seq(-0.5, 2.75, by = 1))+
  #xlab("og Cover Crop Biomass (g m-2)")+
  #ylab("Log Weed Biomass (g m-2)")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 15))+
  theme(axis.title.y = element_blank())-> RegLateDens
  #theme(plot.title = element_blank())+
  #theme(legend.position = "none")+
  #annotate("text", x = 5.1, y = 2.7, label = bquote("R^2 == 0.48"), size = 5, parse = TRUE) 
RegLateDens




EarlyDensityPlot+RegLateDens+
  plot_layout(ncol = 1)+
  plot_annotation(title = "Cover Crop Biomass vs Weed Density", theme = theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.2))) -> figureDens
figureDens

LinDens <- patchwork::patchworkGrob(figureDens)
#library(gridExtra)
gridExtra::grid.arrange(LinBio, left = textGrob(expression(paste("Log Weed Biomass grams DM m"^"-2")),gp=gpar(fontsize=15), rot = 90)) -> lrdens
lrdens

ggsave(lrdens, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\UPDATEDLinRegDens.png", dpi = 750, width = 9, height = 7)




#Ignore these other ones
# temperature vs weed density ---------------------------------------------

summary(combined.data)
combined.data %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,sumWeed_Density,sumPigweed_Density,avgTemperature)) %>% 
  filter(Site != "Colfax") %>% 
  #filter(Season == "Late") %>% 
  filter(Treatment %in% c("Cover","Vetch","Rye")) %>% 
  mutate(logDensity = (log(sumWeed_Density+1)))-> TempDens

temp <- lm(logDensity~avgTemperature, data = TempDens)

print(summary(dens))

ggplot(TempDens, aes(x = avgTemperature, y = logDensity))+
  geom_point()+
  stat_smooth(method = "lm", col = "red")




# temperature vs weed bio -------------------------------------------------

combined.data %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,avgBiomass_Total,avgBiomass_Pigweed,avgMoisture)) %>% 
  filter(Site != "Colfax") %>% 
  filter(Season == "Early") %>% 
  filter(Treatment %in% c("Cover","Vetch","Rye")) %>% 
  mutate(logBio = (log(avgBiomass_Total+1))) %>% 
  filter(logBio > 0) -> WeedBio

TotalWeedBio <- lm(logBio~avgMoisture, data = WeedBio)

print(TotalWeedBio)
print(summary(TotalWeedBio))

#R2 = 0.1466, p value = 0.01019

ggplot(WeedBio, aes(x = avgMoisture, y = logBio))+
  geom_point()+
  stat_smooth(method = "lm", col = "red")
#no correlation. stronger in early season tho


# CROP WATCH: Relationship between log early total weed biomass and log CC biomass --------------------
#new data frame
combined.data2 %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,avgBiomass_Total,avgBiomass_Pigweed,avgBiomass_Grass)) %>% 
  filter(Site != "Colfax") %>% 
  filter(Season == "Early") %>% 
  filter(Treatment %in% c("Multi Species","Hairy Vetch","Cereal Rye")) %>% 
  mutate(CCbio = (log(avgCoverCrop_Biomass))*8.92) %>% 
  mutate(logBio = (log(avgBiomass_Total+1))*8.92)-> EarlyWeedBio

TotalWeedBio <- lm(logBio~CCbio+Site+Treatment, data = EarlyWeedBio)

print(TotalWeedBio)
print(summary(TotalWeedBio))
anova(TotalWeedBio)

#R2 = 0.6246

EarlyWeedBio$Site <- factor(EarlyWeedBio$Site, levels = c("SCAL","ENREC","Greeley","Howard","Merrick"))



#with units of lb acre
ggplot(EarlyWeedBio, aes(x = CCbio, y = logBio))+
  geom_point(aes(color = Treatment, shape = Site),size =6)+
  #scale_shape_manual(values=c(0, 1, 2,3,4), color = Treatment)+
  scale_color_manual(values = c("Multi Species" = "#9d0208",
                                "Cereal Rye"="#B46504",
                                "Hairy Vetch"="#708D81"))+
  stat_smooth(method = "lm", col = "black")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title = "Cover Crop Biomass vs Early Total Weed Biomass")+
  xlab(bquote('Log Cover Crop Biomass lb acre'^-1))+
  ylab(bquote('Log Weed Biomass lb acre'^-1))+
  #xlab("Log Cover Crop Biomass (g m-2)")+
  #ylab("Log Weed Biomass (g m-2)")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 15))+
  #theme(legend.position = "none")+
  annotate("text", x = 52.5, y = 7, label = bquote("R^2 == 0.62"), size = 5, parse = TRUE) -> RegEarlyBio
RegEarlyBio


#Instructions to make pretty: https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/



# CROP WATCH: CC bio vs late weed bio -------------------------------------------------
combined.data2 %>% 
  select(c(Site,Treatment,Rep,Season,avgCoverCrop_Biomass,avgBiomass_Total,avgBiomass_Pigweed,avgBiomass_Grass)) %>% 
  filter(Site != "Colfax") %>% 
  filter(Season == "Late") %>% 
  filter(Treatment %in% c("Multi Species","Vetch","Rye")) %>% 
  mutate(CCbio = (log(avgCoverCrop_Biomass))*8.92) %>% 
  mutate(logBio = (log(avgBiomass_Total+1))*8.92)-> LateWeedBio

WeedBio2 <- lm(logBio~CCbio, data = LateWeedBio)

print(WeedBio2)
print(summary(WeedBio2))

#R2 = 0.1333

LateWeedBio$Site <- factor(LateWeedBio$Site, levels = c("SCAL","ENREC","Greeley","Howard"))


ggplot(LateWeedBio, aes(x = CCbio, y = logBio))+
  geom_point(aes(color = Treatment, shape = Site), size = 6)+
  scale_color_manual(values = c("Multi Species" = "#9d0208",
                                "Rye"="#B46504",
                                "Vetch"="#708D81"))+
  stat_smooth(method = "lm", col = "black")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title = "Cover Crop Biomass vs Late Total Weed Biomass")+
  xlab(bquote('Log Cover Crop Biomass lb acre'^-1))+
  ylab(bquote('Log Weed Biomass lb acre'^-1))+
  #xlab("Log Cover Crop Biomass (g m-2)")+
  #ylab("Log Weed Biomass (g m-2)")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 15))+
  theme(axis.title.y = element_blank())+
  theme(legend.position = "none")+
  annotate("text", x = 45, y =22, label = bquote("R^2 == 0.13"), size = 5, parse = TRUE) -> RegLateBio
RegLateBio



library(patchwork)

RegEarlyBio+RegLateBio -> figureBio
figureBio








