#Figure 2: Aboveground pigweed biomass and pigweed density











# GGPLOTTING FIGURES: Pigweed Biomass --------------------------------------------------------------

#On farm
pigbio_onfarm_emmeansplot <- pigbio%>% 
  as_tibble() %>%
  #filter(Site != "Colfax") %>% 
  drop_na() %>% 
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(strip.text.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,11),breaks = seq(0, 10, by = 2),"Total Pigweed Biomass") +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
pigbio_onfarm_emmeansplot

#research station
pigbio_research_emmeansplot <- pigbio.research.emmeans %>% 
  as_tibble() %>%
  drop_na() %>% 
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,11),breaks = seq(0, 10, by = 2), "Total Biomass") +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
pigbio_research_emmeansplot

#put both together
pigbio_research_emmeansplot+pigbio_onfarm_emmeansplot+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = " Pigweed Biomass", theme = theme(plot.title = element_text(size = 15, hjust = 0.4, face = "bold")))-> pbio
pbio

pbiomass <- patchwork::patchworkGrob(pbio)
gridExtra::grid.arrange(pbiomass, left = textGrob(expression(paste("Estimated Pigweed Biomass (g DM ", m^-2, ")")), rot = 90)) -> bioplot





# Pigweed Density ---------------------------------------------------------

# GGPLOTTING FIGURES --------------------------------------------------------------

#on farm sites
pigdensity_onfarm_emmeansplot.log <- pig %>%
  as_tibble() %>%
  mutate(lower.SE = (response-SE)) %>% 
  mutate(lowSE = ifelse(lower.SE < 0, 0, lower.SE)) %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = lowSE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season, drop = TRUE) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(legend.position = "none")+
  theme(axis.ticks.x = element_blank())+
  theme(strip.text.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(limits = c(0,1000),expression(paste("Emerged Pigweed Density weeds m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
pigdensity_onfarm_emmeansplot.log

#research station sites
pigdensity_research_emmeansplot <- pigdensity.research.emmeans %>% 
  as_tibble() %>%
  #filter(Site == "SCAL") %>% 
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE), width = 0.1) +
  facet_grid(Site~Season) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(legend.position = "none")+
  theme(axis.ticks.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  scale_y_continuous(expression(paste("Emerged Pigweed Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
pigdensity_research_emmeansplot


#combine on farm and research stations
pigdensity_research_emmeansplot+pigdensity_onfarm_emmeansplot.log+
  plot_layout(ncol = 1, heights = c(2,3))+
  plot_annotation(title = "Pigweed Density", theme = theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold")))-> dens
dens

density <- patchwork::patchworkGrob(dens)
gridExtra::grid.arrange(density, left = textGrob(expression(paste("Estimated Pigweed  Density (weeds  ", m^-2, ")")), rot = 90)) -> densplot

grid.arrange(densplot, bioplot, ncol=2, widths = c(0.85,1)) -> Figure2
Figure2
ggsave(Figure2, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\WEEDS_CC_PUB\\Figures\\Figure2_noasterick.png", dpi = 1200, width = 12, height = 6)

#save as desired (change to your own pathway)
#Save 3/11/22
# ggsave(densplot, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\totalweeddensity.png", dpi = 600, width = 6, height = 6)

